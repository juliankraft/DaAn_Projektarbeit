library(tidyverse)
library(scales)

data_path <- '/cfs/earth/scratch/kraftjul/DaAn_Projektarbeit/data/aggregated_data'
out_path <- '/cfs/earth/scratch/kraftjul/DaAn_Projektarbeit/data/results'

# reading data
print('Reading data')

files <- dir(data_path)

data_out <- tibble()

for (file in files) {
  # read csv and append data to list
  print(file)
  data <- read_csv(file.path(data_path, file))
  data_out <- rbind(data_out, data)
}

# processing data
print('Processing data')

data_analysis <- 
   data_out %>%
        mutate(
            # change to date format
            betriebsdatum = dmy(betriebsdatum),
            # extract year from betriebsdatum
            year = year(betriebsdatum),
            # change to hms format
            soll_an_von = case_when(
                soll_an_von < 86400 ~ hms::as_hms(soll_an_von),
                soll_an_von >= 86400 ~ hms::as_hms(soll_an_von - 86400)
            ),
            # stunde extrahieren
            hour = hour(soll_an_von),
            # tramtyp_id erstellen
            tramtyp_id = case_when(
                (fahrzeug >= 1601) & (fahrzeug <= 1690) ~ 1, # Mirage
                (fahrzeug >= 2001) & (fahrzeug <= 2435) ~ 2, # Tram 2000
                (fahrzeug >= 3001) & (fahrzeug <= 3088) ~ 3, # Cobra
                (fahrzeug >= 4001) & (fahrzeug <= 4070) ~ 4, # Flexity
            ),
            verspaetung_cat = 
                case_when(
                    verspaetung < -60 ~ -1, # zu früh
                    verspaetung < 120 ~ 0,  # pünktlich
                    verspaetung < 300 ~ 1,  # leicht verspätet
                    TRUE ~ 2                # stark verspätet 

            ),
            # Ausreisser begrenzen auf max 1h zu früh oder zu spät
            verspaetung = case_when(
                verspaetung > 3600 ~ 3600,
                verspaetung < -3600 ~ -3600,
                TRUE ~ verspaetung
            ),
            # tramtyp_id und hour in factor umwandeln
            tramtyp_id = as.factor(tramtyp_id),
            hour = as.factor(hour),
            verspaetung_cat = as.factor(verspaetung_cat) %>% fct_rev()
        )

# running statistics
print('Running statistics')

alpha <- 0.05
result_bartlett <- bartlett.test(verspaetung ~ year, data = data_analysis)

if(result_bartlett$p.value < alpha) {
    print('Reject H0: Variances are not equal')
    print("Performing Welch's ANOVA")
    model <- oneway.test(verspaetung ~ year, data = data_analysis)
}else{
    print('Accept H0: Variances are equal')
    print("Performing ANOVA")
    model <- aov(verspaetung ~ year, data = data_analysis)
}
model

if(model$p.value < alpha) {
    print('Reject H0: There is a significant difference between the groups')
    print("Post Hoc test")

    with(data_analysis,
         pairwise.t.test(verspaetung, year))
}else{
    print('Accept H0: There is no significant difference between the groups')
}

# summarizing data
print('Summarizing data')

summary <- data_analysis %>%
    group_by(year) %>%
    summarise(
        mean = mean(verspaetung),
        median = median(verspaetung),
        sd = sd(verspaetung),
        n = n()
    ) 

summary %>% write_csv(file.path(out_path, "summary_delay_per_year.csv"))

summary <- data_analysis %>%
    count(year, verspaetung_cat) %>%
    group_by(year) %>%
    mutate(
        perc = round(n / sum(n)*100,2)
    ) %>%
    select(-n) %>%
    pivot_wider(names_from = year, values_from = perc)

summary %>% write_csv(file.path(out_path, "summary_delay_per_year_cat.csv"))


###############################################################################
# plotting delay per category
print('Plotting delay per category')

delay_colors <- c(
  "-1" = "#1f77b4",  # Too Early
  "0" = "#2ca02c",   # On Time
  "1" = "#ff7f0e",   # Late
  "2" = "#d62728"    # Very Late
)

data_analysis %>%
    ggplot(
        aes(
            x = year,
            fill = verspaetung_cat,
        )
    )+

    geom_bar(
        position = "fill"
    )+

    scale_y_continuous(
        labels = scales::percent_format()
    )+

    scale_fill_manual(
        name = "",
        values = delay_colors,
        labels = c("stark verspätet", "verspätet", "pünktlich", "zu früh")
    )+

    labs(
        title = "Verspätung nach Kategorie pro Jahr",
        x = "",
        y = "Anteile"
    )+

    theme_bw()+

    theme(
        aspect.ratio = 0.5
    )

ggsave(file.path(out_path, "delay_per_year.png"), dpi = 1000)


###############################################################################
# Einfluss der Tageszeit auf die Verspätung
print("Analyzing delay per hour")
data_hour <- data_analysis

alpha <- 0.05

# Überprüfung der Varianzhomogenität mittels Bartlett-Test
result_bartlett <- bartlett.test(verspaetung ~ hour, data = data_hour)

if(result_bartlett$p.value < alpha) {
    print('Reject H0: Variances are not equal')
    print("Performing Welch's ANOVA")
    model <- oneway.test(verspaetung ~ hour, data = data_hour)
}else{
    print('Accept H0: Variances are equal')
    print("Performing ANOVA")
    model <- aov(verspaetung ~ hour, data = data_hour)
}
model

if(model$p.value < alpha) {
    print('Reject H0: There is a significant difference between the groups')
    print("Post Hoc test")

    post_hoc <- with(data_hour,
         pairwise.t.test(verspaetung, hour))
    post_hoc
}else{
    print('Accept H0: There is no significant difference between the groups')
}

post_hoc$p.value %>%
    as_data_frame() %>% 
    rownames_to_column(var = "hour") %>% 
    write_csv(file.path(out_path,"verspaetung_hour_post_hoc.csv"))


# Zusammenfassung
data_hour %>% 
group_by(hour) %>% 
summarise(
    mean = mean(verspaetung),
    median = median(verspaetung),
    sd = sd(verspaetung),
    n = n()
    ) %>%
    # adjust path
    write_csv(file.path(out_path, "verspaetung_per_hour_summary.csv"))

# Prozent Anteile der Verspätungskategorien pro Stunde
data_hour %>%
    count(hour, verspaetung_cat) %>%
    group_by(hour) %>%
    mutate(
        perc = round(n / sum(n)*100,2)
    ) %>%
    select(-n) %>%
    pivot_wider(names_from = hour, values_from = perc) %>%
    # adjust path
    write_csv(file.path(out_path, "verspaetung_cat_per_hour_percentage.csv"))

print("Plotting delay per hour")
# Plot - Color Palette
delay_colors <- c(
  "-1" = "#1f77b4",  # Too Early
  "0" = "#2ca02c",   # On Time
  "1" = "#ff7f0e",   # Late
  "2" = "#d62728"    # Very Late
)

# Plot
data_hour %>%
    ggplot(
        aes(
            x = hour,
            fill = verspaetung_cat,
        )
    )+

    geom_bar(
        position = "fill"
    )+

    scale_y_continuous(
        labels = scales::percent_format()
    )+

    scale_fill_manual(
        name = "",
        values = delay_colors,
        labels = c("stark verspätet", "verspätet", "pünktlich", "zu früh")
    )+

    labs(
        title = "Anteil der Verspätungen pro Uhrzeit",
        x = "Uhrzeit",
        y = "Anteile"
    )+

    theme_bw()+

    theme(
        aspect.ratio = 0.5
    )
    # save plot
    ggsave(
        file.path(out_path, "Plot_verspaetung_cat_per_hour_percentage.png"),
        dpi = 1000
    )


###############################################################################
# Einfluss der Tramtypen auf die Verspätung
print("Analyzing delay per tramtype")

data_fahrzeug <- data_analysis %>% drop_na(tramtyp_id)

# Überprüfung der Varianzhomogenität mittels Bartlett-Test
result_bartlett <- bartlett.test(verspaetung ~tramtyp_id, data = data_fahrzeug)

if(result_bartlett$p.value < alpha) {
    print('Reject H0: Variances are not equal')
    print("Performing Welch's ANOVA")
    model <- oneway.test(verspaetung ~tramtyp_id, data = data_fahrzeug)
}else{
    print('Accept H0: Variances are equal')
    print("Performing ANOVA")
    model <- aov(verspaetung ~tramtyp_id, data = data_fahrzeug)
}
model

if(model$p.value < alpha) {
    print('Reject H0: There is a significant difference between the groups')
    print("Post Hoc test")

    post_hoc <- with(data_fahrzeug,
         pairwise.t.test(verspaetung, tramtyp_id))
    post_hoc
}else{
    print('Accept H0: There is no significant difference between the groups')
}

post_hoc$p.value %>%
    as_data_frame() %>% 
    rownames_to_column(var = "tramtyp_id") %>% 
    write_csv(file.path(out_path,"verspaetung_tramtyp_id_post_hoc.csv"))


# Zusammenfassung
data_fahrzeug %>% 
group_by(tramtyp_id) %>% 
summarise(
    mean = mean(verspaetung),
    median = median(verspaetung),
    sd = sd(verspaetung),
    n = n()
    ) %>%
    # adjust path
    write_csv(file.path(out_path, "verspaetung_per_tramtyp_id_summary.csv"))

# Prozent Anteile der Verspätungskategorien pro Stunde
data_fahrzeug %>%
    count(tramtyp_id, verspaetung_cat) %>%
    group_by(tramtyp_id) %>%
    mutate(
        perc = round(n / sum(n)*100,2)
    ) %>%
    select(-n) %>%
    pivot_wider(names_from = tramtyp_id, values_from = perc) %>%
    # adjust path
    write_csv(file.path(out_path, "verspaetung_cat_per_tramtyp_id_percentage.csv"))


print("Plotting delay per tramtype")
# Plot - Color Palette
delay_colors <- c(
  "-1" = "#1f77b4",  # Too Early
  "0" = "#2ca02c",   # On Time
  "1" = "#ff7f0e",   # Late
  "2" = "#d62728"    # Very Late
)

# Plot
data_fahrzeug %>%
    ggplot(
        aes(
            x = tramtyp_id,
            fill = verspaetung_cat,
        )
    )+

    geom_bar(
        position = "fill"
    )+

    scale_y_continuous(
        labels = scales::percent_format()
    )+

    scale_x_discrete(
        labels = c("Mirage", "Tram 2000", "Cobra", "Flexity")
    )+

    scale_fill_manual(
        name = "",
        values = delay_colors,
        labels = c("stark verspätet", "verspätet", "pünktlich", "zu früh")
    )+

    labs(
        title = "Verspätung nach Tramtyp",
        x = "",
        y = "Anteile"
    )+

    theme_bw()+

    theme(
        aspect.ratio = 0.5
    )
    
    # save plot
    ggsave(
        file.path(out_path, "Plot_verspaetung_cat_per_tramtyp_id_percentage.png"),
        dpi = 1000
    )