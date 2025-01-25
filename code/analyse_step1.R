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
            betriebsdatum = dmy(betriebsdatum),
            year = year(betriebsdatum),
            year = as.factor(year),
            verspaetung_cat = 
                case_when(
                    verspaetung < -60 ~ -1,
                    verspaetung < 120 ~ 0,
                    verspaetung < 300 ~ 1,
                    TRUE ~ 2

                ),
            verspaetung_cat = as.factor(verspaetung_cat) %>% fct_rev()        )

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
