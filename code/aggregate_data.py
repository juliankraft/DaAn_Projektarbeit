import pandas as pd
import os
from pathlib import Path

def path_to_year(year):
    path = datapath / year
    return path

def files_in_year(year, filter=None):
    all_files = os.listdir(path_to_year(year))
    if filter:
        return [f for f in all_files if filter in f]
    else:
        return all_files

def aggregate_data_per_file(file_path, save_file_path: Path):
    data = pd.read_csv(file_path)

    # filtering for line 1 - 20 (only tram) and fw_typ 1 (only regular service)
    filtered_data = data.where((data['linie'] <= 20) & (data['fw_typ'] == 1)).dropna()

    # calculate delay
    filtered_data['verspaetung'] = filtered_data['ist_an_von'] - filtered_data['soll_an_von']

    # select relevant columns
    cols_to_select = ['linie', 'betriebsdatum', 'fahrzeug', 'soll_an_von', 'verspaetung', 'halt_id_von']
    selected_data = filtered_data[cols_to_select]

    # ensure the directory for save_file_path exists
    if not save_file_path.parent.exists():
        save_file_path.parent.mkdir(parents=True)

    with save_file_path.open('a') as f:
        selected_data.to_csv(f, index=False, header=f.tell()==0)


if __name__ == '__main__':

    datapath = Path('/cfs/earth/scratch/kraftjul/DaAn_Projektarbeit/data/')
    years = ['2016', '2017', '2018', '2019', '2020', '2021', '2022']
    save_path = datapath / 'aggregated_data'

    for year in years:

        files = files_in_year(year, filter='Fahrzeiten_SOLL_IST')

        for file in files:

            file_path = path_to_year(year) / file
            save_file_path = save_path / f'{year}_aggregated.csv'

            aggregate_data_per_file(file_path, save_file_path)

