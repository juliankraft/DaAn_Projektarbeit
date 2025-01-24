import pycurl
import json
import requests
from io import BytesIO
import pandas as pd
import re

from pathlib import Path


# Function to get metadata from the opendata API
def get_metadata(url, filter):
    """
    Get metadata from the opendata API.

    Parameters:
    url (str): The URL to the API.
    filter (str): The filter to search for.

    Returns:
    dict: A JSON object containing the metadata if the request is successful.
    None: If the request is not successful or the response is empty.
    """
    buffer = BytesIO()
    c = pycurl.Curl()
    c.setopt(c.URL, f'{url}package_search?q=title:{filter}')
    c.setopt(c.WRITEDATA, buffer)

    c.perform()
    c.close()

    response = buffer.getvalue().decode('utf-8')

    if not response.strip():
        print("The response is empty.")
    else:
        try:
            data = json.loads(response)

            if data.get('success'):
                return data
            else:
                print("API did not return a successful response:", data)
        except json.JSONDecodeError as e:
            print("Failed to decode JSON:", e)
            print("Response content:", response)

def save_json(data, filename):
    with open(filename, 'w') as json_file:
        json.dump(data, json_file, indent=4)

def filter_result_by_year(data, year):
    for result in data['result']['results']:
        if str(year) in result['name']:
            return result
        
def get_resource_url_for_year(data, year):
    combined_data = {
        'data': [],
        'metadata': []
    }

    result = filter_result_by_year(data, year)

    for resource in result['resources']:
        url = resource['download_url']
        if "Haltepunkt" in url or "Haltestelle" in url:
            combined_data['metadata'].append(url)
        else:
            combined_data['data'].append(url)

    return combined_data

def pack_all_urls_into_one_dict(url, filter):
    data = get_metadata(url, filter)

    all_urls = {}

    for dataset in data['result']['results']:
        year = re.search(r'\d{4}', dataset['name']).group(0)

        all_urls[year] = get_resource_url_for_year(data, year)
    
    all_urls_sorted = dict(sorted(all_urls.items()))

    return all_urls_sorted


# Function to download the data from the opendata API
def download_csv(url):

    try:
        response = requests.get(url)
        response.raise_for_status()
        return response.content
    except requests.exceptions.RequestException as e:
        print("Failed to download the data:", e)
        return None

def save_csv(content, filename):
    try:
        with open(filename, 'wb') as csv_file:
            csv_file.write(content)
    except IOError as e:
        print("Failed to save the data:", e)


# defining the download function
def downloading_data(data_dict, data_path):

    for key, value in data_dict.items():

        save_path = data_path / f"{key}"

        if not save_path.exists():
            save_path.mkdir()
        else:
            raise FileExistsError("The directory already exists.")

        for url in value['data']:
            data = download_csv(url)
            file_path = save_path / url.split('/')[-1]
            save_csv(data, file_path)

        for url in value['metadata']:
            data = download_csv(url)
            file_path = save_path / url.split('/')[-1]
            save_csv(data, file_path)
            

if __name__ == "__main__":
    
    # Path setup
    project_path = Path('/cfs/earth/scratch/kraftjul/DaAn_Projektarbeit')
    data_path = project_path / 'data'

    # filter and download setup
    url = "https://ckan.opendata.swiss/api/3/action/"
    filter = "Fahrzeiten"
    storage_options = {'User-Agent': 'Mozilla/5.0'}

    # get metadata
    download_url = pack_all_urls_into_one_dict(url, filter)

    # download data
    downloading_data(download_url, data_path)
