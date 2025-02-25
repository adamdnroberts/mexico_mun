# -*- coding: utf-8 -*-
"""
Created on Fri Feb 14 16:40:50 2025

@author: adamd
"""

import requests
from bs4 import BeautifulSoup
import pandas as pd
from tqdm import tqdm

#%%
def scrape_data(url):
    # Send a GET request to the URL
    response = requests.get(url)

    # Check if the request was successful
    if response.status_code == 200:
        # Parse the HTML content of the page
        soup = BeautifulSoup(response.content, 'html.parser')
    else:
        print("Failed to retrieve the webpage.")
        return None

    # Find all rows in the table
    rows = soup.find_all('tr')

    # Define the header and initialize the data list
    header = [["Estado", "Municipio", "SECCIONES", "CASILLAS", "PAN", "PRI", "PPS", "PRD", "PFCRN", "PARM", "UNO_PDM", "PT", "PVEM", "NO REG", "NULOS", "TOTAL", ""]]
    data = header

    # Append the first row of data
    data.append(rows[5].get_text().split('\n'))

    # Define the range for the loop (e.g., from 8 to the length of rows)
    start = 8
    end = len(rows)

    # Loop through only even numbers
    for i in range(start, end + 1, 2):
        try: 
            data.append(rows[i].get_text().split('\n'))
        except IndexError:
            print(f"Index {i} is out of range.")

    # Create a DataFrame from the data
    df = pd.DataFrame(data)

    # Step 1: Extract the first row as the new column names
    new_columns = df.iloc[0]

    # Step 2: Remove the first row from the DataFrame
    df = df[1:]

    # Step 3: Assign the new column names to the DataFrame
    df.columns = new_columns

    # Reset the index (optional, to have a clean index starting from 0)
    df = df.reset_index(drop=True)
    
    return df

#%%
# Example usage
url = "https://portalanterior.ine.mx/documentos/RESELEC/nuevo_1994/sen_94/ent_mpio/1_sen_94.html"
df = scrape_data(url)
df["Estado"] = 1

#%%
for i in tqdm(range(2,33)):
    url = "https://portalanterior.ine.mx/documentos/RESELEC/nuevo_1994/sen_94/ent_mpio/" + str(i) + "_sen_94.html"
    df2 = scrape_data(url)
    df2["Estado"] = i
    df = pd.concat([df, df2], ignore_index=True)

df = df.iloc[:, :-1]
#%%
file_path = 'C:/Users/adamd/Documents/mexico_mun/data/mex94_senate.csv'

#df['Estado'].value_counts(ascending=True)
#df['Estado'].describe()

df.to_csv(file_path, index=False)

#%% Deputies, PR
url = "https://portalanterior.ine.mx/documentos/RESELEC/nuevo_1994/dip_94/ent_mpio/1_drp_94.html"
df = scrape_data(url)
df["Estado"] = 1

for i in tqdm(range(2,33)):
    url = "https://portalanterior.ine.mx/documentos/RESELEC/nuevo_1994/dip_94/ent_mpio/" + str(i) + "_drp_94.html"
    df2 = scrape_data(url)
    df2["Estado"] = i
    df = pd.concat([df, df2], ignore_index=True)
    
df = df.iloc[:, :-1]

file_path = 'C:/Users/adamd/Documents/mexico_mun/data/mex94_drp.csv'

#df['Estado'].value_counts(ascending=True)
#df['Estado'].describe()

df.to_csv(file_path, index=False)

#%% Deputies, MR
url = "https://portalanterior.ine.mx/documentos/RESELEC/nuevo_1994/dip_94/ent_mpio/1_dmr_94.html"

df = scrape_data(url)
df["Estado"] = 1

for i in tqdm(range(2,33)):
    url = "https://portalanterior.ine.mx/documentos/RESELEC/nuevo_1994/dip_94/ent_mpio/" + str(i) + "_dmr_94.html"
    df2 = scrape_data(url)
    df2["Estado"] = i
    df = pd.concat([df, df2], ignore_index=True)
    
df = df.iloc[:, :-1]

file_path = 'C:/Users/adamd/Documents/mexico_mun/data/mex94_dmr.csv'

#df['Estado'].value_counts(ascending=True)
#df['Estado'].describe()

df.to_csv(file_path, index=False)

#%% Presidencia
url = "https://portalanterior.ine.mx/documentos/RESELEC/nuevo_1994/pres_94/ent_mpio/1_pre_94.html"

df = scrape_data(url)
df["Estado"] = 1

for i in tqdm(range(2,33)):
    url = "https://portalanterior.ine.mx/documentos/RESELEC/nuevo_1994/pres_94/ent_mpio/" + str(i) + "_pre_94.html"
    df2 = scrape_data(url)
    df2["Estado"] = i
    df = pd.concat([df, df2], ignore_index=True)
    
df = df.iloc[:, :-1]

file_path = 'C:/Users/adamd/Documents/mexico_mun/data/mex94_pres.csv'

#df['Estado'].value_counts(ascending=True)
#df['Estado'].describe()

df.to_csv(file_path, index=False)