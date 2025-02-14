# -*- coding: utf-8 -*-
"""
Created on Fri Feb 14 16:40:50 2025

@author: adamd
"""

import requests
from bs4 import BeautifulSoup

# URL of the page to scrape
url = 'https://portalanterior.ine.mx/documentos/RESELEC/nuevo_1994/sen_94/ent_mpio/21_sen_94.html'

# Send a GET request to the URL
response = requests.get(url)

# Check if the request was successful
if response.status_code == 200:
    # Parse the HTML content of the page
    soup = BeautifulSoup(response.content, 'html.parser')
    
    # Extract the desired content
    # For example, to get all the text content
    text_content = soup.get_text()
    
    # Print the text content
    print(text_content)
    
    # You can also extract specific elements by their tags or classes
    # For example, to get all paragraphs
    paragraphs = soup.find_all('p')
    for paragraph in paragraphs:
        print(paragraph.get_text())
else:
    print(f"Failed to retrieve the page. Status code: {response.status_code}")