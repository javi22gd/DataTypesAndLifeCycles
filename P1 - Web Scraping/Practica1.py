#!/usr/bin/env python
# coding: utf-8

# # Tipología y Ciclo de Vida de los Datos
# ## Práctica 1: *Web scraping*
# ### Javier Gómez de Diego

# In[1]:


import pandas as pd
import requests
import builtwith
import whois
import time
import warnings
import urllib.request
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import Select
from chromedriver_py import binary_path


# In[2]:


driver = webdriver.Chrome()                                               # Open the browser
driver.get('https://about.netflix.com/es_es/new-to-watch')                # Load the page
driver.execute_script("window.scrollTo(0, document.body.scrollHeight)")   # Scroll down for the page to load completely
time.sleep(1)                                                             # Wait for the rest of the page to load
driver.execute_script("window.scrollTo(0, document.body.scrollHeight)")   # Scroll to the bottom

s = []                                                                    # Array for pages' content: every element is the soup for one of the pages
s.append(BeautifulSoup(driver.page_source))                               # Save content of current page

# Navigation throught all pages while saving the content
pag = 0    
sig = 2
while len(driver.find_elements(By.XPATH, "//*[text()='{sig:}']".format(sig=sig))) != 0:
    driver.execute_script("window.scrollTo(0, document.body.scrollHeight)")
    time.sleep(3)
    driver.find_element(By.XPATH, "//*[text()='{sig:}']".format(sig=sig)).find_element(By.XPATH, "./..").click()   # Click button to go to the next page
    time.sleep(3)
    driver.execute_script("window.scrollTo(0, document.body.scrollHeight)")
    s.append(BeautifulSoup(driver.page_source))                                                                    # Save content
    pag = pag + 1
    sig = sig + 1
         
driver.close()                                                            # Close the browser


# In[3]:


titulos = []                                                                 # Array for title name
estreno = []                                                                 # Array for release date
enlaces = []                                                                 # Array for movie's Netflix link
portadas = []                                                                # Array for movie's cover art link

# Go throught all pages saving the data
j = 0
for ss in s:
    for a in ss.find_all('a'):
        if (a.get('class') == ['link__StyledAnchor-sc-1gds0w3-0', 'cGrGmh']) & (a.p != None):
            titulos.append(a.p.string)                                                                         # Save title name
            for p in a.find_all('p'):
                if p.get('class') == ['release-schedule-grid-cardstyles__DateText-sc-15f8nyv-2', 'dJFmAg']:
                    estreno.append(p.string)                                                                   # Save release date
                    enlaces.append(a.get('href'))                                                              # Save link
    for i in ss.find_all('img'):
        if j<len(titulos):
            if i.get('alt') == titulos[j]:
                portadas.append(i.get('src'))                                                                  # Save cover
                j=j+1
        else: break


# In[4]:


# Set headers for multiple requests
headers = {
    'authority': 'scrapeme.live',
    "Accept-Language": "es-SP",
    'dnt': '1',
    'upgrade-insecure-requests': '1',
    'user-agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/5\
    37.36 (KHTML, like Gecko) Chrome/95.0.4638.69 Safari/537.36',
}

year = []                                                           # Array for year of production
genero = []                                                         # Array for genre
duracion = []                                                       # Array for duration (time or seassons)
edad = []                                                           # Array for age rating
protagonistas = []                                                  # Array for leading actors
sinopsis = []                                                       # Array for synopsis

# Access Netflix movies' sites
for e, i in zip(enlaces, range(len(enlaces))):
    time.sleep(.5)
    page = requests.get(e, headers=headers)
    
    # Check response from server
    if page.status_code >= 200 & page.status_code < 300:
        print('Página ' + str(i) + ' de ' + str(len(enlaces) - 1) + ' accedida correctamente.', end='\r')
        
        s = BeautifulSoup(page.content)
        
        # Extract genre
        if s.find('a', {'class':'title-info-metadata-item item-genre'}) != None:
            genero.append(s.find('a', {'class':'title-info-metadata-item item-genre'}).string)
        else: genero.append('-')
        
        # Extract duration
        if s.find('span', {'class':'duration'}) != None:
            duracion.append(s.find('span', {'class':'duration'}).string)
        elif s.find('span', {'class':'title-info-metadata-item item-runtime'}) != None:
            duracion.append(s.find('span', {'class':'title-info-metadata-item item-runtime'}).string)
        else: duracion.append('-')
        
        # Extract year
        if s.find('span', {'class':'title-info-metadata-item item-year'}) != None:
            year.append(s.find('span', {'class':'title-info-metadata-item item-year'}).string)
        else: year.append('-')
        
        #Extract age rating
        if s.find('span', {'class':'title-info-metadata-item item-maturity'}) != None:
            edad.append(s.find('span', {'class':'title-info-metadata-item item-maturity'}).string)
        else: edad.append('-')
        
        # Extract leading actors
        if s.find('span', {'class':'title-data-info-item-list'}) != None:
            protagonistas.append(s.find('span', {'class':'title-data-info-item-list'}).string)
        else: protagonistas.append('-')
        
        # Extract synopsis
        if s.find('div', {'class':'title-info-synopsis'}) != None:
            sinopsis.append(s.find('div', {'class':'title-info-synopsis'}).string)
        else: sinopsis.append('-')
    
    elif page.status_code == 403:
        print('\nPágina ' + str(i) + ' bloqueada.')
    
    else: print('\nPágina ' + str(i) + ' no accedida.')


# In[5]:


# Download covers and set unique title
for i, j, k in zip(portadas, titulos, range(len(titulos))):
    urllib.request.urlretrieve(i, ("Portadas/" + (str(k+1) + "_" + ''.join(char for char in j if char.isalnum())) + ".jpg"))


# In[6]:


# Create DataFrame
netflixcomingsoonDF = pd.DataFrame({'Titulo':titulos,
                                    'Fecha Estreno':estreno,
                                    'Genero':genero,
                                    'Duracion':duracion,
                                    'Año':year,
                                    'Clasificacion Edad':edad,
                                    'Protagonistas':protagonistas,
                                    'Sinopsis':sinopsis,
                                    'Link':enlaces,
                                    'Portada':portadas})


# In[7]:


# Save data as CSV
netflixcomingsoonDF.to_csv('Datos/NetflixComingSoon.csv', index=False, sep=';')  # Save as CSV

