#!/usr/bin/env python
# coding: utf-8

# # Tipología y Ciclo de Vida de los Datos
# ## Práctica 2: Limpieza y análisis de datos
# ### Javier Gómez de Diego

# In[1]:


import numpy as np
import pandas as pd
import sklearn
import matplotlib.pyplot as plt
import re


# In[2]:


a = pd.read_csv('activities.csv', sep=',')

a.columns


# In[3]:


a.columns[a.columns.str.contains('translation_missing')]


# In[4]:


structure = '<span (?:.*?)>(.*?)</span>'
newc = {}
for i in a.columns[a.columns.str.contains('translation_missing')]:
    newc[i] = re.findall(structure, i)[0]
a.rename(columns = newc, inplace = True)
a.columns


# In[5]:


a.to_csv('activities2.csv', index=False, sep=',')  # Save as CSV

