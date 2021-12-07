# -*- coding: utf-8 -*-
"""
Created on Tue Nov 30 22:45:16 2021

@author: David
"""

import pandas as pd
import datetime


df = pd.read_csv("C:/Users/David/Desktop/4.UNAL2021-2/TAE/trabajo1/incidentes_viales.csv", sep=";")

#datetime.datetime.strptime("21/12/2008", "%d/%m/%Y %H:%M:%S").strftime("%Y-%m-%d")


#from datetime import datetime
#d = datetime.fromisoformat("2014-08-05T12:15:00.000Z"[:-1])
#d.strftime('%Y-%m-%d %H:%M:%S')


df["year_accident"] = pd.to_datetime(df["FECHA_ACCIDENTES"])

import datetime
df3 =  datetime.datetime.strptime(df["year_accident"], "%d/%m/%Y %H:%M:%S").strftime("%d/%m/%Y")


datetime.datetime.strptime(df["FECHA_ACCIDENTES"], "%d/%m/%Y").strftime('%Y-%m-%d')

df.to_csv('incidentes_date.csv')
