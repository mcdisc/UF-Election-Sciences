# -*- coding: utf-8 -*-
"""
Created on Fri May 13 13:57:44 2016

@author: Tyler
"""
#this bit imports pandas and other data processing packages that makes this project easier
import pandas as pd
import datetime
from numpy import random
import matplotlib.pyplot as plt
import sys #only needed to determine Python version number
import matplotlib #only needed to determine Matplotlib version number and to use for data analysis, super fun

#creates two dataframes from csv files 
Location = r'C:\Users\Tyler\Documents\Data Projects\National Voter Data File\Ohio\VINTON.csv'
df = pd.read_csv(Location)
df['Type'] = ("PURGE")
Location2 = r'C:\Users\Tyler\Documents\Data Projects\National Voter Data File\Ohio\VINTON2.csv'
df2 = pd.read_csv(Location2)
df2['Type'] = ("NEWVR")
frames = [df, df2]
df4 = pd.concat(frames)
df5=df4.drop_duplicates(['SOS_VOTERID'], keep=False)
df5.to_csv(r'C:\Users\Tyler\Documents\Data Projects\National Voter Data File\Ohio\change.csv')

