# -*- coding: utf-8 -*-
"""
Created on Sun Aug  7 13:50:08 2016

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
Location = r'C:\Users\Tyler\Documents\Data Projects\National Voter Data File\Ohio\Voter Files\Messing Around\VINTON1.csv'
df_a = pd.read_csv(Location)
Location2 = r'C:\Users\Tyler\Documents\Data Projects\National Voter Data File\Ohio\Voter Files\Messing Around\VINTON2.csv'
df_b = pd.read_csv(Location2)

merged = pd.merge(df_a, df_b, on='SOS_VOTERID', how='left', suffixes=('_week1', '_week2'))