#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Apr  7 13:40:34 2022

@author: nrobinson15
"""

import numpy as np
import pandas as pd
from numpy.random import normal
import matplotlib.pyplot as plt 
import yfinance as yf


##############################################################################

array_prices_total = pd.DataFrame()

data_df = yf.download('^DJI', 
                      start='2020-01-01', 
                      end='2022-04-20', 
                      progress=False,
)


data_df_prices = np.array(data_df['Close'])

data_df['Dates'] = data_df.index

data_df_dates = np.array(data_df['Dates'])

data_df_dates = pd.to_datetime(data_df_dates)


for dates in data_df_dates:
    dates = dates.date()

prices_GSPC = data_df_prices

dates_year_GSPC = []
dates_GSPC_MODEL = []

for i in range(0, len(data_df_dates)):
    dates_year_GSPC.append(data_df_dates[i].date().year)
    dates_GSPC_MODEL.append(data_df_dates[i].date())
    
    
    
    

#df = pd.read_excel('SP_data.xlsx', index_col=None, header=None)





#prices_GSPC = np.array(df[1][1:])
#dates_GSPC = np.array(df[0][1:])



#year_index=[]
#dates_year_GSPC = []

#dates_GSPC_MODEL = []
#count = 0 
#for dates in dates_GSPC:
#    count = count + 1
   
 
    
   


#    dates_GSPC_MODEL.append(dates.date())
    
    
#    dates_year_GSPC.append(dates.year)


#dates_year_GSPC = list(filter(lambda v: v==v, dates_year_GSPC))
#prices_GSPC = list(filter(lambda v: v==v, prices_GSPC))




##############################################################################


for j in range(0,3):

    prices_new_GSPC = prices_GSPC[j*150:(j+1)*150]
    dates_new_year_GSPC = dates_year_GSPC[j*150:(j+1)*150]
    datetimes_new_year_GSPC = dates_GSPC_MODEL[j*150:(j+1)*150]
    
    for i in range(0, len(prices_new_GSPC)):
        prices_new_GSPC[i] = float(prices_new_GSPC[i])
        dates_year_GSPC[i] = int(dates_year_GSPC[i])
    
    start_year = dates_new_year_GSPC[0]
    end_year = dates_new_year_GSPC[149]
    
       
    
    n_years = (end_year - start_year)+1
    

    
    count_year = []
    
    for i in range(start_year,end_year+1):
        count_year.append(len(list(filter(lambda year: year == i, dates_new_year_GSPC))))
    

    closing_percentage_array = []
    
    closing_data_for_year = {"test": "haha"}
    
    for i in range(0,len(dates_new_year_GSPC)):
        if i==len(dates_new_year_GSPC)-1:
            break
        closing_percentage_array.append((abs(prices_new_GSPC[i]-prices_new_GSPC[i+1])/prices_new_GSPC[i]))
    
    closing_percentage_array.append((abs(prices_new_GSPC[len(dates_new_year_GSPC)-2]-prices_new_GSPC[len(dates_new_year_GSPC)-1])/prices_new_GSPC[len(dates_new_year_GSPC)-1]))
    
    means = []
    stds = []
    
   

    
    
    add_1 = 0
    for i in range(0,n_years):
        add_2 = count_year[i] + add_1
        closing_data_for_year[i] = closing_percentage_array[add_1:add_2]
        means.append((prices_new_GSPC[add_2-1]-prices_new_GSPC[add_1])/(prices_new_GSPC[add_1]*count_year[i]))
        add_1 = count_year[i]
        
       
    
       
    for i in range(0,n_years):
        stds.append(np.std(closing_data_for_year[i])/20)
        
  
    daily_percentage_returns = {"test": "haha"}

    
    for i in range(0,n_years):
        daily_percentage_returns[i]=normal(loc=means[i], scale=stds[i], size=count_year[i])
        
    
        
    total_percentage_returns = daily_percentage_returns[0]
    for i in range(1,n_years):
        total_percentage_returns = np.concatenate((total_percentage_returns, daily_percentage_returns[i]))
    
 
    x0 = prices_new_GSPC[0]
    
    prices=[x0]
    x1= x0

    for i in range(0, len(dates_new_year_GSPC)-1):   
        if i>count_year[0]:
            x0 = prices_new_GSPC[count_year[0]+1]
        price = x1 + x0*total_percentage_returns[i]
        prices.append(price)  
        x1 = price
         
 
         
    prices_index = np.linspace(0, len(dates_new_year_GSPC), num=len(dates_new_year_GSPC))
    
   
   
    dates_year_GSPC.pop()
    
    df_prices = pd.DataFrame(prices)
    df_dates = pd.DataFrame(dates_GSPC_MODEL)
   
    df_prices_array = np.array(df_prices)
  
  


   # plt.plot(datetimes_new_year_GSPC, df_prices_array, color="red")
    
   # plt.plot(datetimes_new_year_GSPC, prices_new_GSPC, color="blue")
   
   # plt.show()
    
    array_prices_total = pd.concat([array_prices_total, df_prices], axis=0)
  
    
   
    
print("Done")
array_prices_total.to_excel("Prices_DJI_extra.xlsx", index=False)
df_dates.to_excel("Dates_DJI_extra.xlsx", index=False)


   
