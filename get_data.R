
# installing and loading in packages
install.packages('quantmod')
install.packages('TSstudio')
library(quantmod)
library(dplyr)
library(TSstudio)

#loading in S&P 500 data (titled GSPC) from Yahoo; other indexes can be loaded into the vector, 
#e.g. Dow Jones, under ^DJI)
getSymbols("^GSPC", src = "yahoo")
getSymbols("^DJI", src = "yahoo")
  saveSymbolLookup(file = "indexes.rda")  #saving data to the file 'indexes.rda'
  getSymbols(c("^GSPC", "^DJI"))

# checking the class of data GSPC ('xts' and 'zoo')  
class(GSPC)

#write.zoo(^GSPC, file="SPindex.csv", sep=",") #no 
#setSymbolLookup('^GSPC'=list(src="csv",format="%Y-%m-%d")) #no
#GSPC <- getSymbols("^GSPC", auto.assign=FALSE) #no

View(GSPC)  #outputting table GSPC

# Outputting line and bar chart from whole GSPC data set, barChart is a quantmod function
barChart(GSPC)

#------------------------------------------------extracting relevant columns (closing price, time) and plotting

#Extracting 1st (time variable) column and "GSPC.Close" column from the GSPC data set and assigning to SandP_data
#GSPC.Close column I assume represents the closing prices
SandP_data <- GSPC[, "GSPC.Close"]

View(SandP_data)

#Outputting line chart of SandP_data
barChart(SandP_data)

class(SandP_data)

#Installed the TSstudio package which allows for plotting of xts and zoo class objects
#checking data structure of GSPC and SandP_data 
ts_info(GSPC)   
ts_info(SandP_data)

#plotting time series of SandP data (realized that the barChart function can be used without this extra package)
ts_plot(SandP_data,
        title = "S & P 500 Time series [source: Yahoo Finance]",
        Xtitle = "Time (using daily data)",
        Ytitle = "Closing price"
        )

# function for extracting the closing price, time variables from any index, when the closing price column is 
# the nth column
time_series_data <- function(index_name, n){
  index_name[, n]
}

#Saving the subset of the index output by the function above to an object 'index_data'
index_data <- time_series_data(GSPC, 4)

#Viewing subset index 
View(index_data)

# top 20 rows of the SandP dataset
head(SandP_data, 20)
SandP_data[,0] %>% head()
SandP_data %>% head(20)

#converting data from xts to data frame
SPdf <- data.frame(date=index(SandP_data), coredata(SandP_data))

# beginning with AVR method; determining the increments (1's will be changed to t's in the function) (creating miew)
SP_to_join <- SPdf %>% head(20+1) %>% mutate(t_incremented = row_number())

SP <- SPdf %>% head(20) %>% mutate(t = row_number(), 
                                   t_incremented = row_number() + 1
                                   )
SP_increments <- SP %>% left_join(SP_to_join, 
                                   by = "t_incremented", 
                                   suffix = c(".x", ".y")) %>% 
  mutate(increments     = GSPC.Close.y - GSPC.Close.x,
         abs_increments = abs(GSPC.Close.y - GSPC.Close.x)
        )

total_increments <- SP_increments %>% select(abs_increments) %>% sum()

SP_increments %>% mutate(measure = abs_increments / total_increments)
