# installing and loading in packages
install.packages('quantmod')
install.packages('TSstudio')
install.packages('SciViews')
library(quantmod)
library(dplyr)
library(TSstudio)
library(SciViews)
library(ggplot2)

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
View(DJI)
#------------------------------------------------extracting relevant columns (closing price, time) and plotting

#Extracting 1st (time variable) column and "GSPC.Close" column from the GSPC data set and assigning to SandP_data
#GSPC.Close column I assume represents the closing prices
SandP_data <- GSPC[, "GSPC.Close"]
DowJones <- DJI[, "DJI.Close"]

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

#Saving the subset of the index output by the function above to an object 'index_data', getting S&p AND DOE JONES
index_data <- time_series_data(GSPC, 4)
index_data2 <- time_series_data(DJI, 4)
index_data2 %>% head(6)

#Viewing subset index 
View(index_data)

# top 20 rows of the SandP dataset
head(SandP_data, 20)
SandP_data[,0] %>% head()
SandP_data %>% head(20)

#converting data from xts to data frame
SPdf <- data.frame(date=index(SandP_data), coredata(SandP_data))
DJ <- data.frame(date=index(index_data2), coredata(index_data2))
DJ %>% head(6)

#DJ %>% filter(date >= start_date & date <= start_date + 50)
#as.Date("2008-10-19") + 50
#start_date <- as.Date("2008-10-19")

#as.Date("2001-01-01") + 45 ------------ this outputs as class 'date'

# beginning with AVR method; determining the increments (1's will be changed to T's in the function) (creating miew)
# doing with N = 20 
# Output table SP_increments which gets the measure miew based off of a time series of N elements, t=1,2,.,N(t=NUMBER OF ROWS)
# and T, the interval size, here we use T=1 (put T, N in the arguments of the function)- delete unneeded columns
# modulating: #TO DO WHEN FILTERING FOR CRASHES: make N into a filter of a time series between dates
#Inputs: stock market index to be used, index of the closing price column, N, T interval, start date of time series,
# period of time over which to analyse (in days), these last 2 variables to be added
#Output: measure column, miew -------------------------------------------------------, start_date, period
measure_data <- function(index_name, closing_column, N, interval_T){
  
  output_index <-index_name[, closing_column]
  
  index_dataset <- data.frame(date=index(output_index), coredata(output_index))
  
  # the head(N+interval_T) will be changed appropriately to accommodate for date filter and periods over which
  # the time series needs to be analysed 
  
  index_to_join <- index_dataset %>% head(N+interval_T) %>% mutate(t_incremented = row_number())
  
  index_sub_dataset <- index_dataset %>% head(N) %>% mutate(t = row_number(), 
                                     t_incremented = row_number() + interval_T
                                     )
  SP_increments <- index_sub_dataset %>% 
    left_join(index_to_join, 
              by = "t_incremented", 
              suffix = c(".x", ".y")
    ) %>% 
    mutate(increments     = DJI.Close.y - DJI.Close.x,
           abs_increments = abs(DJI.Close.y - DJI.Close.x)
    )%>%
    compute("SP_increments")
  
  total_increments <- SP_increments %>% select(abs_increments) %>% sum()
  
  SP_increments <- SP_increments %>% mutate(measure = abs_increments / total_increments) %>% select("measure")
  
  return(SP_increments)  #whole table returned but only column 'measure' is needed
}
#utilizing above function with N=20, T= 1
measure_data(DJI, 4, 20, 1)


# Creating empty data frame to populate the sums for the different values of q, modulating this now:
# number of iterations depend on the range of values for q and the increment value; parameterize the increments and
# the range 
partition_function_data <- function(){
  
  SP_increments <- measure_data(DJI, 4, 20, 1)
  
  iterations = 304
  variables = 3
  i <- 1
  output <- matrix(ncol=variables, nrow=iterations)
  
  for(q in seq(from=-5, to=5, by=0.033)){
    
    powers = sum('^'(SP_increments$measure,q))  #need to change SP_increments to the table outputted by measure_data
    output[i,1] <- i
    output[i,2] <- q
    output[i,3] <- powers
    i = i + 1
  }
  column_names <- c("iteration", "q", "Zqn")
  PF_data <- data.frame(output)
  colnames(PF_data) <- column_names
  
  return(PF_data)
}

partition_function_data() %>% head(6)

# setting up data for specific heat curve
# modulating this here: (the commands below), N=number of time series elements (number of rows)
# outputting the table with 2 columns; q and Cq
specific_heat_function <- function(N){
  
  data <- partition_function_data()
  
  specific_heat <- data %>% mutate(tau_q = -1*ln(Zqn)/ln(N),
                                   iteration_plus  = iteration + 1,  #getting tau_q + 1 and tau_q - 1 for Cq
                                   iteration_minus = iteration - 1)
  
  specific_heat_data <- specific_heat %>% 
    left_join(specific_heat %>% 
                select("iteration" = "iteration_plus", "tau_q_minus" = "tau_q"), 
              by = "iteration"
    ) %>%
    left_join(specific_heat %>% 
                select("iteration" = "iteration_minus", "tau_q_plus" = "tau_q"), 
              by = "iteration"
    ) %>% 
    mutate(two_tau_q = 2* tau_q) %>%
    mutate(Cq = -1*(tau_q_plus - two_tau_q + tau_q_minus))%>%
    select("q", "Cq") %>%
    compute("specific_heat_data")
  
  return(specific_heat_data)
}

#plotting the specific heat data (Cq against q)
ggplot(specific_heat_function(20)) +
  geom_line(aes(x = q, y= Cq))

specific_heat_function(20) %>% head(6)

