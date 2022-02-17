
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
# doing with N = 20 
SP_to_join <- SPdf %>% head(20+1) %>% mutate(t_incremented = row_number())

SP <- SPdf %>% head(20) %>% mutate(t = row_number(), 
                                   t_incremented = row_number() + 1
                                   )

SP_increments <- SP %>% 
  left_join(SP_to_join, 
            by = "t_incremented", 
            suffix = c(".x", ".y")
            ) %>% 
  mutate(increments     = GSPC.Close.y - GSPC.Close.x,
         abs_increments = abs(GSPC.Close.y - GSPC.Close.x)
        )%>%
  compute("SP_increments")

total_increments <- SP_increments %>% select(abs_increments) %>% sum()

SP_increments <- SP_increments %>% mutate(measure = abs_increments / total_increments)

SPdf %>% head(20)
SPdf %>% arrange(desc(date)) %>% head(20)

SP_increments %>% colnames()
measure_table <- SP_increments %>% select(measure)

class(SP_increments$date.x)

# creating empty data frame to populate the sums for the different values of q

iterations = 304
variables = 3
i <- 1
output <- matrix(ncol=variables, nrow=iterations)

for(q in seq(from=-5, to=5, by=0.033)){
  
  powers = sum('^'(SP_increments$measure,q))
  output[i,1] <- i
  output[i,2] <- q
  output[i,3] <- powers
  i = i + 1
}
output %>% head(6)

SP_increments %>% mutate(powers = '^'(measure,-5+(0.033*303))) %>% select(powers) %>% sum() 

column_names <- c("iteration", "q", "Zqn")
data <- data.frame(output)
colnames(data) <- column_names
data %>% head(6)

# setting up data for specific heat curve, for first 10 rows (delete 10 later and include all rows)
specific_heat <- data %>% mutate(tau_q = ln(Zqn)/ln(20),
                                              iteration_plus  = iteration + 1,
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
  mutate(Cq = tau_q_plus - two_tau_q + tau_q_minus)%>%
  compute("specific_heat_data")

#specific_heat_data

# plotting specific heat curve 
ggplot(specific_heat_data) +
  geom_line(aes(x = q, y= Cq))
