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
measure_data <- function(index_name, closing_column, starting_date, N, interval_T){
  
  output_index <-index_name[, closing_column]
  
  index_dataset <- data.frame(date=index(output_index), coredata(output_index))
  
  # the head(N+interval_T) will be changed appropriately to accommodate for date filter and periods over which
  # the time series needs to be analysed 
  
  index_to_join <- index_dataset %>% 
    filter(date >= as.Date(starting_date)) %>%
    arrange(date) %>%
    head(N+interval_T) %>% 
    mutate(t_incremented = row_number())
  
  index_sub_dataset <- index_dataset %>% 
    filter(date >= as.Date(starting_date)) %>%
    arrange(date) %>% 
    head(N) %>% 
    mutate(t = row_number(), 
           t_incremented = row_number() + interval_T
           )
  
  SP_increments <- index_sub_dataset %>% 
    left_join(index_to_join, 
              by = "t_incremented", 
              suffix = c(".x", ".y")
    ) %>% 
    mutate(increments     = DJI.Close.y - DJI.Close.x,
           abs_increments = abs(DJI.Close.y - DJI.Close.x)
    ) %>%
    compute("SP_increments")
  
  total_increments <- SP_increments %>% select(abs_increments) %>% sum()
  
  SP_increments <- SP_increments %>% mutate(measure = abs_increments / total_increments) #%>% select("measure")
  
  return(SP_increments)  #whole table returned but only column 'measure' is needed
}

#utilizing above function with N=20, T= 1 + the other input parameters 
measure_data(DJI, 4, "2007-01-03", 20, 1)




# Adding a start date into the above (and time period, T)-------------------------

output_index <- DJI[, 4]

index_dataset <- data.frame(date=index(output_index), coredata(output_index))

index_to_join <- index_dataset %>% 
  filter(date >= as.Date("2007-01-03")) %>%
  arrange(date) %>%
  head(1000)  #the value of N 


index_to_join
#%>%
  
#  head(N+interval_T) %>% mutate(t_incremented = row_number())


#index_dataset %>% head()
#--------------------------------------------DONE---------------------------------




# Creating empty data frame to populate the sums for the different values of q, modulating this now:
# number of iterations depend on the range of values for q and the increment value; parameterize the increments and
# the range 
#measure_data(DJI, 4, "2007-01-03", 20, 1)


partition_function_data <- function(index_name, closing_column, starting_date, N, interval_T){
  
  SP_increments <- measure_data(index_name, closing_column, starting_date, N, interval_T)
  
  iterations = 306
  variables = 3
  i <- 1
  output <- matrix(ncol=variables, nrow=iterations)
  
  for(q in seq(from=-5.033, to=5.033, by=0.033)){
    
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

partition_function_data(DJI, 4, "2007-01-03", 21, 1) %>% head(6)

# setting up data for specific heat curve
# modulating this here: (the commands below), N=number of time series elements (number of rows)
# outputting the table with 2 columns; q and Cq
specific_heat_function <- function(index_name, closing_column, starting_date, N, interval_T){
  
  data <- partition_function_data(index_name, closing_column, starting_date, N, interval_T)
  
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
    mutate(Cq = -1*(tau_q_plus - two_tau_q + tau_q_minus),
           iteration = iteration - 1)%>%
    filter(!is.na(Cq)) %>%
    select("iteration", "q", "Cq") %>%
    compute("specific_heat_data")
  
  return(specific_heat_data)
}


#plotting the specific heat data (Cq against q)
ggplot(specific_heat_function(DJI, 4, "2007-01-03", 20, 1)) +
  geom_line(aes(x = q, y= Cq))

specific_heat_function(DJI, 4, "2007-01-03", 20, 1) %>% head()

specific_heat_function(DJI, 4, "2007-01-03", 20, 1) %>% nrow()

specific_heat_function(DJI, 4, "2007-01-03", 20, 1) %>% arrange(desc(q)) %>% head()


#integrating under the specific heat curve via the trapezium rule--------------- 
cq <- specific_heat_function(DJI, 4, "2007-01-03", 20, 1)

cq_to_join <- cq %>%
  mutate(iteration = iteration - 1)

cq %>% 
  left_join(cq_to_join,
            by = "iteration",
            suffix = c(".a", ".b")) %>% 
  mutate(area = 0.5*(Cq.a + Cq.b)*0.033)%>%
  select(area) %>%
  sum(na.rm = TRUE)
#-------------------------------------------------------------------------------

# modulating the above;
AUC <- function(index_name, closing_column, starting_date, N, interval_T){
  
  cq <- specific_heat_function(index_name, closing_column, starting_date, N, interval_T)
  
  #joining on the next value of Cq to each value of Cq in order to find the area of each trapezium in between 
  #consecutive values of Cq, separated by the increment value set in the partition function above
  auc_cq <- cq %>%                                  
    left_join(cq_to_join,
              by = "iteration",
              suffix = c(".a", ".b")) %>% 
    mutate(area = 0.5*(Cq.a + Cq.b)*0.033)%>%
    select(area) %>%
    sum(na.rm = TRUE)
  
  return(as.numeric(auc_cq))
}

AUC(DJI, 4, "2007-01-03", 20, 1)



#------------------------------------------------------------------------------
#--------------------TESTs-----------------------------------------------
DJ %>% arrange(desc(date)) %>% head()
DJ %>% head()

as.Date("2010-01-01") - 1050
DJ %>% filter(date >= start_date & date <= start_date + 50)
#as.Date("2008-10-19") + 50
#start_date <- as.Date("2008-10-19")
#---------------------------TESTS----------------------------------------

measure_data(DJI, 4, "2007-01-03", 20, 1)
partition_function_data(DJI, 4, "2007-01-03", 21, 1) %>% head()
specific_heat_function(DJI, 4, "2007-01-03", 21, 1) %>% head()
AUC(DJI, 4, "2007-01-03", 1000, 1)  #area under curve for N=20 points starting from 2007-01-03, this is the 1st index; n=1 

specific_heat_function(DJI, 4, "2007-01-03", 21, 1) %>% head()

# making area shifts below;
cq <- specific_heat_function(DJI, 4, "2007-01-03", 20, 1)

cq_to_join <- cq %>%
  mutate(iteration = iteration - 1)

cq %>% 
  left_join(cq_to_join,
            by = "iteration",
            suffix = c(".a", ".b")) %>% 
  mutate(area = 0.5*(Cq.a + Cq.b)*0.033) %>% head(10)
# need to go back to original time series dataset 
DJ %>% filter(date >= as.Date("2007-01-03") + 1) %>% arrange(date) %>% head()


#----------------------tests---------------------------------------------------
shifts = 4   #this will be l = shifts
cols = 2
n <- 1
auc_table <- matrix(ncol=cols, nrow=shifts)

AUC(DJI, 4, as.Date("2007-01-03")+18, 1000, 1)
measure_data(DJI, 4, as.Date("2007-01-03")+1, 20, 1) #good

measure_data(DJI, 4, as.Date("2007-01-03") + 18, 20, 1)
partition_function_data(DJI, 4, as.Date("2007-01-03") + 18, 20, 1) %>% head()
specific_heat_function(DJI, 4, as.Date("2007-01-03") + 29, 20, 1) %>% head()
AUC(DJI, 4, as.Date("2007-01-03") + 9, 1000, 1) #working- from what I see
#------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


# function with loop for area under the curve of N series elements shifting along a time series 
auc_cq_function <- function(total_shifts, index_name, closing_column, starting_date, N, interval_T){
  
  shifts = total_shifts + 1
  variables = 4
  n <- 1
  output <- matrix(ncol=variables, nrow=shifts)
  
  for(l in seq(from=0, to=total_shifts, by=1)){
    
    area = AUC(index_name, closing_column, (as.Date(starting_date) + l), N, interval_T)  #need to change SP_increments to the table outputted by measure_data
    output[n,1] <- n
    output[n,2] <- l
    #output[n,3] <- as.Date(as.Date(starting_date) + N + (l))  #THIS COMES OUT AS DIGITS; think more about this
    output[n,4] <- area
    n = n + 1
  }
  
  column_names <- c("n", "shift", "last_date", "A(n)")
  auc_cq <- data.frame(output)
  colnames(auc_cq) <- column_names
  
  return(auc_cq)
}

#testing the above function 
auc_cq_function(50, DJI, 4, "2007-01-03", 1000, 1)




#---------------------------modulating the below, above ^^^^^----------------
rows = 5
variables = 2
n <- 1
output <- matrix(ncol=variables, nrow=rows)

for(l in seq(from=0, to=4, by=1)){
  
  area = AUC(DJI, 4, (as.Date("2007-01-03") + l), 1000, 1)  #need to change SP_increments to the table outputted by measure_data
  output[n,1] <- n
  output[n,2] <- area
  n = n + 1
}

column_names <- c("n", "area")
auc_cq <- data.frame(output)
colnames(auc_cq) <- column_names

auc_cq
#----------------------------------------------------------------------------

mavback <- function(x,n){
  a<-mav(x,1)
  b<-mav(x,(n+1))
  c<-(1/n)*((n+1)*b - a)
  return(c)
}

average_table <- auc_cq_function(4, DJI, 4, "2007-01-03", 1000, 1)

average_table[1, 4]

# loop calculating rolling average from the previous terms in 

rollmeanr(average_table[,4],3,fill=NA)

average_table[,4]
