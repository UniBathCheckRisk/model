# installing and loading in packages
install.packages('quantmod')
install.packages('TSstudio')
install.packages('SciViews')
library(quantmod)
library(dplyr)
library(TSstudio)
library(SciViews)
library(ggplot2)

#loading in S&P 500 data (titled GSPC) from Yahoo; other indexes can be loaded into the vector
getSymbols("^GSPC", from = as.Date("1929-01-01"),to = "2022-07-01", src = "yahoo")
getSymbols("^DJI", from = as.Date("1987-01-01"),to = "2022-07-01", src = "yahoo")
saveSymbolLookup(file = "indexes.rda")  #saving data to the file 'indexes.rda'

# checking the class of data GSPC ('xts' and 'zoo')  
class(GSPC)

#write.zoo(^GSPC, file="SPindex.csv", sep=",") #no 
#setSymbolLookup('^GSPC'=list(src="csv",format="%Y-%m-%d")) #no
#GSPC <- getSymbols("^GSPC", auto.assign=FALSE) #no

View(GSPC)  #outputting table GSPC

# Outputting line and bar chart from whole GSPC data set, barChart is a quantmod function
barChart(GSPC)
View(DJI)


#-----------------IMPORTING the indexes------------------------------------------------------

#Getting indices (from 1970's)

getSymbols(c("^HSI", "^FTSE", "BTC-USD", "GC=F", "GLD"), from = "1970-01-01",
to = "2022-07-01")

View(`BTC-USD`)

#function to convert the index to a dataframe 
dataframe_index <- function(output_index){
  data.frame(date=index(output_index), coredata(output_index))
}

FTSE_df <- dataframe_index(FTSE)
FTSE_df %>% head()


GLD_df <- dataframe_index(GLD)
GLD_df %>% head()

BTC_df <- dataframe_index(`BTC-USD`)
BTC_df %>% head()
#View(BTC_df)
#`BTC-USD`

HSI_df <- dataframe_index(HSI)
HSI_df %>% head()
View(FTSE)

#getting the DJ data from 1000 days before the start of 1987
#getSymbols(c("^DJI"), from = as.Date("1987-01-01"),
#           to = "2017-01-15")
#WE HAVE NO DJI FROM YAHOO FROM BEFORE 1992

DJI_df <- dataframe_index(DJI)
DJI_df %>% head()

GSPC_df <- dataframe_index(GSPC)
GSPC_df %>% nrow()
GSPC_df <- GSPC_df %>% rename(SP_dates = date)

#importing excel with the historical dates (S&P AND DJI FROM 1929)
library(readxl)
SPX_1929_2018_2_imported_into_R <- read_excel("H:/project_checkrisk/SPX 1929_2018(2)-imported into R.xlsx", 
                                              col_types = c("date", "numeric", "date", 
                                                            "numeric"))
View(SPX_1929_2018_2_imported_into_R)

historic_SP_DJI_df <- data.frame(SPX_1929_2018_2_imported_into_R) 

historic_SP_DJI_df <- historic_SP_DJI_df %>%
  select("SP_dates" = "Dates...1", "GSPC.Close" = "PX_LAST.S.P.500",
         "date" = "Dates...3", "DJI.Close" = "PX_LAST.DJI")

historic_SP_DJI_df %>% head()

historic_SP_DJI_df %>% arrange(desc(SP_dates)) %>% head()

#HISTORIC DJI data
historic_DJI <- historic_SP_DJI_df %>% 
  select(date, DJI.Close) %>%
  filter(date < "1992-01-02") %>% 
  arrange(date)

#HISTORIC SP DATA- which we don't need as its already loaded in through yahoo
historic_SP <- historic_SP_DJI_df %>% select(SP_dates, GSPC.Close)

#joining the historic DJI to the currect DJI loaded through yahoo
hd <- historic_DJI
cd <- DJI_df %>% select(date, DJI.Close)
whole_historic_DJI <- rbind(hd, cd)


#THE HISTORIC dataset for DJI (from 1929) up until the day loaded in through yahoo
# !!! the FIRST column is needed to fetch the closing prices of this data !!!
#converting df to xts to then be put in the measure function
DJI_whole <- xts(whole_historic_DJI[,2], order.by = whole_historic_DJI[,1]) 

DJI_whole %>% View()

#HISTORIC SP DATA - help here please. Error [match.names(clabs, names(xi), names do not match previous names)]
historic_SP <- historic_SP_DJI_df %>% 
  select(SP_dates, GSPC.Close) %>%
  filter(SP_dates < "1992-01-02") %>% 
  arrange(SP_dates)
hd_SP <- historic_SP
cd_SP <- GSPC_df %>% select(SP_dates, GSPC.Close)
whole_historic_SP <- rbind(hd_SP, cd_SP)
SP_whole <- xts(whole_historic_SP[,2], order.by = whole_historic_SP[,1])

SP_whole %>% View()
SP_whole[,1] %>% class()

cd_SP %>% head()
hd_SP %>% head()

#renaming for the 2nd time 
GSPC_df <- GSPC_df %>% rename(date= SP_dates )


#---------------------------------------------------------------------------------------------

#Extracting 1st (time variable) column and "GSPC.Close" column from the GSPC data set and assigning to SandP_data
#GSPC.Close column I assume represents the closing prices
SandP_data <- GSPC[, "GSPC.Close"]
DowJones <- DJI[, "DJI.Close"]

View(SandP_data)

# Outputting line chart of SandP_data
barChart(SandP_data)


# Plotting time series of SandP & DJ data 
ts_plot(SandP_data,
        title = "S & P 500 Time series [source: Yahoo Finance]",
        Xtitle = "Time (using daily data)",
        Ytitle = "Closing price"
)

ts_plot(DowJones,
        title = "Dow Jones Time series [source: Yahoo Finance]",
        Xtitle = "Time (using daily data)",
        Ytitle = "Closing price"
)

# Function for extracting the closing price, time variables from any index, when the closing price column is 
# the nth column
time_series_data <- function(index_name, n){
  index_name[, n]
}


#--------------------------------------------------------------------------------------------
# beginning with AVR method; determining the increments (1's will be changed to T's in the function) (creating miew)
# doing with N = 20 
# Output table SP_increments which gets the measure miew based off of a time series of N elements, t=1,2,.,N(t=NUMBER OF ROWS)
# and T, the interval size, here we use T=1 (put T, N in the arguments of the function)- delete unneeded columns
# modulating: #TO DO WHEN FILTERING FOR CRASHES: make N into a filter of a time series between dates
#Inputs: stock market index to be used, index of the closing price column, N, T interval, start date of time series,
# period of time over which to analyse (in days), these last 2 variables to be added
#Output: measure column, miew, start_date, period
measure_data <- function(index_name, closing_column, starting_date, N, interval_T){
  
  output_index <-index_name[, closing_column]
  
  index_dataset <- data.frame(date=index(output_index), coredata(output_index))
  
  # the head(N+interval_T) will be changed appropriately to accommodate for date filter and periods over which
  # the time series needs to be analysed 
  
  index_to_join <- index_dataset %>% 
    mutate(date = as.Date(date)) %>%
    filter(date >= as.Date(starting_date)) %>%   #CHANGING THIS !!! FROM VERSION 3.5 TO 3.6, removing as.Dates here and below
    arrange(date) %>%
    head(N+interval_T) %>% 
    mutate(t_incremented = row_number())
  
  index_sub_dataset <- index_dataset %>% 
    mutate(date = as.Date(date)) %>% 
    filter(date >= (starting_date)) %>%
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
    mutate(increments     = .[[6]] - .[[2]]) %>%           #this is equivalent to DJI.Close.y - DJI.Close.x
    mutate(abs_increments = abs(increments))
  
  
  total_increments <- SP_increments %>% select(abs_increments) %>% sum()
  
  #SP_increments <- SP_increments %>% mutate(measure = abs_increments / total_increments)
  
  #LIMITATION OF AVR METHOD FOUND; when there is no price change, the measure is 0 which when raised to negative 
  #powers of q, it gives out infinity
  SP_increments <- SP_increments %>% 
    mutate(abs_increments = ifelse(abs_increments == 0, 0.005, abs_increments)) %>% 
    mutate(measure = abs_increments / total_increments)
  
  return(SP_increments)  #whole table returned but only column 'measure' is needed
}

#utilizing above function with N=20, T= 1 + the other input parameters 
#measure_data(GSPC, 4, "2007-08-09", 20, 1)

measure_data(DJI, 4, as.Date("2008-09-29")-1100, 1000, 1) %>% arrange(date.x) %>% head()

#---------------------------------------------------------------------------------------------------------

# Function which outputs the Z (partition function value) values with corresponding q values (moments)
# Number of iterations depend on the range of values for q and the increment value; maybe parameterize the
# increments and the range 

partition_function_data <- function(index_name, closing_column, starting_date, N, interval_T){
  
  SP_increments <- measure_data(index_name, closing_column, starting_date, N, interval_T)
  
  iterations = 306   #427 when from -7.033 to 7.033 and 306 when from -5.033 to 5.033
  variables = 3
  i <- 1
  output <- matrix(ncol=variables, nrow=iterations)
  
  # Creating empty data frame to populate the sums for the different values of q
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

partition_function_data(DJI, 4, "2007-01-03", 20, 1) #%>% head(6)

#------------------------------------------------------------------------------------------------------
# setting up data for specific heat curve
# N = number of time series elements (number of rows)
# Outputting the table with 3 columns; iteration number, q and Cq
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

#A few checks 
specific_heat_function(DJI, 4, "2007-01-03", 20, 1) %>% head()

specific_heat_function(DJI, 4, "2007-01-03", 20, 1) %>% nrow()

specific_heat_function(DJI, 4, "2007-01-03", 20, 1)

specific_heat_function(DJI, 4, as.Date("2008-09-29")-1100, 1000, 1) 

ggplot(specific_heat_function(DJI, 4, as.Date("2008-09-29")-1000, 1001, 1)) +
  geom_line(aes(x = q, y= Cq))


#OUTPUTS---------------------------------------------------------------------------------------------
#specific heat replicating canessa on S&P dataset; removing black Monday; the 1972th point after 1980

#(remove black Monday) excl. #1971st point starting from 1980-01-01, from GSPC dataset
SP_missing_point <- GSPC_df %>% 
  filter(date >= "1980-01-01") %>% filter(row_number() != 1972)

GSPC_df %>% head()

#this is the dataset to evaluate specific heat on, with closing column 1
GSPC_missing_p <- xts(SP_missing_point[,5], order.by = SP_missing_point[,1]) #checked 

View(GSPC_missing_p)

SP_missing_point %>% nrow()
GSPC_df %>% filter(date >= "1980-01-01") %>% nrow()
GSPC_df %>% 
  filter(date >= "1980-01-01") %>% filter(row_number() == 1972)

#saving the functions which include different data points
SP_1980_499 <- specific_heat_function(GSPC, 4, "1980-01-01", 498, 1) #first 499 points (no crash)
SP_1980_3287 <- specific_heat_function(GSPC, 4, "1980-01-01", 3286, 1) #all points incl crash and after
SP_1980_1971 <- specific_heat_function(GSPC, 4, "1980-01-01", 1970, 1) #points up to excl crash

#CHECK THIS!!!!!!!!!!!!---lower green lobe when 1972 output and higher green lobe when 1971 output. 1972 is Black Mon
SP_1980_1969 <- specific_heat_function(GSPC_missing_p, 1, "1980-01-01", 3285, 1) #all points excl crash

#setting up the plot and saving as sp1980_p1
SP1980_p1 <- ggplot(data = SP_1980_499, color = "black", aes(x = q, y= Cq)) +
  geom_line() + geom_point(data = SP_1980_499, color = "black") +
  geom_line(data= SP_1980_3287, color = "red") + 
  geom_point(data= SP_1980_3287, color = "red") +
  geom_line(data= SP_1980_1971, color = "blue") + 
  geom_point(data= SP_1980_1971, color = "blue") +
  geom_line(data= SP_1980_1969, color = "green") + 
  geom_point(data= SP_1980_1969, color = "green") +
  ggtitle("Specific Heat Curve for S&P500 1980-1992, black = 499 pts, red = 3287 pts, blue = 1971 pts,
          green = 3286 points, removing crash date point 1972, 1987-10-19")

SP1980_p1 #outputting the plot




#----------------------------------------------------------------------------------------------

# integrating under the specific heat curve via the trapezium rule
AUC <- function(index_name, closing_column, starting_date, N, interval_T){
  
  cq <- specific_heat_function(index_name, closing_column, starting_date, N, interval_T)
  
  cq_to_join <- cq %>%
    mutate(iteration = iteration - 1)
  #joining on the next value of Cq to each value of Cq in order to find the area of each trapezium in between 
  #consecutive values of Cq, separated by the increment value set in the partition function above (0.033)
  auc_cq <- cq %>%                                  
    left_join(cq_to_join,
              by = "iteration",
              suffix = c(".a", ".b")) %>% 
    mutate(area = 0.5*(Cq.a + Cq.b)*0.033)%>%
    select(area) %>%
    sum(na.rm = TRUE)
  
  return(as.numeric(auc_cq))
}

# Area under the specific heat curve for the DJI index, on a window size 20 and increment value 1 (price values 
# taken each day- financial day- as they are published) 
AUC(DJI, 4, "2007-01-03", 20, 1)

#------------------DOUBLE CHECKING AUC---------------------IT SEEMS CORRECT--------------

cq <- specific_heat_function(DJI, 4, "2007-01-03", 20, 1)
cq

cq_to_join <- cq %>%
  mutate(iteration = iteration - 1)
#joining on the next value of Cq to each value of Cq in order to find the area of each trapezium in between 
#consecutive values of Cq, separated by the increment value set in the partition function above
auc_cq <- cq %>%                                  
  left_join(cq_to_join,
            by = "iteration",
            suffix = c(".a", ".b")) %>% 
  mutate(area = 0.5*(Cq.a + Cq.b)*0.033)#%>%
#select(area) %>%
#sum(na.rm = TRUE)

auc_cq %>% head()

auc_cq %>% arrange(desc(iteration)) %>% head()

cq_to_join %>% head()

cq_to_join %>% arrange(desc(iteration)) %>% head()

#---------------------------TESTS------------------------------------------------------

measure_data(DJI, 4, "2007-01-03", 20, 1)
partition_function_data(DJI, 4, "2007-01-03", 21, 1) %>% head()
specific_heat_function(DJI, 4, "2007-01-03", 21, 1) %>% head()

#area under curve for N=20 points starting from 2007-01-03, this is the 1st index; n=1 
AUC(DJI, 4, "2007-01-03", 1000, 1)

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
DJI_df %>% filter(date >= as.Date("2007-01-03") + 1) %>% arrange(date) %>% head()

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

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------

# function with loop for area under the curve of N series elements shifting along a time series 
auc_cq_function <- function(total_shifts, index_name, closing_column, starting_date, N, interval_T){
  
  shifts = total_shifts + 1
  variables = 4
  n <- 1
  output <- matrix(ncol=variables, nrow=shifts)
  
  for(l in seq(from=0, to=total_shifts, by=1)){
    
    area = AUC(index_name, closing_column, (as.Date(starting_date) + l), N, interval_T) 
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

# function outputting a table which includes the 'eta' as seen on fondseca 
dynamic_eta <- function(total_shifts, index_name, closing_column, starting_date, N, interval_T){
  
  auc_cq_table <- auc_cq_function(total_shifts, index_name, closing_column, starting_date, N, interval_T)
  
  av_table <- auc_cq_table %>% mutate(average_An = NA)
  
  for(i in seq(from = 1, to = total_shifts + 1, by = 1)){ #ADDED + 1
    
    average_an <- (sum(av_table[1:i-1,4]))/(i-1) 
    
    av_table[i,5] <- average_an
  }
  
  av_table <- av_table %>% mutate(eta = abs((av_table[,4] / average_An) - 1))
  
  return(av_table)
}

dynamic_eta(100, DJI, 4, "2007-01-03", 1000, 1) %>% head()

#plotting the AVR (eta against n)
ggplot(dynamic_eta(100, DJI, 4, "2007-01-03", 1000, 1)) +
  geom_point(aes(x = n, y= eta))

# running AVR on the 2008 crash
dynamic_eta(100, DJI, 4, as.Date("2008-09-29")-1100, 1000, 1)

ggplot(dynamic_eta(100, DJI, 4, as.Date("2008-09-29")-1100, 1000, 1)) +
  geom_point(aes(x = n, y= eta))

# 'AVR plot over extended time period1' (saved as this)
#ggplot(dynamic_eta(1000, DJI, 4, as.Date("2008-09-29")-1100, 1000, 1)) +
#  geom_point(aes(x = n, y= eta))


#-----------TO DO; MAKE SURE ALL FUNCTIONS INCLUDE A RETURN STATEMENT


#function to the get raw index data with times and closing prices 
raw_data <- function(index_name, closing_column, starting_date, N, total_shifts){
  
  output_index <-index_name[, closing_column]
  
  index_dataset <- data.frame(date=index(output_index), coredata(output_index))
  
  index_to_join <- index_dataset %>% 
    mutate(date = as.Date(date)) %>%
    filter(date >= as.Date(starting_date)) %>%
    arrange(date) %>%
    head(N+total_shifts) %>% #get rid of this as this needs to be the whole dataset onwards from the starting date
    #BUT could crunch the data though, so that it encompases the start date plus the number of days needed
    #this c=would be the start date + N + total shift (do this in the head() calculation)
    mutate(day_number = row_number())
  
  return(index_to_join)
}


# -----------------ADDING the date to identify the time of the AVR jumps--------------

#function adding a 'start date' (when the window began) and an 'end_date' (the end of the window)
AVR_with_time <- function(total_shifts, index_name, closing_column, starting_date, N, interval_T){
  
  eta <- dynamic_eta(total_shifts, index_name, closing_column, starting_date, N, interval_T) %>% 
    mutate(end_day = shift + (N + interval_T)) 
  
  time_series_dates <-  raw_data(index_name, closing_column, starting_date, N, total_shifts)
  
  timed_AVR <- eta %>% 
    left_join(
      time_series_dates %>%
        select("n" = "day_number" , "start_date" = "date"), 
      by = "n"
    ) %>%
    left_join(
      time_series_dates %>%
        select("end_day" = "day_number" , "end_date" = "date"),
      by = "end_day"
    ) %>%
    filter(!is.na(end_date))
  
  return(timed_AVR)
}

# Outputting the above function
AVR_with_time(7, DJI, 4, "2007-01-03", 1000, 1)


#------------- A LOOK BACK, say we want to look back 1000 data points since the 2008 crash (2008-09-29)
#Used to get the precise date to start off with when evaluating for e.g. AVR on 1000 data points before a crash

lookback_date <- function(index_name, closing_column, crash_date, lookback){
  
  output_index <- index_name[, closing_column]
  
  index_dataset <- data.frame(date=index(output_index), coredata(output_index))
  
  lookback_data <- index_dataset %>%
    filter(date <= crash_date) %>%
    arrange(desc(date)) %>%
    head(lookback)
  
  lookback_date <- lookback_data %>% arrange(date) %>% head(1)
  
  lookback_date <- lookback_date[1,1]
  
  return(lookback_date)
}

#Finding the date of the point of 1000 data points before 2008-09-29
lookback_date(DJI, 4, "2008-09-29", 1000)

#The above is different from taking 1000 days less than 2008-09-29 as not all days are included in the financial 
#index
as.Date("2008-09-29")-1000


#-----------TO DO; MAKE SURE ALL FUNCTIONS INCLUDE A RETURN STATEMENT

#Now, plotting AVR and checking the time for 2008 crash

crash_2008 <- dynamic_eta(200, DJI, 4, as.Date("2003-12-23"), 1000, 1)

crash_2008 %>% ggplot(aes(x = n, y= eta)) +
  geom_point() + ggtitle("2008 crash, 200 shifts with window = 1000 from 2003-12-23 to 2008-09-29,
                         jump when point 18th july 2008 included") +
  geom_line()

View(AVR_with_time(200, DJI, 4, as.Date("2003-12-23"), 1000, 1))

lookback_date(DJI, 4, "2008-09-29", 1200)

# TESTING the blank plots 
#filter data input into the plot so that there is cut off at last data point available;---add this to the AVR time 
#function above and from now on use the AVR_times function to plot the AVR 
AVR_cut <- AVR_with_time(16, DJI, 4, as.Date("2022-03-02"), 4, 1) %>%
  filter(!is.na(end_date))

ggplot(AVR_cut, aes(x = n, y= eta)) +
  geom_point() + geom_line()

AVR_cut %>% View()

#extended time period; 
#AVR_extended2 <- AVR_with_time(6614, DJI, 4, as.Date("1992-01-02"), 1000, 1) 
#AVR_extended2 %>% View()

#ggplot(AVR_extended2,  aes(x = n, y= eta)) +
#  geom_point() + geom_line()

#n_200 <- AVR_extended2 %>% filter(n <= 200)

#n_200 %>% ggplot(aes(x = n, y= eta)) +
#  geom_point() + geom_line() + ggtitle("extended time period AVR from 1992 to 2022, taking the first 200 shifts,
#                                     window size N= 1000, end dates 1995 to 1996 included")

#n_200 %>% View()


#write.csv(AVR_extended,"H:\\project_checkrisk\\AVR1.csv", row.names = TRUE)


#class(AVR_extended)



#-------------------------------------------------------
DJI_df %>% head()
DJI_df %>%nrow()

measure_data(DJI, 4, "1992-01-02", 1000, 1)
partition_function_data(DJI, 4, "1992-01-02", 1000, 1)


#REPLICATING canessa plots--------
#specific heat curves;

DJI_sp<- specific_heat_function(DJI, 4, "1980-01-01", 499, 1) 
ggplot(DJI_sp, aes(x = q, y= Cq)) +
  geom_point() + geom_line()

## changing measure function;DONE IT 
SP_table <- measure_data(DJI, 4, "2007-01-01", 20, 1)

SP_table %>% mutate(trying = .[[6]] - .[[2]]) %>% 
  mutate(col = ifelse(trying == increments, "yes", "no"))

#------------
# replicating Fonseca AVR overlay plots with DJI------------------------------

lookback_date(DJI_whole, 1, "2020-04-24", 1130)

DJI_87 <- AVR_with_time(130, DJI_whole, 1, as.Date("1983-09-07"), 1000, 1)
DJI_08 <- AVR_with_time(130, DJI_whole, 1, as.Date("2004-07-15"), 1000, 1)
DJI_11 <- AVR_with_time(130, DJI_whole, 1, as.Date("2007-05-09"), 1000, 1)
DJI_T60 <- AVR_with_time(130, DJI_whole, 1, as.Date("1955-12-30"), 1000, 1)
DJI_29 <- AVR_with_time(130, DJI_whole, 1, as.Date("1925-11-20"), 1000, 1)
DJI_20 <- AVR_with_time(130, DJI_whole, 1, as.Date("2016-02-11"), 1000, 1)

#setting up the overlay plot
AVRDJI <- ggplot(data = DJI_87, color = "green", aes(x = n, y = eta)) +
  geom_line() + geom_point(data = DJI_87, color = "green") +
  geom_line(data = DJI_08, color = "blue") +
  geom_point(data = DJI_08, color = "blue") +
  geom_line(data = DJI_11, color = "pink") +
  geom_point(data = DJI_11, color = "pink") +
  geom_line(data = DJI_T60, color = "black") +
  geom_point(data = DJI_T60, color = "black") +
  geom_line(data = DJI_29, color = "red") +
  geom_point(data = DJI_29, color = "red") +
  geom_line(data = DJI_20, color = "orange") +
  geom_point(data = DJI_20, color = "orange") +
  ggtitle("AVR Overlay DJI replicating Fonseca, red=1929, green=1987, blue=2008, pink=2011, orange=2020, test(1960)=black")

AVRDJI #outputting the plot

#output AVR overlay with S&P-----------------------------------------------
lookback_date(SP_whole, 1, "2020-02-20", 1020) 

SP_87 <- AVR_with_time(130, SP_whole, 1, as.Date("1985-09-26"), 1000, 1) 
SP_08 <- AVR_with_time(130, SP_whole, 1, as.Date("2004-07-15"), 1000, 1)
SP_11 <- AVR_with_time(130, SP_whole, 1, as.Date("2007-05-09"), 1000, 1)
SP_T60 <- AVR_with_time(130, SP_whole, 1, as.Date("1955-12-30"), 1000, 1)
SP_29 <- AVR_with_time(130, SP_whole, 1, as.Date("1926-12-06"), 1000, 1)
SP_20 <- AVR_with_time(130, SP_whole, 1, as.Date("2016-02-02"), 1000, 1)

#setting up the overlay plot
AVRSP <- ggplot(data = SP_87, color = "green", aes(x = n, y = eta)) +
  geom_line() + geom_point(data = SP_87, color = "green") +
  geom_line(data = SP_08, color = "blue") +
  geom_point(data = SP_08, color = "blue") +
  geom_line(data = SP_11, color = "pink") +
  geom_point(data = SP_11, color = "pink") +
  geom_line(data = SP_T60, color = "black") +
  geom_point(data = SP_T60, color = "black") +
  geom_line(data = SP_29, color = "red") +
  geom_point(data = SP_29, color = "red") +
  geom_line(data = SP_20, color = "orange") +
  geom_point(data = SP_20, color = "orange") +
  ggtitle("AVR Overlay SPY replicating Fonseca, red=1929, green=1987, blue=2008, pink=2011, orange=2020, test(1960)=black")

AVRSP #outputting the plot 



#output AVR overlay Covid crash on all markets----------------------------------

lookback_date(SP_whole, 1, "2020-02-20", 1025)
lookback_date(DJI_whole, 1, "2020-02-20", 1025) 
lookback_date(HSI, 4, "2020-02-20", 1025) 
lookback_date(GLD, 4, "2020-02-20", 1025) 
lookback_date(FTSE, 4, "2020-02-20", 1025) 
lookback_date(`BTC-USD`, 4, "2020-02-20", 1025)

SPCoV <- AVR_with_time(50, SP_whole, 1, as.Date("2016-01-26"), 1000, 1) 
DJICoV <- AVR_with_time(50, DJI_whole, 1, as.Date("2016-01-26"), 1000, 1)
HSICoV <- AVR_with_time(50, HSI, 1, as.Date("2015-12-24"), 1000, 1)
GLDCoV <- AVR_with_time(50, GLD, 1, as.Date("2016-01-26"), 1000, 1)
FTSECoV <- AVR_with_time(50, FTSE, 1, as.Date("2016-02-03"), 1000, 1)
`BTC-USDCoV` <- AVR_with_time(50, `BTC-USD`, 1, as.Date("2017-05-02"), 1000, 1)

#setting up the overlay plot
AVRCoV <- ggplot(data = SPCoV, color = "green", aes(x = n, y = eta)) +
  geom_line() + geom_point(data = SPCoV, color = "green") +
  geom_line(data = DJICoV, color = "black") +
  geom_point(data = DJICoV, color = "black") +
  geom_line(data = HSICoV, color = "pink") +
  geom_point(data = HSICoV, color = "pink") +
  geom_line(data = GLDCoV, color = "red") +
  geom_point(data = GLDCoV, color = "red") +
  geom_line(data = FTSECoV, color = "orange") +
  geom_point(data = FTSECoV, color = "orange") +
  geom_line(data = `BTC-USDCoV`, color = "turquoise") +
  geom_point(data = `BTC-USDCoV`, color = "turquoise") +
  ggtitle("AVR Overlay Covid crash, red=GLD, green=SP, black=DJI, orange=FTSE, pink=HSI, `BTC-USDCoV`=turquoise", )

AVRCoV #outputting the plot


#--------------------------------------------------------------------------------
# OUTPUT ---- 1987 crash AVR with DJI and SP changing avr window for jump date accuracy
#assume 1987 crash date is Black Monday 1987-10-19

#attempt 1 - avr window n=100 with crash date centred at n=50

lookback_date(GSPC, 4, "1987-10-19", 1050)

AVR87_1_DJI <- AVR_with_time(100, DJI_whole, 1, as.Date("1983-08-27"), 1000, 1)
AVR87_1_GSPC <- AVR_with_time(100, GSPC, 4, as.Date("1983-08-24"), 1000, 1)

AVR87_1 <- ggplot(data = AVR87_1_DJI, color = "green", aes(x = n, y = eta)) +
  geom_line() + geom_point(data = AVR87_1_DJI, color = "green") +
  geom_line(data = AVR87_1_GSPC, color = "blue") +
  geom_point(data = AVR87_1_GSPC, color = "blue") +
  ggtitle("AVR 1987 crash, n=100 centred at BM, green=DJI, blue=S&P")

AVR87_1 #output plot

#attempt 2 - avr window n=200 with crash date centred at n=100

lookback_date(DJI_whole, 1, "1987-10-19", 1100)

AVR87_2_DJI <- AVR_with_time(200, DJI_whole, 1, as.Date("1983-06-13"), 1000, 1)
AVR87_2_GSPC <- AVR_with_time(200, GSPC, 4, as.Date("1983-06-14"), 1000, 1)

AVR87_2 <- ggplot(data = AVR87_2_DJI, color = "green", aes(x = n, y = eta)) +
  geom_line() + geom_point(data = AVR87_2_DJI, color = "green") +
  geom_line(data = AVR87_2_GSPC, color = "blue") +
  geom_point(data = AVR87_2_GSPC, color = "blue") +
  ggtitle("AVR 1987 crash, n=200 centred at BM, green=DJI, blue=S&P")

AVR87_2 #output plot

#attempt 3 - avr window n=50 with crash date centred at n=25

lookback_date(GSPC, 4, "1987-10-19", 1025)

AVR87_3_DJI <- AVR_with_time(50, DJI_whole, 1, as.Date("1983-09-28"), 1000, 1)
AVR87_3_GSPC <- AVR_with_time(50, GSPC, 4, as.Date("1983-09-29"), 1000, 1)

AVR87_3 <- ggplot(data = AVR87_3_DJI, color = "green", aes(x = n, y = eta)) +
  geom_line() + geom_point(data = AVR87_3_DJI, color = "green") +
  geom_line(data = AVR87_3_GSPC, color = "blue") +
  geom_point(data = AVR87_3_GSPC, color = "blue") +
  ggtitle("AVR 1987 crash, n=50 centred at BM, green=DJI, blue=S&P")

AVR87_3 #output plot

#attempt 4 - avr window n=20 with crash date at end of window

lookback_date(GSPC, 4, "1987-10-19", 1025)

AVR87_4_DJI <- AVR_with_time(25, DJI_whole, 1, as.Date("1983-09-28"), 1000, 1)
AVR87_4_GSPC <- AVR_with_time(25, GSPC, 4, as.Date("1983-09-29"), 1000, 1)

AVR87_4 <- ggplot(data = AVR87_4_DJI, color = "green", aes(x = n, y = eta)) +
  geom_line() + geom_point(data = AVR87_4_DJI, color = "green") +
  geom_line(data = AVR87_4_GSPC, color = "blue") +
  geom_point(data = AVR87_4_GSPC, color = "blue") +
  ggtitle("AVR 1987 crash, n=20 BM at end, green=DJI, blue=S&P")

AVR87_4 #output plot
#-------------------------------use small n window for accuracy ----------------------


### changing n=25 date for AVR plot on 1987 crash

lookback_date(DJI_whole, 1, "1987-09-29", 1025)

AVR19 <- AVR_with_time(25, DJI_whole, 1, as.Date("1983-09-28"), 1000, 1)
AVR15 <- AVR_with_time(25, DJI_whole, 1, as.Date("1983-09-26"), 1000, 1)
AVR13 <- AVR_with_time(25, DJI_whole, 1, as.Date("1983-09-22"), 1000, 1)
AVR9 <- AVR_with_time(25, DJI_whole, 1, as.Date("1983-09-20"), 1000, 1)
AVR7 <- AVR_with_time(25, DJI_whole, 1, as.Date("1983-09-16"), 1000, 1)
AVR5 <- AVR_with_time(25, DJI_whole, 1, as.Date("1983-09-14"), 1000, 1)
AVR1 <- AVR_with_time(25, DJI_whole, 1, as.Date("1983-09-12"), 1000, 1)
AVR29 <- AVR_with_time(25, DJI_whole, 1, as.Date("1983-09-08"), 1000, 1)

AVRshift <- ggplot(data = AVR19, color = "green", aes(x = n, y = eta)) +
  geom_line() + geom_point(data = AVR19, color = "green") +
  geom_line(data = AVR7, color = "blue") +
  geom_point(data = AVR7, color = "blue") +
  geom_line(data = AVR15, color = "pink") +
  geom_point(data = AVR15, color = "pink") +
  geom_line(data = AVR5, color = "black") +
  geom_point(data = AVR5, color = "black") +
  geom_line(data = AVR13, color = "red") +
  geom_point(data = AVR13, color = "red") +
  geom_line(data = AVR1, color = "orange") +
  geom_point(data = AVR1, color = "orange") +
  geom_line(data = AVR9, color = "turquoise") +
  geom_point(data = AVR9, color = "turquoise") +
  geom_line(data = AVR29, color = "grey") +
  geom_point(data = AVR29, color = "grey") +
  ggtitle("AVR 1987 crash, moving n-25 date from 19/10 to 29/09")

AVRshift #output plot


#------------
#attempt to add in financial raw data to background
#
#lookback_date(DJI_whole, 1, "1987-10-19", 24)

#library(patchwork)

#AVR87_5_DJI <- AVR_with_time(50, DJI_whole, 1, as.Date("1983-09-28"), 1000, 1)
#AVR87_5_GSPC <- AVR_with_time(50, GSPC, 4, as.Date("1983-09-29"), 1000, 1)

#AVR87_5 <- ggplot(data = AVR87_5_DJI, color = "green", aes(x = end_date, y = eta)) +
#  geom_line() + geom_point(data = AVR87_5_DJI, color = "green") 
#  ggtitle("AVR 1987 crash, n=50 centred at BM, green=DJI, blue=raw")

#raw87 <- raw_data(DJI_whole, 1, as.Date("1987-09-14"), 49, 1)
#View(raw87)

#AVRraw <- ggplot(data = raw87, color = "black", aes(x = date, y = coredata.output_index.)) +
#  geom_line()

#AVRraw
#AVR87_5
#AVR87_5 + AVRraw #output plot


#geom_line(data = raw87, color = "blue") +
#  geom_point(data = raw87, color = "blue") +

