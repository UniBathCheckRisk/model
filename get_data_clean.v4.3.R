# installing and loading in packages
install.packages('quantmod')
install.packages('TSstudio')
install.packages('SciViews')
library(quantmod)
library(dplyr)
library(TSstudio)
library(SciViews)
library(ggplot2)

install.packages('patchwork')
library(patchwork)



#loading in S&P 500 data (titled GSPC) from Yahoo; other indexes can be loaded into the vector
getSymbols("^GSPC", from = as.Date("1992-01-01"),to = "2022-07-01", src = "yahoo")
getSymbols("^DJI", from = as.Date("1987-01-01"),to = "2022-07-01", src = "yahoo")
saveSymbolLookup(file = "indexes.rda")  #saving data to the file 'indexes.rda'

# checking the class of data GSPC ('xts' and 'zoo')  
class(GSPC)

#write.zoo(^GSPC, file="SPindex.csv", sep=",") #no 
#setSymbolLookup('^GSPC'=list(src="csv",format="%Y-%m-%d")) #no
#GSPC <- getSymbols("^GSPC", auto.assign=FALSE) #no

#View(GSPC)  #outputting table GSPC

# Outputting line and bar chart from whole GSPC data set, barChart is a quantmod function
barChart(GSPC)
#View(DJI)


#-----------------IMPORTING the indexes------------------------------------------------------

#Getting indices (from 1970's)

getSymbols(c("^HSI", "^FTSE", "BTC-USD", "GC=F", "GLD"), from = "1970-01-01",
           to = "2022-07-01")

#View(`BTC-USD`)

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
#View(FTSE)

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
#View(SPX_1929_2018_2_imported_into_R)

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
#historic_SP <- historic_SP_DJI_df %>% select(SP_dates, GSPC.Close)

#joining the historic DJI to the currect DJI loaded through yahoo
hd <- historic_DJI
cd <- DJI_df %>% select(date, DJI.Close)
whole_historic_DJI <- rbind(hd, cd)


#THE HISTORIC dataset for DJI (from 1929) up until the day loaded in through yahoo
# !!! the FIRST column is needed to fetch the closing prices of this data !!!
#converting df to xts to then be put in the measure function
DJI_whole <- xts(whole_historic_DJI[,2], order.by = whole_historic_DJI[,1]) 

#DJI_whole %>% View()

#HISTORIC SP DATA - help here please. Error [match.names(clabs, names(xi), names do not match previous names)]
historic_SP <- historic_SP_DJI_df %>% 
  select(SP_dates, GSPC.Close) %>%
  filter(SP_dates < "1992-01-02") %>% 
  arrange(SP_dates)
hd_SP <- historic_SP
cd_SP <- GSPC_df %>% select(SP_dates, GSPC.Close)
whole_historic_SP <- rbind(hd_SP, cd_SP)
#whole_historic_SP %>% View()
SP_whole <- xts(whole_historic_SP[,2], order.by = whole_historic_SP[,1])

#SP_whole %>% View()
SP_whole[,1] %>% class()

historic_SP %>% head()

cd_SP %>% head()
hd_SP %>% head()

#renaming for the 2nd time 
GSPC_df <- GSPC_df %>% rename(date= SP_dates )

output_index <-SP_whole[, 1]

index_dataset <- data.frame(date=index(output_index), coredata(output_index))

#View(index_dataset)
#---------------------------------------------------------------------------------------------

#Extracting 1st (time variable) column and "GSPC.Close" column from the GSPC data set and assigning to SandP_data
#GSPC.Close column I assume represents the closing prices
SandP_data <- GSPC[, "GSPC.Close"]
DowJones <- DJI[, "DJI.Close"]

#View(SandP_data)

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

output_index <-HSI[, 4]

index_dataset <- data.frame(date=index(output_index), coredata(output_index)) %>% distinct()

index_dataset %>% filter(date >= "2016-07-29") %>% head() %>% filter(!is.na(.[[2]]))


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
  
  index_dataset <- data.frame(date=index(output_index), coredata(output_index)) %>% 
    distinct() %>%
    filter(!is.na(.[[2]]))                      #!!!!!THESE LAST 2 ROWS ADDED IN FOR V4.1 (NOT IN V4.0)!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
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
#ggplot(specific_heat_function(DJI, 4, "2007-01-03", 20, 1)) +
#  geom_line(aes(x = q, y= Cq))

#A few checks 
specific_heat_function(DJI, 4, "2007-01-03", 20, 1) %>% head()

specific_heat_function(DJI, 4, "2007-01-03", 20, 1) %>% nrow()

specific_heat_function(DJI, 4, "2007-01-03", 20, 1)

specific_heat_function(DJI, 4, as.Date("2008-09-29")-1100, 1000, 1) 

#ggplot(specific_heat_function(DJI, 4, as.Date("2008-09-29")-1000, 1001, 1)) +
#  geom_line(aes(x = q, y= Cq))


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
#ggplot(dynamic_eta(100, DJI, 4, "2007-01-03", 1000, 1)) +
#  geom_point(aes(x = n, y= eta))

# running AVR on the 2008 crash
#dynamic_eta(100, DJI, 4, as.Date("2008-09-29")-1100, 1000, 1)

#ggplot(dynamic_eta(100, DJI, 4, as.Date("2008-09-29")-1100, 1000, 1)) +
#  geom_point(aes(x = n, y= eta))

# 'AVR plot over extended time period1' (saved as this)
#ggplot(dynamic_eta(1000, DJI, 4, as.Date("2008-09-29")-1100, 1000, 1)) +
#  geom_point(aes(x = n, y= eta))


#-----------TO DO; MAKE SURE ALL FUNCTIONS INCLUDE A RETURN STATEMENT


#function to the get raw index data with times and closing prices 
raw_data <- function(index_name, closing_column, starting_date, N, total_shifts){
  
  output_index <-index_name[, closing_column]
  
  index_dataset <- data.frame(date=index(output_index), coredata(output_index)) %>% distinct()
  
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
  
  index_dataset <- data.frame(date=index(output_index), coredata(output_index)) %>% distinct()
  
  lookback_data <- index_dataset %>%
    mutate(date = as.Date(date)) %>%
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

#crash_2008 %>% ggplot(aes(x = n, y= eta)) +
#  geom_point() + ggtitle("2008 crash, 200 shifts with window = 1000 from 2003-12-23 to 2008-09-29,
#                         jump when point 18th july 2008 included") +
#  geom_line()

AVR_with_time(200, DJI, 4, as.Date("2003-12-23"), 1000, 1)

# TESTING the blank plots 
#filter data input into the plot so that there is cut off at last data point available;---add this to the AVR time 
#function above and from now on use the AVR_times function to plot the AVR 
AVR_cut <- AVR_with_time(16, DJI, 4, as.Date("2022-03-02"), 4, 1) %>%
  filter(!is.na(end_date))

#ggplot(AVR_cut, aes(x = n, y= eta)) +
#  geom_point() + geom_line()

#AVR_cut %>% View()

#-------------------------------------------------------
DJI_df %>% head()
DJI_df %>%nrow()

measure_data(DJI, 4, "1992-01-02", 1000, 1)
partition_function_data(DJI, 4, "1992-01-02", 1000, 1)


#REPLICATING canessa plots--------
#specific heat curves;

#DJI_sp<- specific_heat_function(DJI, 4, "1980-01-01", 499, 1) 
#ggplot(DJI_sp, aes(x = q, y= Cq)) +
#  geom_point() + geom_line()

## changing measure function;DONE IT 
SP_table <- measure_data(DJI, 4, "2007-01-01", 20, 1)

SP_table %>% mutate(trying = .[[6]] - .[[2]]) %>% 
  mutate(col = ifelse(trying == increments, "yes", "no"))

#------------

#testing AVR on HSI index to see if there is an occasion when it will jump
#lookback_date(HSI, 4, "2015-07-08", 1000)

#testHSI <- AVR_with_time(50, HSI, 4, "2011-06-20", 1000, 1)

#ggplot(data = testHSI, color = "green", aes(x = n, y = eta)) +
#  geom_line() + geom_point() +
#  ggtitle("test HSI AVR")


#--------------------------------------------------------------------------------

#NAT ADDING THINGS ON 04/04/22- AFTER 3.8 (EM ADDED THE HISTORICAL DATA IN ANOTHER VERSION SO COPY AND PASTE THIS BIT OVER)



#STATISTICS OUTPUTS-------------------BEGIN----------------------------------------------------------

#bringing in DJI large daily drops & historic crashes (by eye for DJI)
Summary_of_crashes_DJI <- read_excel("H:/project_checkrisk/Summary_of_crashes.xlsx", 
                                     sheet = "DJI large daily drops", 
                                     col_types = c("text", "text", "date", "numeric", "date")) 

DJI_by_eye <- Summary_of_crashes_DJI %>%
  select("worst_day" = "Worst crash day(s)", "crash_day" = "historical crash start Dates") %>%
  data.frame() %>%
  mutate(ts_condition_by_eye = 1)

DJI_by_eye

#bringing in S&P large daily drops & historic crashes (by eye for S&P)
Summary_of_crashes_SP <- read_excel("H:/project_checkrisk/Summary_of_crashes.xlsx", 
                                    sheet = "SP500 large daily drops", 
                                    col_types = c("text", "text", "date", "numeric", "date"))

SP_by_eye <- Summary_of_crashes_SP %>%
  select("worst_day" = "Worst crash day(s)", "crash_day" = "historical crash start Dates") %>%
  data.frame() %>%
  filter(!is.na(crash_day)) %>%
  mutate(ts_condition_by_eye = 1)

crash_dates_by_eye <- SP_by_eye %>% select(crash_day, ts_condition_by_eye)

# creating a table satisfying condition (3) for a crash; identifying price drops which are over 10% over 5 days; if 1 then yes
# if 0 then no, DROP_PERIOD = number of data points over which to calculate the percentage drop, 
# drop = the percentage drop eg 10 = 10% drop
# THIS IS TAKING A CRASH OF 10% BUT ALSO A RISE IN 10% OVER 5 DAYS- DO WE WANT THAT? BCZ AVR WILL STILL PICK IT 
# UP- RELATIVE CHANGES drop period = days over which the price drop happens, drop = the magnitude of the drop (10 % but can change if we want it to)
# LAST INPUT; same as index_name but in quotes!!! e.g. "DJI_whole" or "GSPC"
stat_condition_3 <- function(index_name, closing_column, starting_date, N, total_shifts, drop_period, drop, index_quotes){
  
  # load the raw data in, instert the starting date and the total number of shift with the window (N), N + total
  # shifts equals the number of rows in the time series to be evaluated (N + total_shifts = total rows in ts)
  ts <- raw_data(index_name, closing_column, starting_date, N, total_shifts)  
  ts_day1 <- ts
  ts_day5 <- ts %>%
    mutate(day_number = day_number + (drop_period - 1))  # getting the 5th day to compare to the prices of the firs day
  
  ts_day1 <- ts_day1 %>% 
    left_join(
      ts_day5, by = "day_number", suffix(c(".1",".5"))
    ) %>%
    mutate(price_diff = .[[5]] - .[[2]]) %>%                      #price difference; day_1 - day_5
    mutate(abs_price_diff = abs(price_diff)) %>%
    mutate(percentage_drop = (abs_price_diff / .[[5]])*100,
           percentage_drop_2 = (price_diff / .[[5]])*100) %>%   #[abs(day 1 - day 5) / day 1] * 100
    mutate(condition_3 = ifelse(percentage_drop >= drop, 1, 0),
           info_5_days = ifelse(percentage_drop_2 >= 0, "rise", "crash")) %>%
    select("ts_day1" = "date.y", "ts_day5" = "date.x", "ts_condition_3" = "condition_3", "info_5_days") %>%   # selecting required cols- delete to investigate
    filter(!is.na(ts_day1))       #getting rid of null days (at the top of the table- no info)
  
  # adding condition for crash dates and worst dates of a crash by eye 
  ts_day1 <- ts_day1 %>% 
    left_join(crash_dates_by_eye %>%
                rename(ts_day1 = crash_day),
              by = "ts_day1") %>%
    mutate(ts_condition_by_eye = ifelse(is.na(ts_condition_by_eye), 0, 1))
  
  # adding worse day crashes for S&P or DJI, adding a new input
  if(index_quotes == "DJI_whole"){
    
    ts_day1 <- ts_day1 %>%
      left_join(DJI_by_eye %>%
                  rename(ts_day1 = worst_day, ts_condition_worst = ts_condition_by_eye),
                by = "ts_day1") %>%
      distinct()                           #date in 1929 is repeated 
    
  } else if (index_quotes %in% c("GSPC", "SP_whole") ){
    
    ts_day1 <- ts_day1 %>%
      left_join(SP_by_eye %>%
                  rename(ts_day1 = worst_day, ts_condition_worst = ts_condition_by_eye),
                by = "ts_day1") %>%
      distinct()
    
  } else {
    
    ts_day1 <- ts_day1          #no addition to ts_day1 (we only have worst crash day data for S&P and DJI)
    
  }
  
  # now adding the over 5% daily drop condition
  ts_day0 <- ts
  #adding 5% drops/ rises - rises will count as crashes/something happening in the system too
  ts_next_day <- ts_day0 %>%
    mutate(day_number = day_number + 1)
  
  condition_5_percent <- ts_day0 %>% 
    left_join(
      ts_next_day,
      by = "day_number", 
      suffix = (c(".1",".0"))
    ) %>%
    mutate(price_diff = .[[5]] - .[[2]]) %>%                      #price difference; day_1 - day_5
    mutate(abs_price_diff = abs(price_diff)) %>%
    mutate(percentage_drop = (abs_price_diff / .[[5]])*100) %>%        #[abs(day 1 - day 5) / day 1] * 100
    mutate(ts_condition_2 = ifelse(percentage_drop >= 5, 1, 0)) %>%
    filter(!is.na(ts_condition_2)) %>%
    select("ts_day1" = "date.0", "ts_day2_5p_drop" = "date.1", ts_condition_2)
  
  # now joining ts_day1 and condition_5_percent in order to get all conditions for crash in 1 table 
  
  final_conditions <- ts_day1 %>% 
    mutate(ts_condition_worst = ifelse(is.na(ts_condition_worst), 0, 1)) %>%
    left_join(condition_5_percent,
              by = "ts_day1") %>% 
    mutate(ts_conditions = ifelse(ts_condition_worst == 1 | ts_condition_2 == 1 | ts_condition_3 == 1 | ts_condition_by_eye == 1, 1, 0)) %>%
    select(ts_day1, ts_conditions)   #selecting 2 columns- remove this line for more detail 
  
  return(final_conditions)
}

# CONDITION 3 ON WHOLE DJI TIME SERIES
#running condition 3 on DJI from 1929-01-02 until 2022-03-23, filtering for the dates where there was a 10% drop over 5 days 
# should be 114 rows 
# DONT WORRY AB THE WARNINGS- code still works 
stat_DJI_3 <- stat_condition_3(DJI_whole, 1, "1929-01-02", 1000, 22521, 5, 10, "DJI_whole") #filter(!is.na(ts_condition_worst))
View(stat_DJI_3)
numb_rows <- stat_DJI_3 %>% filter(ts_conditions == 1) %>% nrow() 
numb_rows
stat_SP <- stat_condition_3(GSPC, 4, "1929-01-02", 1000, 22521, 5, 10, "GSPC")
View(stat_SP)   #works on GSPC 




#BEGIN- AVR ON CONDITION (3)----------------------------------------------------------------
#A LOOK FORWARD on for AVR -----------------------------------------------------------------

# function to output date, looking 'lookforward' days ahead of a specific date

lookforward_date <- function(index_name, closing_column, date_of_interest, lookforward){
  
  output_index <- index_name[, closing_column]
  
  index_dataset <- data.frame(date=index(output_index), coredata(output_index))
  
  lookforward_data <- index_dataset %>% 
    mutate(date = as.Date(date)) %>%
    filter(date >= date_of_interest) %>%
    arrange((date)) %>%
    head(lookforward)
  
  lookforward_date <- lookforward_data[lookforward,1]
  
  #outputting the date which is equivalent to the 'lookforward' number of data points
  #after the date of interest 
  return(lookforward_date)
}

#testing the above function- correct 
lookforward_date(DJI_whole, 1, "1992-01-02", 20)

#NOW_________________ONTO the AVR tables appended after each other

#function outputting the AVR windows with the points at which AVR jumps above the threshold (1's and 0's)
# MINIMUM INPUT FOR AVR_windows IS 1!! (gives 3 WINDOWS)
#our definition; AVR jumps over 10^-3 threshold; need to add threshold of white noise still !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
AVR_stat_test <- function(total_shifts, index_name, closing_column, starting_date, N, interval_T, AVR_windows){
  
  next_date <- starting_date 
  
  AVR_first <- AVR_with_time(total_shifts, index_name, closing_column, next_date, N, interval_T) %>% 
    mutate(condition_AVR = ifelse(eta > 10^-3, 1, 0)) %>%                #our definition; AVR jumps over 10^-3 threshold
    mutate(AVR_window = 1)
  
  #outputs AVR_WINDOWS;(INPUT AVR WINDOWS WANTED - 2 IN THE FOR LOOP)
  for(l in seq(from=0, to=AVR_windows - 2, by=1)) { 
    
    #KEEP the starting date here
    next_date <- lookforward_date(index_name, closing_column, starting_date, (total_shifts*(l+1))-l)  
    
    AVR_next <-  AVR_with_time(total_shifts, index_name, closing_column, next_date, N, interval_T)  %>% 
      mutate(condition_AVR = ifelse(eta > 10^-3, 1, 0)) %>%
      mutate(AVR_window = l + 2)
    
    AVR_first <- rbind(AVR_first, AVR_next) 
    
  }
  
  #AVR second; filtering AVR_first; taking out the beginning rows of each window; where condition_AVR = NA  
  AVR_second <- AVR_first %>%      
    filter(!is.na(condition_AVR)) %>%
    mutate(row = row_number())
  
  AVR_1 <- AVR_second %>%    #AVR_1; keeping just row number, end date and condition columns from AVR_second
    select(start_date, end_date, condition_AVR, row, AVR_window)
  
  AVR_2 <- AVR_1 %>%   #AVR_2; creating table with row index + 1 in order to see if the previous condition_AVR = 0
    mutate(row = row + 1)
  
  
  #joining the 2 AVR tables together and testing the jump condition; 1 when a group of 1's pops up 
  #(the first instance of an AVR jump), i.e. when a 0 appears before a 1 in the column or the 1 is the first non-empty
  #entry in the column (with an NA in front)
  AVR_3 <- AVR_1 %>% 
    left_join(
      AVR_2, 
      by = "row", suffix = c("", ".prev")
    ) %>%
    mutate(stat_test = ifelse((condition_AVR == 1 & condition_AVR.prev == 0) | (condition_AVR == 1 & is.na(condition_AVR.prev)), 1, 0)) %>%
    select(start_date, end_date, condition_AVR, stat_test, AVR_window)
  
  return(AVR_3)
}

#--------------------------------------------------------------------------------------
# function to get the number to input into lookforward_date (the last input- lookfoward) in order to see
# the last date included in the AVR windows, from the above function (THE FORMULA WORKS - HAS BEEN QA'ED)
last_AVR_date <- function(index_name, closing_column, date_of_interest, total_shifts, AVR_windows){
  
  date_place <- (total_shifts*(AVR_windows-1))-(AVR_windows-2) + (total_shifts-1)
  lookforward_date(index_name, closing_column, date_of_interest, date_place)
  
}

# TEST; how these 2 functions can be used; 
AVR_stat_1 <- AVR_stat_test(20, DJI_whole, 1, "1992-01-02", 1000, 1, 4)
View(AVR_stat_1)

#the last (START DATE) date output from the table via the AVR_stat_test function (+ N + interval_T to get the first end date)
last_AVR_date(DJI_whole, 1, "1992-01-02", 20, 4)



#THE FINAL STATISTICAL OUTCOME TABLE 
# NOW, putting the above in a function to output the summarized table (4 cols)
# this will take a window over which AVR shifts are performed (specified by input 'total_shifts') then output weather the AVR has jumped 
# and in this same window (takeing into consideration the AVR end_dates and using this dates to check if crashes have 
# occurred in this time in the financial time series)- based off of this sucesses and failures are output 
AVR_TS_stat_test_1 <- function(total_shifts, index_name, closing_column, starting_date, N, interval_T, AVR_windows, drop_period, drop, index_quotes){
  
  AVR_stat_1 <- AVR_stat_test(total_shifts, index_name, closing_column, starting_date, N, interval_T, AVR_windows)
  
  #rows in AVR_stat_1;
  AVR_rows <- AVR_stat_1 %>% nrow()
  
  #getting the last and first day from AVR_jump to filter ts;
  #first end day in AVR (the one contributing to the jumps in AVR);
  AVR_start_date <- AVR_stat_1[1,2]
  #last AVR end_date
  last_date <- AVR_stat_1[AVR_rows, 2]
  
  #BRING in the time series dataset, need to get from and up to the same date as AVR
  ts <- stat_condition_3(index_name, closing_column, AVR_start_date, N, total_shifts, drop_period, drop, index_quotes)
  
  #get ts dates to match up with AVR dates
  ts_new <- ts %>%
    mutate(ts_day1 = as.Date(ts_day1)) %>%
    filter(ts_day1 <= last_date) %>%                                  
    rename(end_date = ts_day1)
  
  # join AVR and ts together to get the stat test outputs 
  AVR_ts <- AVR_stat_1 %>% 
    left_join(ts_new,  
              by = "end_date")
  
  #CAN OUTPUT this table if more detail needed about the summaries table outputted from this function
  #AVR_ts_new <- AVR_ts %>% 
  #  group_by(AVR_window) %>%
  #  mutate(ts_stat = sum(ts_condition_3),
  #         AVR_stat = sum(stat_test),
  #         begin_date = min(end_date))
  
  #table outputting the successes (1) and failures (0) of the AVR jumps compared with the crash points of the
  # time series 
  AVR_ts <- AVR_ts %>% 
    group_by(AVR_window) %>%
    summarise(ts_stat = sum(ts_conditions),   
              AVR_stat = sum(stat_test),
              begin_shift_end_date = min(end_date)) %>%
    mutate(Success = ifelse((ts_stat == AVR_stat) | (AVR_stat > 0 & ts_stat > 0), 1, 0))
  
  return(AVR_ts) 
} 

#testing the above function - seems to work
AVR_TS_stat_test_1(20, DJI_whole, 1, "1992-01-02", 1000, 1, 4, 5, 10, "DJI_whole")

GSPC_stat_test <- AVR_TS_stat_test_1(20, GSPC, 4, "1992-01-02", 1000, 1, 4, 5, 10, "GSPC")
SP_stat_test <- AVR_TS_stat_test_1(20, SP_whole, 1, "1992-01-02", 1000, 1, 4, 5, 10, "SP_whole")

SP_stat_test

#number of trails is the max number in the AVR_windows column and sucesses are the sum of the 1's 
sucesses <- GSPC_stat_test %>% summarise(total_sucessessful_trails = sum(Success)) %>% data.frame()
trails <- GSPC_stat_test %>% summarise(number_of_trails = max(AVR_window)) %>% data.frame()
 
#getting Z value depending on the stat_test output
X <- sucesses[1,1]
n <- trails[1,1]

#input the table released from function AVR_TS_stat_test_1 to get the Z value, with a success 
# probability claim, p, input into the function 
Z <- function(stat_test_table, p){
  
  sucesses <- stat_test_table %>% summarise(total_sucessessful_trails = sum(Success)) %>% data.frame()
  trails <- stat_test_table %>% summarise(number_of_trails = max(AVR_window)) %>% data.frame()
  
  X <- sucesses[1,1]
  n <- trails[1,1]
  
  Z <- (X - (n * p)) / (sqrt(n*p*(1-p)))
  
  return(Z)
}

#testing Z value with the GSPC_stat_test table and p=0.03 
Z(GSPC_stat_test, 0.03)


