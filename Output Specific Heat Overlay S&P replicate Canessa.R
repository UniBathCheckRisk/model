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