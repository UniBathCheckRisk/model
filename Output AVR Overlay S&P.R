#output AVR overlay with S&P-----------------------------------------------
lookback_date(SP_whole, 1, "2020-02-20", 1040)

SP_T60 <- AVR_with_time(130, SP_whole, 1, as.Date("1955-12-14"), 1000, 1)
SP_29 <- AVR_with_time(130, SP_whole, 1, as.Date("1926-03-29"), 1000, 1)
SP_87 <- AVR_with_time(130, SP_whole, 1, as.Date("1983-09-08"), 1000, 1) 
SP_08 <- AVR_with_time(130, SP_whole, 1, as.Date("2004-07-15"), 1000, 1)
SP_11 <- AVR_with_time(130, SP_whole, 1, as.Date("2007-05-09"), 1000, 1)
SP_20 <- AVR_with_time(130, SP_whole, 1, as.Date("2016-01-04"), 1000, 1)

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
  ggtitle("AVR Overlay S&P replicating Fonseca, red=1929 (n=30 24/10/1929)
          , green=1987 (n=40 19/10/1987), blue=2008 (n=50 15/09/2008), pink=2011 
          (n=70 05/08/2011), orange=2020 (n=40 20-02-2020), black=test (n=20 01-01-1960)")

AVRSP #outputting the plot 