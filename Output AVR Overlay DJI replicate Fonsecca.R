# replicating Fonseca AVR overlay plots with DJI------------------------------

lookback_date(DJI_whole, 1, "2020-02-20", 1040)

DJI_T60 <- AVR_with_time(130, DJI_whole, 1, as.Date("1955-12-30"), 1000, 1)
DJI_29 <- AVR_with_time(130, DJI_whole, 1, as.Date("1925-11-20"), 1000, 1)
DJI_87 <- AVR_with_time(130, DJI_whole, 1, as.Date("1983-09-07"), 1000, 1)
DJI_08 <- AVR_with_time(130, DJI_whole, 1, as.Date("2004-07-15"), 1000, 1)
DJI_11 <- AVR_with_time(130, DJI_whole, 1, as.Date("2007-05-09"), 1000, 1)
DJI_20 <- AVR_with_time(130, DJI_whole, 1, as.Date("2016-01-04"), 1000, 1)

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
  ggtitle("AVR Overlay DJI replicating Fonseca, red=1929 (n=30 24/10/1929)
          , green=1987 (n=40 19/10/1987), blue=2008 (n=50 15/09/2008), pink=2011 
          (n=70 05/08/2011), orange=2020 (n=40 20-02-2020), black=test (n=20 01-01-1960)")

AVRDJI #outputting the plot