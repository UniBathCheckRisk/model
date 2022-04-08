# Improved dates-  using biggest daily price drop as crash day
#replicating/improving Fonseca AVR overlay plots with DJI------------------------------

lookback_date(DJI_whole, 1, "2020-03-16", 1040)

DJI_T60i <- AVR_with_time(130, DJI_whole, 1, as.Date("1955-12-30"), 1000, 1)
DJI_29i <- AVR_with_time(130, DJI_whole, 1, as.Date("1925-11-24"), 1000, 1)
DJI_87i <- AVR_with_time(130, DJI_whole, 1, as.Date("1983-09-07"), 1000, 1)
DJI_08i <- AVR_with_time(130, DJI_whole, 1, as.Date("2004-07-29"), 1000, 1)
DJI_11i <- AVR_with_time(130, DJI_whole, 1, as.Date("2007-05-10"), 1000, 1)
DJI_20i <- AVR_with_time(130, DJI_whole, 1, as.Date("2016-01-28"), 1000, 1)

#setting up the overlay plot
AVRDJIi <- ggplot(data = DJI_87i, color = "green", aes(x = n, y = eta)) +
  geom_line() + geom_point(data = DJI_87i, color = "green") +
  geom_line(data = DJI_08i, color = "blue") +
  geom_point(data = DJI_08i, color = "blue") +
  geom_line(data = DJI_11i, color = "pink") +
  geom_point(data = DJI_11i, color = "pink") +
  geom_line(data = DJI_T60i, color = "black") +
  geom_point(data = DJI_T60i, color = "black") +
  geom_line(data = DJI_29i, color = "red") +
  geom_point(data = DJI_29i, color = "red") +
  geom_line(data = DJI_20i, color = "orange") +
  geom_point(data = DJI_20i, color = "orange") +
  scale_x_continuous(breaks=seq(0, 130, 20)) +
  ggtitle("AVR Overlay DJI improving dates from Fonseca, red=1929 (n=30 28/10/1929)
          , green=1987 (n=40 19/10/1987), blue=2008 (n=50 29/09/2008), pink=2011 
          (n=70 08/08/2011), orange=2020 (n=40 16-03-2020), black=test (n=20 01-01-1960)")

AVRDJIi #outputting the plot
