#AVR 1987 crash changing parameter T1:6 crash window n=50, DJI, crash at n=25 16/03/2020

lookback_date(DJI_whole, 1, "1987-10-19", 1025)


T_1 <- AVR_with_time(50, DJI_whole, 1, as.Date("1983-09-29"), 1000, 1)
#changed T2 start date so that both T1 and T2 have end_date=16-03-2020 at n=25
T_2 <- AVR_with_time(50, DJI_whole, 1, as.Date("1983-09-28"), 1000, 2)
T_3 <- AVR_with_time(50, DJI_whole, 1, as.Date("1983-09-27"), 1000, 3)
T_4 <- AVR_with_time(50, DJI_whole, 1, as.Date("1983-09-26"), 1000, 4)
T_5 <- AVR_with_time(50, DJI_whole, 1, as.Date("1983-09-23"), 1000, 5)
T_6 <- AVR_with_time(50, DJI_whole, 1, as.Date("1983-09-22"), 1000, 6)


changing_T <- ggplot(data = T_1, color = "green", aes(x = n, y = eta)) +
  geom_line() + geom_point(data = T_1, color = "green") +
  geom_line(data = T_2, color = "blue") +
  geom_point(data = T_2, color = "blue") +
  geom_line(data = T_3, color = "red") +
  geom_point(data = T_3, color = "red") +
  geom_line(data = T_4, color = "black") +
  geom_point(data = T_4, color = "black") +
  geom_line(data = T_5, color = "pink") +
  geom_point(data = T_5, color = "pink") +
  geom_line(data = T_6, color = "orange") +
  geom_point(data = T_6, color = "orange") +
  ggtitle("AVR 2020 crash, centred at n=25 at 16-03-2020, green=T=1,
          blue=T=2, red=T=3, black=T=4, pink=T=5, orange=T=6")

changing_T #output plot

