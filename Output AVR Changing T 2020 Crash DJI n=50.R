#AVR Covid 2020 changing parameter T1:6 crash window n=50, DJI, crash at n=25 16/03/2020

lookback_date(DJI_whole, 1, "2020-03-16", 1025)


T1 <- AVR_with_time(50, DJI_whole, 1, as.Date("2016-02-19"), 1000, 1)
#changed T2 start date so that both T1 and T2 have end_date=16-03-2020 at n=25
T2 <- AVR_with_time(50, DJI_whole, 1, as.Date("2016-02-18"), 1000, 2)
T3 <- AVR_with_time(50, DJI_whole, 1, as.Date("2016-02-17"), 1000, 3)
T4 <- AVR_with_time(50, DJI_whole, 1, as.Date("2016-02-16"), 1000, 4)
T5 <- AVR_with_time(50, DJI_whole, 1, as.Date("2016-02-12"), 1000, 5)
T6 <- AVR_with_time(50, DJI_whole, 1, as.Date("2016-02-11"), 1000, 6)


change_T <- ggplot(data = T1, color = "green", aes(x = n, y = eta)) +
  geom_line() + geom_point(data = T1, color = "green") +
  geom_line(data = T2, color = "blue") +
  geom_point(data = T2, color = "blue") +
  geom_line(data = T3, color = "red") +
  geom_point(data = T3, color = "red") +
  geom_line(data = T4, color = "black") +
  geom_point(data = T4, color = "black") +
  geom_line(data = T5, color = "pink") +
  geom_point(data = T5, color = "pink") +
  geom_line(data = T6, color = "orange") +
  geom_point(data = T6, color = "orange") +
  ggtitle("AVR 2020 crash, centred at n=25 at 16-03-2020, green=T=1, blue=T=2, red=T=3")

change_T #output plot

