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