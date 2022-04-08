# OUTPUT ---- 1987 crash AVR with DJI and SP changing avr window for jump date accuracy
#assume 1987 crash date is Black Monday 1987-10-19

#attempt 1 - avr window n=100 with crash date centred at n=50
lookback_date(SP_whole, 1, "1987-10-19", 1050)

AVR87_1_DJI <- AVR_with_time(100, DJI_whole, 1, as.Date("1983-08-27"), 1000, 1)
AVR87_1_GSPC <- AVR_with_time(100, SP_whole, 1, as.Date("1983-08-24"), 1000, 1)

AVR87_1 <- ggplot(data = AVR87_1_DJI, color = "green", aes(x = n, y = eta)) +
  geom_line() + geom_point(data = AVR87_1_DJI, color = "green") +
  geom_line(data = AVR87_1_GSPC, color = "blue") +
  geom_point(data = AVR87_1_GSPC, color = "blue") +
  ggtitle("AVR 1987 crash, n=100 centred at BM, green=DJI, blue=S&P")

AVR87_1 #output plot


#attempt 2 - avr window n=200 with crash date centred at n=100
lookback_date(SP_whole, 1, "1987-10-19", 1100)

AVR87_2_DJI <- AVR_with_time(200, DJI_whole, 1, as.Date("1983-06-13"), 1000, 1)
AVR87_2_GSPC <- AVR_with_time(200, SP_whole, 1, as.Date("1983-06-14"), 1000, 1)

AVR87_2 <- ggplot(data = AVR87_2_DJI, color = "green", aes(x = n, y = eta)) +
  geom_line() + geom_point(data = AVR87_2_DJI, color = "green") +
  geom_line(data = AVR87_2_GSPC, color = "blue") +
  geom_point(data = AVR87_2_GSPC, color = "blue") +
  ggtitle("AVR 1987 crash, n=200 centred at BM, green=DJI, blue=S&P")

AVR87_2 #output plot


#attempt 3 - avr window n=50 with crash date centred at n=25
lookback_date(SP_whole, 1, "1987-10-19", 1025)

AVR87_3_DJI <- AVR_with_time(50, DJI_whole, 1, as.Date("1983-09-28"), 1000, 1)
AVR87_3_GSPC <- AVR_with_time(50, SP_whole, 1, as.Date("1983-09-29"), 1000, 1)

AVR87_3 <- ggplot(data = AVR87_3_DJI, color = "green", aes(x = n, y = eta)) +
  geom_line() + geom_point(data = AVR87_3_DJI, color = "green") +
  geom_line(data = AVR87_3_GSPC, color = "blue") +
  geom_point(data = AVR87_3_GSPC, color = "blue") +
  ggtitle("AVR 1987 crash, n=50 centred at BM, green=DJI, blue=S&P")

AVR87_3 #output plot


#attempt 4 - avr window n=20 with crash date at end of window
lookback_date(SP_whole, 1, "1987-10-19", 1025)

AVR87_4_DJI <- AVR_with_time(25, DJI_whole, 1, as.Date("1983-09-28"), 1000, 1)
AVR87_4_GSPC <- AVR_with_time(25, SP_whole, 1, as.Date("1983-09-29"), 1000, 1)

AVR87_4 <- ggplot(data = AVR87_4_DJI, color = "green", aes(x = n, y = eta)) +
  geom_line() + geom_point(data = AVR87_4_DJI, color = "green") +
  geom_line(data = AVR87_4_GSPC, color = "blue") +
  geom_point(data = AVR87_4_GSPC, color = "blue") +
  ggtitle("AVR 1987 crash, n=25 BM at end, green=DJI, blue=S&P")

AVR87_4 #output plot
#-------------------------------use small n window for accuracy ----------------------
