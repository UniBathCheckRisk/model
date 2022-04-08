#AVR 1987 crash changing parameter N crash window n=50, DJI, crash at n=25 16/03/2020
# N:50, 100, 250, 500, 1000, 2000, 5000 

lookback_date(DJI_whole, 1, "1987-10-19", 5025)

T_1 <- AVR_with_time(50, DJI_whole, 1, as.Date("1983-09-28"), 1000, 1)
#changed
T_2 <- AVR_with_time(50, DJI_whole, 1, as.Date("1987-04-22"), 100, 1)
T_3 <- AVR_with_time(50, DJI_whole, 1, as.Date("1986-09-17"), 250, 1)
T_4 <- AVR_with_time(50, DJI_whole, 1, as.Date("1985-09-19"), 500, 1)
T_5 <- AVR_with_time(50, DJI_whole, 1, as.Date("1979-10-15"), 2000, 1)
T_6 <- AVR_with_time(50, DJI_whole, 1, as.Date("1987-07-02"), 50, 1)
T_7 <- AVR_with_time(50, DJI_whole, 1, as.Date("1967-11-16"), 5000, 1)

changing_N <- ggplot(data = T_1, color = "green", aes(x = n, y = eta)) +
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
  geom_line(data = T_7, color = "turquoise") +
  geom_point(data = T_7, color = "turquoise") +
  ggtitle("AVR 2020 crash, centred at n=25 at 16-03-2020, green=N=1000,
          blue=N=100, red=N=250, black=N=500, pink=N=2000, orange=N=50, turquoise=N=5000")

changing_N #output plot

#check <- specific_heat_function(DJI_whole, 1, "1979-10-15", 2000, 1)
#View(check)
#ggplot(data = check, aes(x=q, y=Cq)) +
#  geom_point(data = check)



