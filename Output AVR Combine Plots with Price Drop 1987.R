#attempt to plot two charts side by side

lookback_date(DJI_whole, 1, "1987-10-19", 24)

AVR87_5_DJI <- AVR_with_time(50, DJI_whole, 1, as.Date("1983-09-28"), 1000, 1)
AVR87_5_GSPC <- AVR_with_time(50, GSPC, 4, as.Date("1983-09-29"), 1000, 1)

AVR87_5 <- ggplot(data = AVR87_5_DJI, color = "green", aes(x = end_date, y = eta)) +
  geom_line() + geom_point(data = AVR87_5_DJI, color = "green") +
  ggtitle("AVR 1987 crash, n=50 centred at BM, 
          green=DJI, blue=raw")

raw87 <- raw_data(DJI_whole, 1, as.Date("1987-09-14"), 49, 1)

AVRraw <- ggplot(data = raw87, color = "black", aes(x = date, y = coredata.output_index.)) +
  geom_line()

AVRraw + AVR87_5 #plots both side by side

#attempt to add in financial raw data to background
# joining raw87 and AVR87_5_DJI by matching end_date to plot on one graph
table <- AVR87_5_DJI %>% 
  left_join(raw87 %>%
              rename(end_date = date),
            by = "end_date") %>%
  rename(price =  coredata.output_index.) %>%
  mutate(log_price = log10(price))

#plotting both axes onto same graph
ggplot(table, aes(x=end_date)) +
  geom_line( aes(y=eta), color="blue") +
  geom_point( aes(y=eta), color="blue") +
  geom_line( aes(y=(price / 5000)-0.35)) +
  scale_y_continuous(
    name = "eta",
    sec.axis = sec_axis(~.*5000+1750, name = "price" )
  ) +
  ggtitle("1987 crash showing AVR jump in blue and daily price in black")