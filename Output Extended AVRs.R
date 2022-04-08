#extended time period; 
#AVR_extended2 <- AVR_with_time(6614, DJI, 4, as.Date("1992-01-02"), 1000, 1) 
#AVR_extended2 %>% View()

#ggplot(AVR_extended2,  aes(x = n, y= eta)) +
#  geom_point() + geom_line()

#n_200 <- AVR_extended2 %>% filter(n <= 200)

#n_200 %>% ggplot(aes(x = n, y= eta)) +
#  geom_point() + geom_line() + ggtitle("extended time period AVR from 1992 to 2022, taking the first 200 shifts,
#                                     window size N= 1000, end dates 1995 to 1996 included")

#n_200 %>% View()


#write.csv(AVR_extended,"H:\\project_checkrisk\\AVR1.csv", row.names = TRUE)


#class(AVR_extended)
#-------------------------------------------------------------------

#extended time period; 1955-1975

#AVR_extended1955 <- AVR_with_time(5310, DJI_whole, 1, as.Date("1951-01-19"), 1000, 1) 

#lookback_date(DJI_whole, 1, "1955-01-01", 1000)
#whole_historic_DJI %>% filter(date >= "1955-01-01" & date <= "1975-12-31") %>% nrow()


#ggplot(AVR_extended1955,  aes(x = end_date, y= eta)) +
#  geom_point() + geom_line()

#n_200 <- AVR_extended2 %>% filter(n <= 200)

#n_200 %>% ggplot(aes(x = n, y= eta)) +
#  geom_point() + geom_line() + ggtitle("extended time period AVR from 1992 to 2022, taking the first 200 shifts,
#                                     window size N= 1000, end dates 1995 to 1996 included")

#n_200 %>% View()

#write.csv(AVR_extended1955,"H:\\project_checkrisk\\AVR5.csv", row.names = TRUE)
