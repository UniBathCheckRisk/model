#output AVR overlay Covid crash on all markets----------------------------------
#covid biggest day drop is 2020-03-16

lookback_date(SP_whole, 1, "2020-03-16", 1025)
lookback_date(DJI_whole, 1, "2020-03-16", 1025) 
lookback_date(HSI, 4, "2020-03-16", 1025) 
lookback_date(GLD, 4, "2020-03-16", 1025) 
lookback_date(FTSE, 4, "2020-03-16", 1025) 
lookback_date(`BTC-USD`, 4, "2020-03-16", 1025)

SPCoV <- AVR_with_time(50, SP_whole, 1, as.Date("2016-02-19"), 1000, 1) 
DJICoV <- AVR_with_time(50, DJI_whole, 1, as.Date("2016-02-19"), 1000, 1)
HSICoV <- AVR_with_time(50, HSI, 4, as.Date("2016-01-20"), 1000, 1)
GLDCoV <- AVR_with_time(50, GLD, 4, as.Date("2016-02-19"), 1000, 1)
FTSECoV <- AVR_with_time(50, FTSE, 4, as.Date("2016-02-26"), 1000, 1)
`BTC-USDCoV` <- AVR_with_time(50, `BTC-USD`, 4, as.Date("2017-05-27"), 1000, 1)

#Dates to use if you want n=25 to be 20-02-2020 (start of covid crash)
#SPCoV <- AVR_with_time(50, SP_whole, 1, as.Date("2016-01-26"), 1000, 1) 
#DJICoV <- AVR_with_time(50, DJI_whole, 1, as.Date("2016-01-26"), 1000, 1)
#HSICoV <- AVR_with_time(50, HSI, 4, as.Date("2015-12-24"), 1000, 1)
#GLDCoV <- AVR_with_time(50, GLD, 4, as.Date("2016-01-26"), 1000, 1)
#FTSECoV <- AVR_with_time(50, FTSE, 4, as.Date("2016-02-03"), 1000, 1)
#`BTC-USDCoV` <- AVR_with_time(50, `BTC-USD`, 4, as.Date("2017-05-02"), 1000, 1)

#setting up the overlay plot
AVRCoV <- ggplot(data = SPCoV, color = "green", aes(x = n, y = eta)) +
  geom_line() + geom_point(data = SPCoV, color = "green") +
  geom_line(data = DJICoV, color = "black") +
  geom_point(data = DJICoV, color = "black") +
  geom_line(data = HSICoV, color = "pink") +
  geom_point(data = HSICoV, color = "pink") +
  geom_line(data = GLDCoV, color = "red") +
  geom_point(data = GLDCoV, color = "red") +
  geom_line(data = FTSECoV, color = "orange") +
  geom_point(data = FTSECoV, color = "orange") +
  geom_line(data = `BTC-USDCoV`, color = "turquoise") +
  geom_point(data = `BTC-USDCoV`, color = "turquoise") +
  ggtitle("AVR Overlay Covid crash n=25 16-03-2020, red=GLD, green=SP, black=DJI, orange=FTSE, 
          pink=HSI, BTC-US`=turquoise", )

AVRCoV #outputting the plot


