###fridge consumption

meter_data_laundry2 <- meter_data[,c('DateTime', 'Laundry Room')]


plot(meter_data_laundry2$DateTime[1380:2820], 
     meter_data_laundry2$`Laundry Room`[1380:2820], type = 'l',
     xlab = 'hour', ylab = 'energy consumption (Wh)')

meter_data_laundry <- meter_data[,c('Date', 'Laundry Room')]
meter_data_offdays <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(meter_data_offdays) <- c('Date', 'Laundry Room')
  
bla <- unique(meter_data_laundry$Date)


for (ii in bla) {
  tempdata = meter_data_laundry[meter_data_laundry$Date == ii,]
  
  if (max(tempdata$`Laundry Room`) == 2) {
    meter_data_offdays <- rbind (meter_data_offdays, tempdata)
  }
}

meter_data_offdays_daily <- meter_data_offdays %>% group_by(Date) %>% summarize(`Laundry Room` = sum(`Laundry Room`))
meter_data_offdays_daily$Date <- as.POSIXct(meter_data_offdays_daily$Date)

plot(meter_data_offdays_daily$Date, meter_data_offdays_daily$`Laundry Room`)
meter_data_daily_offrem <- subset(meter_data_offdays_daily, `Laundry Room` < 1000)

plot(meter_data_daily_offrem$Date, meter_data_daily_offrem$`Laundry Room`)

meter_data_daily_offrem$Daydiff <- (meter_data_daily_offrem$Date - meter_data_daily_offrem$Date[1]) / 86400

linfit <- lm(meter_data_daily_offrem$`Laundry Room` ~ meter_data_daily_offrem$Daydiff)
linfit$coefficients[2] * 365

meter_data_daily_offrem$fit <- meter_data_daily_offrem$Daydiff*linfit$coefficients[2] + linfit$coefficients[1]

plot(meter_data_daily_offrem$Date, meter_data_daily_offrem$`Laundry Room`, xlab = 'Year', ylab = 'Daily consumption (Wh)')
lines(meter_data_daily_offrem$Date, meter_data_daily_offrem$fit, col = 'red')
