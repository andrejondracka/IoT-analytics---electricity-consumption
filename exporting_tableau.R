###export the reduced dataset

meter_data <- meter_data[lubridate::year(meter_data$DateTime) == 2009,]
meter_data <- meter_data[lubridate::month(meter_data$DateTime) == 12,]

minutedata_dec09 <- gather(meter_data, key = 'meter', value = 'consumption', 
                           Kitchen, `Hot water/AC`, `Laundry Room`, Other, Total)

write.csv(minutedata_dec09, file = 'minutedata.csv')
