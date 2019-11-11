library(Metrics)


load('granulardata.Rdata')

monthly_total <- data.frame(granular_data$Month[c('ds','Total')])
monthly_total$data <- ifelse(lubridate::year(monthly_total$ds) == 2010, 'Test', 'Train')

ggplot(data = monthly_total, aes(x = ds, y = Total, color = data, size = data)) + 
  geom_line() + scale_size_manual(values=c('Train' = 3, 'Test' = 1)) + xlab('Year') +
  ylab('Monthly consumption (kWh)')





monthly_total_train <- monthly_total[1:36,]
monthly_total_test <- monthly_total[37:47,]

monthly_total_ts <- ts(monthly_total_train$Total, frequency = 12)

#auto arima
autoarima_t <- auto.arima(monthly_total_ts)
autoarima_fc <- forecast(autoarima_t, h = 11)

plot(autoarima_fc)

#holt winters
hws_t <- HoltWinters(monthly_total_ts)
hws_fc <- forecast(hws_t, h = 11)

plot(hws_fc)

#lm
linmodel_t <- tslm(monthly_total_ts ~ trend + season)
linmodel_fc <- forecast(linmodel_t, h = 11)

plot(linmodel_fc)


###error analysis 
errors <- data.frame(cbind(monthly_total_test$Total, autoarima_fc$mean, hws_fc$mean, linmodel_fc$mean))
colnames(errors) <- c('real data', 'ARIMA auto', 'Holt Winters', 'Linear model')

errors$`month in 2010` <- c(1:11)

mapes <- c(mape(errors$monthly_total_test.Total, errors$autoarima_fc.mean),
           mape(errors$monthly_total_test.Total, errors$hws_fc.mean),
           mape(errors$monthly_total_test.Total, errors$linmodel_fc.mean))

errors2 <- gather(errors, key = 'model', value = 'Energy consumption (kWh)', 
                  `real data`, `ARIMA auto`, `Holt Winters`, `Linear model`)


ggplot(data = errors2, aes(x = `month in 2010`, y = `Energy consumption (kWh)`, color = model, size = model)) +
  geom_line() + scale_size_manual(values=c('real data' = 2, `ARIMA auto` = 1,
                                           `Holt Winters` = 1, `Linear model` = 1))


plot(c(1:11), errors$monthly_total_test.Total, type='l')
lines(c(1:11), errors$autoarima_fc.mean, col = 'red')
lines(c(1:11), errors$hws_fc.mean, col = 'blue')
lines(c(1:11), errors$linmodel_fc.mean, col = 'green')





plot(meter_data$`Laundry Room`[1569835:1570835], type = 'l', col = 'red')
lines(meter_data$`Laundry Room`[1:1000], col = 'blue')



