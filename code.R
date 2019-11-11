library(RMySQL)
library(tidyverse)
library(plotly)
library(makeR)
library(forecast)
library(prophet)
library(Metrics)

####load data and merge, transform, rename####

con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

dbListTables(con)
dbListFields(con, 'yr_2009')

yr_2006 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")
yr_2007 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")
yr_2008 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")
yr_2009 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")
yr_2010 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")

meter_data <- rbind(yr_2007, yr_2008, yr_2009, yr_2010)

meter_data$Global_active_power <- meter_data$Global_active_power *100/6
meter_data$Other <- meter_data$Global_active_power - meter_data$Sub_metering_1 - meter_data$Sub_metering_2 - meter_data$Sub_metering_3
colnames(meter_data)[c(3:7)] <- c('Total','Kitchen','Laundry Room','Hot water/AC','Other')


####read date and time and gather the DF

meter_data <-cbind(meter_data,paste(meter_data$Date,meter_data$Time), stringsAsFactors=FALSE)
colnames(meter_data)[8] <-"DateTime"
meter_data <- meter_data[,c(ncol(meter_data), 1:(ncol(meter_data)-1))]
meter_data$DateTime <- as.POSIXct(meter_data$DateTime, "%Y/%m/%d %H:%M:%S")
attr(meter_data$DateTime, "tzone") <- "Europe/Paris"

meter_data$Weekday <- factor(lubridate::wday(meter_data$DateTime, week_start = 1, label = TRUE))
meter_data$Week <- lubridate::isoweek(meter_data$DateTime)
meter_data$Year <- lubridate::year(meter_data$DateTime)
meter_data$Month <- lubridate::month(meter_data$DateTime)
meter_data$Day <- lubridate::day(meter_data$DateTime)
meter_data$Hour <- lubridate::hour(meter_data$DateTime)
meter_data$Minute <- lubridate::minute(meter_data$DateTime)
meter_data$Date <- strftime(meter_data$DateTime, format= "%F")
meter_data$Time <- strftime(meter_data$DateTime, format= "%H:%M")

meter_data_test <- meter_data[meter_data$Year == 2010,]
meter_data_train <- meter_data[meter_data$Year != 2010,]

####make some plots####

#a calendar with total consumption
meter_total_byday <- meter_data %>% group_by(Date, Year, Month, Day) %>% 
  summarize(Total = sum(Total)/1000, 
            Kitchen = sum(Kitchen)/1000, 
            `Laundry Room` = sum(`Laundry Room`)/1000, 
            `Hot water/AC` = sum(`Hot water/AC`)/1000,
            Other = sum(Other)/1000)

calendarHeat(meter_total_byday$Date, meter_total_byday$Total, varname = 'Total Consumption')

meter_total_byday <- subset(meter_total_byday, Year < 2010)

calendarHeat(meter_total_byday$Date, meter_total_byday$Kitchen, varname = 'Kitchen Consumption')
calendarHeat(meter_total_byday$Date, meter_total_byday$`Laundry Room`, varname = 'Laundry Room')
calendarHeat(meter_total_byday$Date, meter_total_byday$`Hot water/AC`, varname = 'Hot water/AC')
calendarHeat(meter_total_byday$Date, meter_total_byday$Other, varname = 'Other')

##pie chart of energy consumption on august 2008 day
august19data <- data.frame(t(meter_total_byday[596,6:9]))
colnames(august19data) <- 'consumption'
august19data$submeter <- rownames(august19data)

ggplot(data = august19data, aes(x="", y=consumption, fill=submeter)) + 
  geom_bar(stat = 'identity') + coord_polar('y', start = 0) + xlab('') + ylab('') +
  theme(axis.text.x=element_blank())


sumdata <- data.frame(apply(meter_total_byday[,6:9], 2, sum))
colnames(sumdata) <- 'consumption'
sumdata$submeter <- rownames(sumdata)
sumdata$cumul <- cumsum(sumdata$consumption)

ggplot(data = sumdata, aes(x=submeter, y=consumption, fill=submeter)) + 
  geom_bar(stat = 'identity') +
  theme(axis.text.x=element_blank()) + geom_text(aes(label = round(consumption),
                                                     y = consumption + 1000)) +
  ylab('total consumption (kWh)')


####granularizing the data####

granular_data <- list()

granular_data[['Hour']] <- meter_data %>% group_by(Year, Month, Day, Hour, Week, Weekday) %>% 
  summarize (Kitchen = sum(Kitchen)/1000, 
             `Laundry Room` = sum(`Laundry Room`)/1000, 
             `Hot water/AC` = sum(`Hot water/AC`)/1000, 
             Other = sum(Other)/1000, 
             Total = sum(Total)/1000)
granular_data$Hour$ds <- lubridate::ymd_h(paste(granular_data$Hour$Year, 
                                                granular_data$Hour$Month, 
                                                granular_data$Hour$Day, 
                                                granular_data$Hour$Hour))

granular_data[['Day']] <- meter_data %>% group_by(Year, Month, Day, Week, Weekday) %>% 
  summarize (Kitchen = sum(Kitchen)/1000, 
             `Laundry Room` = sum(`Laundry Room`)/1000, 
             `Hot water/AC` = sum(`Hot water/AC`)/1000, 
             Other = sum(Other)/1000, 
             Total = sum(Total)/1000)
granular_data$Day$ds <- lubridate::ymd(paste(granular_data$Day$Year, 
                                             granular_data$Day$Month, 
                                             granular_data$Day$Day))

granular_data[['Week']] <- meter_data %>% group_by(Year, Week) %>% 
  summarize (Kitchen = sum(Kitchen)/1000, 
             `Laundry Room` = sum(`Laundry Room`)/1000, 
             `Hot water/AC` = sum(`Hot water/AC`)/1000, 
             Other = sum(Other)/1000, 
             Total = sum(Total)/1000, 
             ds = min(DateTime))

granular_data[['Month']] <- meter_data %>% group_by(Year, Month) %>% 
  summarize (Kitchen = sum(Kitchen)/1000, 
             `Laundry Room` = sum(`Laundry Room`)/1000, 
             `Hot water/AC` = sum(`Hot water/AC`)/1000, 
             Other = sum(Other)/1000, 
             Total = sum(Total)/1000)
granular_data$Month$ds <- lubridate::ymd(paste(granular_data$Month$Year, 
                                               granular_data$Month$Month, "1"))

#save(granular_data, file = 'granulardata.Rdata')


grans <- c('Hour','Day', 'Week', 'Month')
rooms <- c('Kitchen', 'Laundry Room', 'Hot water/AC', 'Other')

granular_data_test <- lapply(granular_data, function(x) subset(x, Year == 2010))
granular_data <- lapply(granular_data, function(x) subset(x, Year != 2010))


####prophet daily forecasts with and w/o holidays####

prophetdataday <- list()
timedf <- granular_data[['Day']]$ds
for (i in rooms) {
  prophetdataday[[i]] <- granular_data[['Day']][i]
  prophetdataday[[i]] <- cbind(timedf, prophetdataday[[i]])
  colnames(prophetdataday[[i]]) <- c('ds','y')
}

##without holidays
prophet_nohol <- lapply(prophetdataday, prophet)
#save(prophet_nohol, file = "prophet_dailymodels_nohol.Rdata")
#load("prophet_dailymodels_nohol.Rdata")

futures_nohol <- lapply(prophet_nohol, function(x) make_future_dataframe(x,periods = 365))
forecast_nohol <- map2(prophet_nohol, futures_nohol, function(x,y) predict(x,y))
forecast_nohol_futureonly <- lapply(forecast_nohol, 
                                    function(x) subset(x, lubridate::year(ds) == 2010, 
                                                       select = c(ds, yhat)))



##with holidays
prophet_h <- prophet(weekly.seasonality = TRUE, yearly.seasonality = TRUE)
prophet_h <- add_country_holidays(prophet_h, country_name = 'France')
prophet_hol <- lapply(prophetdataday, function(x) fit.prophet(m = prophet_h, df = x))
#save(prophet_hol, file = "prophet_dailymodels_hol.Rdata")
#load("prophet_dailymodels_hol.Rdata")

futures_hol <- lapply(prophet_hol, function(x) make_future_dataframe(x,periods = 365))
forecast_hol <- map2(prophet_hol, futures_hol, function(x,y) predict(x,y))
forecast_hol_futureonly <- lapply(forecast_hol, 
                                  function(x) subset(x, lubridate::year(ds) == 2010, 
                                                     select = c(ds, yhat)))

plot(prophet_hol$Kitchen, forecast_hol$Kitchen)


####creating the test dataframes for 2010####
testdata <- list()
timedf <- as.POSIXct(granular_data_test[['Day']]$ds)
for (i in rooms) {
  testdata[[i]] <- granular_data_test[['Day']][i]
  testdata[[i]] <- cbind(timedf, testdata[[i]])
  colnames(testdata[[i]]) <- c('ds','y')
}


####merge both hol and nohol predictions with testing data####
realpredicteddf <- list()
realpredicteddf <- map2(testdata, forecast_nohol_futureonly, 
                        function(x,y) merge(x,y, by='ds', all=TRUE))
realpredicteddf <- map2(realpredicteddf, forecast_hol_futureonly, 
                        function(x,y) merge(x,y, by='ds', all=TRUE))
for (i in rooms) {
  realpredicteddf[[i]]$y[realpredicteddf[[i]]$y==0] <- 0.001
}

for (i in rooms) {
  colnames(realpredicteddf[[i]]) <- c('ds', 'y', 'prophet_nohol', 'prophet_hol')
}

plot(realpredicteddf$Kitchen$ds, realpredicteddf$Kitchen$y, type='l')
lines(realpredicteddf$Kitchen$ds, realpredicteddf$Kitchen$prophet_nohol, col = 'red')
lines(realpredicteddf$Kitchen$ds, realpredicteddf$Kitchen$prophet_hol, col = 'blue')

####plotting all the plots in a single ggplot
realpredicteddf_expand <- lapply(realpredicteddf, 
                                 function(x) pivot_longer(x, 
                                                          cols = -ds, 
                                                          names_to = 'model', 
                                                          values_to = 'y'))
for (i in rooms) {
  realpredicteddf_expand[[i]]$meter <- i
}
dataexpanded <- do.call('rbind', realpredicteddf_expand)

a <- ggplot(data = dataexpanded, aes(x = ds, y = y, color = model)) + 
  geom_line() +
  facet_grid(vars(meter)) +
  scale_color_hue(labels = c('prophet with holidays', 'prophet', 'real data'))
a


####calculating stats for prophet
realpredicteddf_narm <- lapply(realpredicteddf, na.omit)
modelstats <- data_frame(meter = rooms)

modelstats$nohol_rmse <- sapply(realpredicteddf_narm, 
                                function(x) Metrics::rmse(x$y, x$prophet_nohol))
modelstats$nohol_mae <- sapply(realpredicteddf_narm, 
                               function(x) Metrics::mae(x$y, x$prophet_nohol))
modelstats$nohol_mape <- sapply(realpredicteddf_narm, 
                                function(x) Metrics::mape(x$y + 1, x$prophet_nohol + 1))
modelstats$hol_rmse <- sapply(realpredicteddf_narm, 
                              function(x) Metrics::rmse(x$y, x$prophet_hol))
modelstats$hol_mae <- sapply(realpredicteddf_narm, 
                               function(x) Metrics::mae(x$y, x$prophet_hol))
modelstats$hol_mape <- sapply(realpredicteddf_narm, 
                                function(x) Metrics::mape(x$y + 1, x$prophet_hol + 1))



####builting time series objects for other models
granular_data_hour_reduced <- granular_data[['Day']][,c('Kitchen','Laundry Room', 'Hot water/AC','Other')]
gran_data_ts <- list()
gran_data_ts2 <- list()

for (i in rooms){
  gran_data_ts[[i]] = ts(granular_data[['Day']][i], start = 2007, frequency = 365)
}

for (i in rooms){
  gran_data_ts2[[i]] = ts(granular_data[['Day']][i], start = 2007, frequency = 7)
}

####linear models
linmodels <- lapply(gran_data_ts, function(x) tslm(x ~ trend + season))
forecast_linmodels <- lapply(linmodels, function(x) forecast(x, h=365))

forecast_linmodels_preds <- list()
for (i in rooms) {
  forecast_linmodels_preds[[i]] <- as.data.frame(forecast_linmodels[[i]]$mean)
  colnames(forecast_linmodels_preds[[i]]) <- 'linmodel'
}

forecast_linmodels_preds <- map2(realpredicteddf, forecast_linmodels_preds, cbind)
forecast_linmodels_preds_narm <- lapply(forecast_linmodels_preds, na.omit)


#linmodels stats
modelstats$linmodel_rmse <- sapply(forecast_linmodels_preds_narm, function(x) Metrics::rmse(x$y, x$linmodel))
modelstats$linmodel_mae <- sapply(forecast_linmodels_preds_narm, function(x) Metrics::mae(x$y, x$linmodel))
modelstats$linmodel_mape <- sapply(forecast_linmodels_preds_narm, function(x) Metrics::mape(x$y + 1, x$linmodel + 1))


####arima auto
autoarimas <- lapply(list(Kitchen = gran_data_ts$Kitchen, 
                          `Laundry Room` = gran_data_ts$`Laundry Room`, 
                          `Hot water/AC` = gran_data_ts$`Hot water/AC`), 
                     function(x) auto.arima(x))
  forecast_autoarimas <- lapply(autoarimas, function(x) forecast(x, h=365))

forecast_autoarimas_preds <- list()
for (i in rooms[1:3]) {
  forecast_autoarimas_preds[[i]] <- as.data.frame(forecast_autoarimas[[i]]$mean)
  colnames(forecast_autoarimas_preds[[i]]) <- 'autoarima'
}    

realpredicteddf2 <- list(Kitchen = realpredicteddf$Kitchen, 
                         `Laundry Room` = realpredicteddf$`Laundry Room`, 
                         `Hot water/AC` = realpredicteddf$`Hot water/AC`)
forecast_autoarimas_preds <- map2(realpredicteddf2, forecast_autoarimas_preds, cbind)
forecast_autoarimas_preds_narm <- lapply(forecast_autoarimas_preds, na.omit)

#autoarima stats
modelstats$autoarima_rmse <- c(sapply(forecast_autoarimas_preds_narm, function(x) Metrics::rmse(x$y, x$autoarima)), 0)
modelstats$autoarima_mae <- c(sapply(forecast_autoarimas_preds_narm, function(x) Metrics::mae(x$y, x$autoarima)), 0)
modelstats$autoarima_mape <- c(sapply(forecast_autoarimas_preds_narm, function(x) Metrics::mape(x$y + 1, x$autoarima + 1)), 0)




####holt-winters
holtwinterss <- lapply(gran_data_ts, function(x) HoltWinters(x))
forecast_holtwinters <- lapply(holtwinterss, function(x) forecast(x, h=365))

forecast_holtwinters_preds <- list()
for (i in rooms) {
  forecast_holtwinters_preds[[i]] <- as.data.frame(forecast_holtwinters[[i]]$mean)
  colnames(forecast_holtwinters_preds[[i]]) <- 'holtwinters'
}

forecast_linmodels_preds <- map2(forecast_linmodels_preds, forecast_holtwinters_preds, cbind)
forecast_linmodels_preds_narm <- lapply(forecast_linmodels_preds, na.omit)

modelstats$holtwinters_rmse <- c(sapply(forecast_linmodels_preds_narm, function(x) Metrics::rmse(x$y, x$holtwinters)))
modelstats$holtwinters_mae <- c(sapply(forecast_linmodels_preds_narm, function(x) Metrics::mae(x$y, x$holtwinters)))
modelstats$holtwinters_mape <- c(sapply(forecast_linmodels_preds_narm, function(x) Metrics::mape(x$y + 1, x$holtwinters + 1)))





toexport <- granular_data$Month
toexport2 <- gather(toexport, key = 'meter', value = 'consumption', Kitchen, `Laundry Room`, `Hot water/AC`, `Other`)
toexport$id <- as.double(row.names(toexport))
write.csv(toexport2, file = 'monthlydata2.csv')




kitchen_day <- ts(gran_data_bymeter[['Day']][["Kitchen"]]$consumption, frequency = 365, start = 2007)
fit_kmin <- tslm(window(gran_data_tss[['Hour']][['Kitchen']],start = gran_data_tss[['Hour']][['Kitchen']][1], end=gran_data_tss[['Hour']][['Kitchen']][2000]) ~ trend + season)
summary(fit_kmin)
forecast_kmin <- forecast(fit_kmin, h=500)


####other stuff####

# gran_data_tss <- list()
# freqs <- c(365*24*60,365*24,365,12)
# dff <- data_frame(grans,freqs)
# 
# 
# for (i in 1:4) {
#   for (j in c('Kitchen', 'Laundry Room', 'Hot water/AC', 'Other')) {
#     gran_data_tss[[dff$grans[i]]][[j]] <- ts(gran_data_bymeter[[i]][[j]]$consumption, frequency = dff$freqs[i], start = 2007)
#   }
# }


##generating time series data example
kitchen_day <- ts(gran_data_bymeter[['Day']][["Kitchen"]]$consumption, frequency = 365, start = 2007)
kitchen_month <- ts(gran_data_bymeter[['Month']][["Kitchen"]]$consumption, frequency = 12, start = 2007)
laundry_month <- ts(gran_data_bymeter[['Month']][["Laundry Room"]]$consumption, frequency = 12, start = 2007)

####time series fit####

gran_data_tss$Minute$Kitchen
fit_kd <- tslm(gran_data_tss$Day$Kitchen ~ trend + season)
summary(fit_kd)
forecast_kd <- forecast(fit_kd, h=50)
plot(forecast_kd)

fit_km <- tslm(gran_data_tss[['Month']][['Kitchen']] ~ trend + season)
summary(fit_km)
forecast_km <- forecast(fit_km, h=50)
plot(forecast_km)

fit_kmin <- tslm(window(gran_data_tss[['Hour']][['Kitchen']],start = gran_data_tss[['Hour']][['Kitchen']][1], end=gran_data_tss[['Hour']][['Kitchen']][2000]) ~ trend + season)
summary(fit_kmin)
forecast_kmin <- forecast(fit_kmin, h=500)
plot(forecast_kmin)

fit_lm <- tslm(laundry_month ~ trend + season)
summary(fit_lm)
forecast_lm <- forecast(fit_lm, h=50)
plot(forecast_lm)




kitchen_mont2 <- gran_data_bymeter[['Month']][["Kitchen"]]$consumption
kitchen_mont3 <- gran_data_bymeter[['Month']][["Kitchen"]]$Month
kmtest <- data_frame(kitchen_mont3,kitchen_mont2)
kmgr <- kmtest %>% group_by(kitchen_mont3) %>% summarize(cons = mean(kitchen_mont2))
plot(kmgr$cons, type = 'l')


####prophecy####
meter_total_byday$Date <- as.POSIXct(meter_total_byday$Date)
meter_total_byday_pr <- meter_total_byday[,c(1,5)]
colnames(meter_total_byday_pr) <- c('ds','y')

prophettest <- prophet(meter_total_byday_pr)

future <- make_future_dataframe(prophettest, periods = 365)
forecast <- predict(prophettest,future)

plot(prophettest, forecast)
