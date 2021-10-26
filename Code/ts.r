install.packages('fpp2')
library(fpp2)
install.packages('uroot')
library(uroot)
library(forecast)

#set the path in the below function setwd() to the project folder path in your local system before running the script
setwd("C:\\Users\\Kiruba\\Data Analytics\\Stats\\Statistical_analysis")

TimeSeries <- read.csv(file = "Data\\eur_data.csv")
TimeSeries
data <- TimeSeries[ ,2]
fd<-ts(data,start = c(2010,1),frequency=12)
fd
autoplot(fd) + xlab('Trade value in Millions (Euros)') + ylab('Year') + ggtitle('Time Series plot')
plot(ets(fd,model='AAA'))
par(mfrow = c(1,1))
fd.ma <- ma(fd,order=2)
plot(fd.ma)
cycle(fd)
plot(aggregate(fd,fun=mean))
boxplot(fd~cycle(fd),xlab="Month of Year", ylab="Trade Value in Millions(Euros)",names=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) 
abline(reg=lm(fd.ma~time(fd.ma)))
autoplot(ma(fd,order=2))
autoplot(fd)
autoplot(decompose(fd))
plot(decompose(diff(fd)))
plot(fd)
df <- diff(fd) # d = 1
plot(df)
adf.test(df)
acf(fd)
pacf(fd) # q = 0
acf(diff(fd))
pacf(diff(fd)) # p = 0

fd
start(fd)
end(fd)
plot(decompose(fd,"additive"))
df.hw <- hw(fd,seasonal='additive')#2736.581
summary(df.hw)

autoplot(df.hw) + ylab('Trade value in million (Euro)')
par(mfrow=c(1,1))
ses.fd <- ses(fd,h=3*12)
summary(ses.fd)
plot(ses.fd)
ets.fd <- ets(fd,model='ZZZ')#2723.344
summary(ets.fd)
plot(ets.fd)
#aRIMA
fd.arima <- arima(fd, c(2, 1, 1),seasonal = list(order = c(0, 1, 1), period = 3))
pred <- predict(fd.arima, n.ahead = 2*12)
ts.plot(fd,2.718^pred$pred, lty = c(1,3))
fd.arima

ndiffs(fd)
install.packages('tseries')
library(tseries)
adf.test(fd)

#stationarity check
adf.test(fd)#fail
par(mfrow=c(2,1))
Acf(fdd)
Pacf(fdd)
ndiffs(fd)
fdd <- diff(fd)
plot(fdd)
adf.test(fdd)#pass
Acf(fdd)
Pacf(fdd)
auto.arima(fdd)
#ARIMA
ar.fd<-Arima(fd,order=c(2, 1, 0),seasonal=list(order=c(0, 1, 1), period=12))
accuracy(ar.fd)

summary(ar.fd)#2654.16
forecast(ar.fd,3*12)
autoplot(forecast(ar.fd, 3*12)) 
qqnorm(ar.fd$residuals)
qqline(ar.fd$residuals)
Box.test(ar.fd$residuals, type = "Ljung-Box")
fit <-auto.arima(fd)
fit

checkresiduals(ar.fd)








#For SES
ses.fd<-ses(fd, h=3*12)
autoplot(ses.fd) + autolayer (fitted(ses.fd),series = "Fitted")
summary(ses.fd)#2889

#holt's

TimeSeries.holt<-hw(fd, model = "AAA", h=3*12)
autoplot(TimeSeries.holt) + autolayer (fitted(TimeSeries.holt),series = "Fitted")
summary(TimeSeries.holt)

auto.arima(df)
#ARIMA
ndiffs(fd)
ar<-arima(df,order=c(2,0,0)) #2380.13
accuracy(ar)
summary(ar)
forecast(ar,3*12)
autoplot(forecast(ar, 3*12)) + autolayer (fitted(ar),series = "Fitted")
 qqnorm(ar$residuals)
qqline(ar$residuals)
Box.test(ar$residuals, type = "Ljung-Box")
fit <-auto.arima(fd)
fit

