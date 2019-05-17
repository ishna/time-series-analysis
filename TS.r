# time-series-analysis
file=read.table("C:/Users/Ishna/Desktop/Minor sem 6.csv",header=TRUE,sep=",")
f=data.frame(file)
f
library(TTR)
library(stats)
#TTR stands for Technical Trading Rules
set.seed(993)
series=f$Consumption..in.metric.tonnes[1:64]
yts=ts(series,frequency=12,start=c(2008,4))
yts
plot.ts(yts)
grid()
acf(yts)

components=decompose(yts)
components
plot(components)

d1=diff(series, differences=1) #differencing
yts1=ts(d1,frequency=12,start=c(2008,4))
yts1
plot.ts(yts1)
grid()
simple=SMA(d1, n=12) #moving average
simple
simple=as.ts(simple)
simple=ts(simple,frequency=12, start=c(2009,3))
lines(simple,col="red")
plot(simple, type="l")
seasonallyadjusted=yts-components$seasonal
plot(seasonallyadjusted)
acf(seasonallyadjusted)
pacf(seasonallyadjusted)
acf(diff(seasonallyadjusted,differences=1))
testadj=adf.test(seasonallyadjusted ,alternative = "stationary")
testadj
test1=adf.test(diff(seasonallyadjusted,differences=1) ,alternative = "stationary")

library(tseries)
test=adf.test(yts,alternative = "stationary")
test
#null hypothesis=data not stationary. large p value means non stationarity
test1=adf.test(yts1 ,alternative = "stationary")
test1
test2=PP.test(yts1)
test2
test3=Box.test(yts1, lag=12,type = "Ljung-Box")
test3
test4=kpss.test(yts1) #reverse hypothesis
test4

d2=diff(series, differences=2)
yts2=ts(d2,frequency=12,start=c(2008,4))
yts2
plot.ts(yts2)
acf(yts2)
library(forecast)
acf(series)
acf(d1)
pacf(d1)

fit=auto.arima(yts,,max.p = 5,max.q = 5,max.P = 5,max.Q = 5,max.d = 3,seasonal = TRUE,ic = 'aicc')
predict=forecast(fit,h=8)
plot(predict)
a
testdata=f$Consumption..in.metric.tonnes[65:72]
testdata
accuracy(f=predict,x=testdata)

plot(d1)
lines(d1)


#the formal solution to your problem would be to reject the null hypothesis of a 
#stationary series when, for a given series, at least one test has a pp-value 
#below 0.05/(3M) where M is the total number of series, 3 is the 
#number of tests you perform on them, 0.050.05 is the favorite 5% significance 
#level, and the whole expression is known as Bonferroni correction for multiple 
#testing. The output does not show the pp-values with sufficient accuracy, 
#so you would need to pull them as the returned class members, 
#such as pp.test(x)$p.value. 

tsdiag(fit)
#The Box–Pierce (and Ljung–Box) test examines the Null of independently distributed
#residuals. It’s derived from the idea that the residuals of a “correctly
#specified” model are independently distributed. If the residuals are not, then
#they come from a miss–specified model.
Box.test(fit$residual, lag=3)
fit$residual
plot(fit$residual)
acf(fit$residual)
acf(d1,lag=48)

fit2=arima(yts,order=c(1,1,3),seasonal = list(order = c(1,1,0),period=12))
predict2=forecast(fit2,h=8)
plot(predict2)
testdata=f$Consumption..in.metric.tonnes[65:72]
testdata
accuracy(f=predict2,x=testdata)

fit3=arima(yts,order=c(3,1,0),seasonal = list(order = c(1,1,0),period=12)) #the ACF is exponentially decaying or sinusoidal;
fit3
predict3=forecast(fit3,h=8)
plot(predict3)
testdata=f$Consumption..in.metric.tonnes[65:72]
accuracy(f=predict3,x=testdata)

plot.ts(yts)
lines(fitted(fit),col="red")
plot.ts(yts)
lines(fitted(fit2),col="red")
plot.ts(yts)
lines(fitted(fit3),col="red")
plot.ts(yts)
lines(fitted(fit),col="green")
lines(fitted(fit2),col="red")
lines(fitted(fit3),col="blue")


