###################
##	Final Project #
###################
setwd("/Users/lydia/Dropbox/Course/STAT 2320/final exam")

Question 1
#################seasonal ARIMA
data1=read.csv("alc.csv")
alc=ts(data1,start=2002,f=12)
plot(y=alc,x=as.vector(time(alc)),ylab='Alcohol wholesale (x$1,000,000)',xlab='Time',type='o',pch=as.vector(season(alc)),main='Montly Alcohol Wholesale in the U.S. \n From 2002 to 2008')

plot(y=diff(alc),x=as.vector(time(diff(alc))),ylab='Difference',xlab='Time',type='o',pch=as.vector(season(diff(alc))),main='First Difference')
plot(y=diff(diff(alc),12),x=as.vector(time(diff(diff(alc),12))),ylab='Seasonal difference', xlab='Time',type='o',pch=as.vector(season(diff(diff(alc),12))),main='Seasonal Difference')

par(mfrow=c(1,2)) 
acf(as.vector(diff(diff(alc),12)), 48,main='ACF')
pacf(as.vector(diff(diff(alc),12)), 48,main='PACF')

#fit AR(2) to seasonal diff
amod1=arima(alc,order=c(2,1,0),seasonal=list(order=c(0,1,0), period=12))
amod1

#diagnostics
tsdiag(amod1)
pacf(residuals(amod1),main='PACF of residuals')
qqnorm(residuals(amod1))
qqline(residuals(amod1))
runs(residuals(amod1))
shapiro.test(residuals(amod1))

#overfit
amod1
amod2=arima(alc,order=c(3,1,0),seasonal=list(order=c(0,1,0), period=12))
amod2
amod3=arima(alc,order=c(2,1,1),seasonal=list(order=c(0,1,0), period=12))
amod3

#forecasting
pred=plot(amod1,n.ahead=12,n1=2006)$pred  
lower=plot(amod1,n.ahead=12,n1=2006)$lpi 
upper=plot(amod1,n.ahead=12,n1=2006)$upi 
past=ts(alc[time(alc)>=2006],start=2006,f=12)  
future=ts(c(past[length(past)],pred),start=2008.917,f=12) 

plot(past,type="o",xlim=c(2006,2010),ylim=c(min(c(pred,lower)-4),max(c(pred,upper))),lwd=2,ylab='Alcohol wholesale (x$1,000,000)',xlab='Time',main='Montly Alcohol Wholesale in the U.S. Beginning in 2006 \n Forecasted 1 Year into the Fugure')
lines(future,lty=2,col="tomato",type="l",lwd=2)
points(pred,col="red",pch=as.vector(season(alc)))  
points(lower,lty=3,col="slateblue4",type="o",lwd=2)
points(upper,lty=3,col="slateblue4",type="o",lwd=2)






###############
Question 2	###
###############

#read in data and plot
data2=read.csv("homicide.csv")
sumhom=ts(data2$sumhom,start=1999,f=12)
plot(sumhom,ylab='Monthly homicides',xlab='Time',type='o',main='Monthly Homicides in Cali, Colombia \n From Jan 1999 to Aug 2008')

#take difference to get rid of the trend
par(mfrow=c(2,1))
plot(diff(sumhom),main='First Difference')
plot(diff(diff(sumhom)),main='Second Difference')

#look at ACF and PACF of first differencing data
acf(as.vector(diff(sumhom)),main='ACF')
pacf(as.vector(diff(sumhom)),main='PACF')
eacf(as.vector(diff(sumhom)))

#try ARIMA(0,1,1) model first
smod1=arima(sumhom, order=c(0,1,1))
smod1

##diagnostics
tsdiag(smod1)
pacf(residuals(smod1),main='PACF of residuals')
qqnorm(rstandard(smod1),)
qqline(rstandard(smod1))
runs(rstandard(smod1))
shapiro.test(rstandard(smod1))

##overfitting, ARIMA(1,1,1) and ARIMA(0,1,2)
smod1
smod2=arima(sumhom,order=c(1,1,1))
smod2
smod3=arima(sumhom,order=c(0,1,2))
smod3

#forecasting
pred=plot(smod1,n.ahead=16,n1=2006)$pred  
lower=plot(smod1,n.ahead=16,n1=2006)$lpi 
upper=plot(smod1,n.ahead=16,n1=2006)$upi 
past=ts(sumhom[time(sumhom)>=2006],start=2006,f=12)  
future=ts(c(past[length(past)],pred),start=2008.583,f=12) 

plot(past,type="o",xlim=c(2006,2010),ylim=c(min(c(pred,lower)-4),max(c(pred,upper))),lwd=2,xlab="Time",ylab="Monthly homicides",main="Monthly Homicides in Cali, Colombia Beginning in 2005: \n Forecasted 16 months into the future")
lines(future,lty=2,col="tomato",type="l",lwd=2)
points(pred,col="red")  
points(lower,lty=3,col="slateblue4",type="o",lwd=2)
points(upper,lty=3,col="slateblue4",type="o",lwd=2)





###############
Question 3	###
###############seasonal ARIMA, P=1,Q=0. maybe p=0,q=1

data3=scan("erie.dat")
erie=ts(data3,start=1921,f=12)
plot(erie,ylab='Lake Erie levels',xlab='Time',type='o',main='Monthly Lake Erie Levels \n From 1921 to 1970')
plot(y=erie,x=as.vector(time(erie)),ylab='Lake Erie levels',xlab='Time',type='o',pch=as.vector(season(erie)),main='Monthly Lake Erie Levels \n From 1950 to 1970',xlim=c(1950,1970))

#take difference to get rid of the trend
plot(diff(erie),ylab='Difference',xlab='Time',type='o',main='First Difference')
plot(y=diff(erie),x=as.vector(time(diff(erie))),ylab='Difference',xlab='Time',type='o',pch=as.vector(season(diff(erie))),main='First Difference from 1950 to 1970',xlim=c(1950, 1970))

#take seasonal difference to get rid of the seasonal trend
plot(diff(diff(erie),12),ylab='Seasonal difference',xlab='Time',type='o',main='Seasonal Difference')
plot(diff(diff(erie),12),x=as.vector(time(diff(diff(erie),12))),ylab='Seasonal difference',xlab='Time',type='o',pch=as.vector(season(diff(diff(erie),12))),main='Seasonal Difference from 1950 to 1970',xlim=c(1950, 1970))

#look at ACF and PACF of seasonal differencing data
par(mfrow=c(2,1))
acf(as.vector(diff(diff(erie),12)),48,main='ACF')
pacf(as.vector(diff(diff(erie),12)),48,main='PACF')

#try out different models for regular arima part
emod1=arima(erie,order=c(1,1,0),seasonal=list(order=c(0,1,1), period=12))
emod1
emod2=arima(erie,order=c(1,1,1),seasonal=list(order=c(0,1,1), period=12))
emod2
emod3=arima(erie,order=c(0,1,1),seasonal=list(order=c(0,1,1), period=12))
emod3
tsdiag(emod1)
tsdiag(emod3)

#diagnostics
pacf(residuals(emod1),main='PACF of residuals')
qqnorm(residuals(emod1))
qqline(residuals(emod1))
runs(residuals(emod1))
shapiro.test(residuals(emod1))



#overfitting
emod4=arima(erie,order=c(2,1,0),seasonal=list(order=c(0,1,1), period=12))
emod4
emod5=arima(erie,order=c(1,1,0),seasonal=list(order=c(0,1,2),period=12))
emod5
emod6=arima(erie,order=c(1,1,0),seasonal=list(order=c(1,1,1),period=12))
emod6


#forecasting
plot(emod1,n.ahead=36,n1=1968)
pred=plot(emod1,n.ahead=36,n1=1968)$pred 
pred 
lower=plot(emod1,n.ahead=36,n1=1968)$lpi 
lower
upper=plot(emod1,n.ahead=36,n1=1968)$upi 
upper
past=ts(erie[time(erie)>=1968],start=1968,f=12)  
future=ts(c(past[length(past)],pred),start=1970.917,f=12) 

plot(past,type="o",xlim=c(1968,1974),ylim=c(min(c(pred,lower)-4),max(c(pred,upper))),lwd=2,xlab="Time",ylab="Lake Erie level",main="Lake Erie Level Beginning in 1968: \n Forecasted three years into the future")
lines(future,lty=2,col="tomato",type="l",lwd=2)
points(pred,col="red")  
points(lower,lty=3,col="slateblue4",type="o",lwd=2)
points(upper,lty=3,col="slateblue4",type="o",lwd=2)



###############
Question 4	###
###############

data4=scan("methane.dat")
data5=scan("co2.dat")
methane=ts(data4,f=1/9)
co2=ts(data5,f=1/9)
par(mfrow=c(2,1))
plot(methane, ylab='methane input (cu. ft/min)',xlab='Second',main='Methane Input')
plot(co2,ylab='co2 percentage',xlab='Second',main='CO2 Percentage in Output')


ccfvalues=prewhiten(as.vector(methane),as.vector(co2),main='methane & co2')
par(mfrow=c(2,1))
acf(as.vector(co2),main='ACF of CO2')
pacf(as.vector(co2),main='PACF of CO2')

lagdat=cbind(co2,c1=lag(co2,-1),c2=lag(co2,-2),methane,m3=lag(methane,-3),m4=lag(methane,-4),m5=lag(methane,-5),m6=lag(methane,-6),m7=lag(methane,-7))

fit1=lm(co2~c1+c2+m3+m4+m5+m6+m7,data=lagdat)
summary(fit1)

fit2=lm(co2~c1+c2+m3+m4+m5+m6,data=lagdat)
summary(fit2)

fit3=lm(co2~c1+c2+m3+m4+m5,data=lagdat)
summary(fit3)

fit4=lm(co2~c1+c2+m3+m4,data=lagdat)
summary(fit4)

fit5=lm(co2~c1+c2+m3,data=lagdat)
summary(fit5)


#diagnostics
par(mfrow=c(1,2))
plot(rstudent(fit4),ylab='Standardized residuals',xlab='Time',main='Residual Plot of fit4')
plot(rstudent(fit5),ylab='Standardized residuals',xlab='Time',main='Residual Plot of fit5')

par(mfrow=c(2,2))
acf(rstudent(fit4),main='ACF of fit4')
pacf(rstudent(fit4),main='PACF of fit4')
acf(rstudent(fit5),main='ACF of fit5')
pacf(rstudent(fit5),main='PACF of fit5')

hist(rstudent(fit4),main='Histogram of fit4')
hist(rstudent(fit5),main='Histogram of fit5')
qqnorm(rstudent(fit4),main='Q-Q plot of fit4')
qqline(rstudent(fit4))
qqnorm(rstudent(fit5),main='Q-Q plot of fit5')
qqline(rstudent(fit5))

runs(rstudent(fit4))
shapiro.test(rstudent(fit4))
runs(rstudent(fit5))
shapiro.test(rstudent(fit5))


acf(rstudent(fit3),main='ACF of fit4')
pacf(rstudent(fit3),main='PACF of fit4')
runs(rstudent(fit2))
shapiro.test(rstudent(fit2))






