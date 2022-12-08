#	Data 1	Weekly U.S. All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)	1	Weekly	5/16/2022
# Release Date:	5/16/2022			
# Next Release Date:	5/23/2022			
library(readxl)
gas_prices <- data.frame(read_excel("~/Downloads/Project Data/EMM_EPM0_PTE_NUS_DPGw.xls"))

# Data 1	Weekly Cushing, OK WTI Spot Price FOB  (Dollars per Barrel)	1	Weekly	5/20/2022
# Release Date:	5/25/2022
# Next Release Date:	6/2/2022
oil_prices<-data.frame(read_excel("~/Downloads/Project Data/RWTCw.xls"))
attach(gas_prices)
attach(oil_prices)
class(gas_prices)

gas_prices.ts<-ts(gas_prices[,2], start=c(1993,14),freq=52)
gas_prices_annual.ts<-aggregate(gas_prices.ts,FUN=mean)

oil_prices.ts<-ts(oil_prices[,2],start=1993-14, freq=52)
# Had to manipulate original dataset in excel to take out 1986-1993(March Values)

layout(1:2)
plot(gas_prices_annual.ts)
boxplot(gas_prices.ts~cycle(gas_prices.ts)) # seasonal values
layout(1:1)

gas_prices.decom<-decompose(gas_prices.ts)  # additive relationship
oil_prices.decom<-decompose(oil_prices.ts)
gas_prices_random<-gas_prices.decom$random
oil_prices_random<-oil_prices.decom$random

plot(gas_prices.decom)
Trend<-gas_prices.decom$trend
Seasonal<-gas_prices.decom$seasonal
ts.plot(Trend, 
        Trend+Seasonal,lty=1:2) # Trend superimposed with seasonal effect
plot(stl(gas_prices.ts,s.window=27))    # Having difficulty using loess     
                                  # Series Starts in April ends in April(maybe)
Newtime<-time(gas_prices.ts)
plot(gas_prices.ts,xlab='Time(Weeks)',ylab='Gasoline Prices',cex=0.8);abline(lm(gas_prices.ts~Newtime))

# Autocorrelations
layout(1:1)
plot(gas_prices.decom$random)
acf(gas_prices.decom$random,na.action=na.exclude) #adjusted for missing values
# Note the cosine shape

#Check to see if seasonal Adjustment has been effective
sd(gas_prices.ts)
sd(gas_prices.ts-gas_prices.decom$trend,na.rm=T)
sd(gas_prices.decom$random,na.rm=T)   # Reduction in sd. suggests seasonal has been effective

# Visualization of oil and gas price series (Does one lead the other?)
ts.plot(gas_prices.ts,oil_prices.ts, lty=c(1,2))
plot(acf(ts.union(gas_prices_random, oil_prices_random),na.action = na.exclude,lag=10))
ccf(gas_prices_random,oil_prices_random,na.action = na.exclude)
print(acf(ts.union(gas_prices_random, oil_prices_random),na.action= na.exclude))
# Note Gasoline Prices don't lag behind Oil Prices. That is Oil Price is not a leading indicator of gas prices
 

# Holt-Winters Method
###### Assuming No Systematic/ Trend Effect
gas_price_hw1<-HoltWinters(gas_prices.ts, beta=F, gamma=F) ; gas_price_hw1
# Alpha close to 1 suggests heavy dependence on recent observation
plot(gas_price_hw1)
gas_price_hw1$SSE
gas_predict_hw<-predict(gas_price_hw1, n.ahead= 13) # Note predicted values are at the mean at the origin(T=0)
ts.plot(gas_prices.ts,gas_predict_hw,lty=1:2) 

gas_price_hw2<-HoltWinters(gas_prices.ts, alpha=0.2, beta=F, gamma=F) ; gas_price_hw1
# I assume light dependence on most recent observation 
plot(gas_price_hw2)
gas_price_hw2$SSE  # Bigger SSE than previous
predict(gas_price_hw1,n.ahead=13)

# Holt-Winters Method
###### Assuming Systematic/ Trend Effect(Of Additive type)
gas_price_hw1<-HoltWinters(gas_prices.ts, seasonal='add') 
gas_price_hw1$alpha;  gas_price_hw1$beta ;gas_price_hw1$gamma
# Alpha close to 1 suggests heavy dependence on recent observation
# Very light seasonality if any at all
# Quite strong trend effect
plot(gas_price_hw1)
plot(gas_price_hw1$fitted)
sqrt(gas_price_hw1$SSE/length(gas_prices.ts))
gas_predict_hw<-predict(gas_price_hw1, n.ahead= 13)
ts.plot(gas_prices.ts,gas_predict_hw,lty=1:2)

# Multiplicative Trend Effect
gas_price_hw2<-HoltWinters(gas_prices.ts, seasonal='mult') 
gas_price_hw2$alpha;  gas_price_hw2$beta ;gas_price_hw2$gamma
# Alpha close to 1 suggests heavy dependence on recent observation
# Very strong seasonal effect
# Quite strong trend effect
plot(gas_price_hw2)
plot(gas_price_hw2$fitted)
sqrt(gas_price_hw2$SSE/length(gas_prices.ts))
# Note the MSE in the additive model is smaller than that of the seasonal.
##### Forecast (6-month forecast)
gas_predict_hw<-predict(gas_price_hw2, n.ahead= 13)
ts.plot(gas_prices.ts,gas_predict_hw,lty=1:2)

## Compare SSE in Multiplicative Trend vs Additive
c(gas_price_hw1$SSE,gas_price_hw2$SSE)   # Trend appears additive

## Random-Walk with Drift Test
gas_price_hw<-HoltWinters(gas_prices.ts,alpha=1,gamma=F) ; gas_price_hw$beta  # Somewhat strong trend
acf(resid(gas_price_hw))   # Not a white noise series
acf(diff(gas_prices.ts))    # Significant lag 2,3 etc. autocorrelations show the random-walk may not be a good approximation
gas_ar<-ar(gas_prices.ts,method='mle')  # Using the AIC criterion
gas_ar$order ; gas_ar$ar
 acf(gas_ar$res[-(1:gas_ar$order)])  # Correlogram for the AR(5) model fitted to the DATA

#AR for Annualized Series
gas_ar<-ar(aggregate(gas_prices.ts,FUN=mean),method='mle') 
gas_ar$order; gas_ar$ar  # AR(1) for the annual gas prices
acf(gas_ar$res[-(1:gas_ar$order)],lag=50)   ## Approximately white noise  # Trends are purely stochastic


## Seasonal Model(No-Intercept)
Seas<-cycle(gas_prices.ts)
Time<-time(gas_prices.ts)
gas.lm<-lm(gas_prices.ts~0+ Time+factor(Seas))   # No intercept
new.t<-seq(2022.462,len=13, by= 1/52) 
new.dat<- data.frame(Time=new.t, Seas=rep(1:52,1))
predict(gas.lm,new.dat)[1:13]

### Harmonic Model
SIN<-COS<- matrix(nr=length(time(gas_prices.ts)),nc=6)
for (i in 1:6){
  COS[,i]<- cos(2*pi*i*time(gas_prices.ts))
   SIN[,i]<-sin(2*pi*i*time(gas_prices.ts))
}
TIME<-(time(gas_prices.ts)-mean(time(gas_prices.ts)))/sd(time(gas_prices.ts))
gp_lm1<-lm(gas_prices.ts~TIME+I(TIME^2)+COS[,1]+SIN[,1]+COS[,2]+SIN[,2]+COS[,3]+SIN[,3]+
             + COS[,4]+SIN[,4]+COS[,5]+SIN[,5]+COS[,6]+SIN[,6])
#Model selection using T- ratio
coef(gp_lm1)/sqrt(diag(vcov(gp_lm1)))
gp_lm2~ lm(gas_prices.ts~ TIME + I(TIME^2) + COS[, 1] + SIN[, 1] + 
             COS[, 2] + SIN[, 2] +COS[, 3] + SIN[, 3] + +COS[, 4] + 
             SIN[, 4] + COS[, 5] + SIN[, 5] + COS[, 6] + SIN[, 6])
# Or 
step(gp_lm1)
gp_lm2<- lm(gas_prices.ts~ TIME + I(TIME^2) + COS[, 1] + SIN[, 1]  + SIN[, 2])
plot(time(gas_prices.ts),resid(gp_lm2),type='l')
abline(0,0,col='red')
acf(resid(gp_lm2));pacf(resid(gp_lm2))
res.ar<-ar(resid(gp_lm2),method='mle') ; res.ar$ar ;res.ar$order
acf(res.ar$res[-(1:9)])

# Prediction Plot 
new.t<-time(ts(st=c(2022,13),end=c(2022,25),fr=52))
TIME<-(new.t-mean(time(gas_prices.ts)))/sd(time(gas_prices.ts))
SIN<-COS<- matrix(nr=length(new.t),nc=6)
for (i in 1:6){
  COS[,i]<- cos(2*pi*i*new.t)
  SIN[,i]<-sin(2*pi*i*new.t)
}
new.dat<-data.frame(TIME=as.vector(TIME),SIN=SIN,COS=COS)
GP.pred.ts<- ts(predict(gp_lm2,new.dat),st=c(2022,13),fr=52)
ts.plot(log(gas_prices.ts),log(GP.pred.ts),lty=1:2)
ts.plot(gas_prices.ts,GP.pred.ts,lty=1:2)
# Prediction Values (13-week)
GP.pred.ts

### Log Transform  # Use Time from harmonic model
gp_lm1<-lm(log(gas_prices.ts)~TIME+I(TIME^2)+I(TIME^3)+COS[,1]+SIN[,1]+COS[,2]+SIN[,2]+COS[,3]+SIN[,3]+
             + COS[,4]+SIN[,4]+COS[,5]+SIN[,5]+COS[,6]+SIN[,6])
step(gp_lm1)
gp_lm2<-lm(log(gas_prices.ts) ~  TIME + I(TIME^2) + I(TIME^3) + 
             COS[, 1] + SIN[, 1] + COS[, 2] + SIN[, 2])
acf(gp_ar$resid[-(1:gp_ar$order)])
# Prediction Plot 
new.t<-time(ts(st=c(2022,13),end=c(2022,25),fr=52))
TIME<-(new.t-mean(time(gas_prices.ts)))/sd(time(gas_prices.ts))
SIN<-COS<- matrix(nr=length(new.t),nc=6)
for (i in 1:6){
  COS[,i]<- cos(2*pi*i*new.t)
  SIN[,i]<-sin(2*pi*i*new.t)
}
new.dat<-data.frame(TIME=as.vector(TIME),SIN=SIN,COS=COS)
GP.pred.ts<- exp(ts(predict(gp_lm2,new.dat),st=c(2022,13),fr=52))
# Prediction using correction factor
empirical.correction.factor<-mean(exp(resid(gp_lm2)))
GP.pred.ts<-GP.pred.ts*empirical.correction.factor
GP.pred.ts                    # Prediction Values (13-week)
ts.plot(log(gas_prices.ts),log(GP.pred.ts),lty=1:2)
ts.plot(gas_prices.ts,GP.pred.ts,lty=1:2)
ts.plot(cbind(window(gas_prices.ts,start=c(2010,1)),GP.pred.ts),lty=1:2)

# ARIMA (1,1,1)
source("~/Downloads/best_aic.R", echo=TRUE)
gas_p_arima<-get.best.arima(log(gas_prices.ts),maxord=rep(2,6)) # Obtain best fitting model
best_fit<-gas_p_arima[[2]]  #Obtain fit parameters
acf(resid(best_fit))
gas_p_arima[[3]] # Order
ts.plot(cbind(window(gas_prices.ts,start=c(2010,1)),exp(predict(best_fit,13)$pred)),lty=1:2)
exp(predict(best_fit,13)$pred) # Obtain predicted values

# Garch model
library(tseries)
gas_res<-resid(gas_p_arima[[2]])
acf(gas_res); acf(gas_res^2)
gas_garch<-garch(gas_res,trace=F)
gas_garch_res<-resid(gas_garch)[-1]
acf(gas_garch_res); acf(gas_garch_res^2)






