# Gas Prices -- Pure Time Series 
library(readxl) ; library(fracdiff); library(fUnitRoots); library(TSA); library(fGarch);
source("~/Downloads/Time Series/backtest.R")
gas_prices <- data.frame(read_excel("~/Downloads/Project Data/EMM_EPM0_PTE_NUS_DPGw.xls"))
oil_prices<-data.frame(read_excel("~/Downloads/Project Data/RWTCw.xls"))
gas_price<- ts(gas_prices[,2], start=c(1993,14),fr=52)
gas_price_annual<-aggregate(gas_price,FUN=mean)
tdx<-c(1:length(gas_price))/52 +1993
layout(1:2)
plot(tdx,gas_price,xlabel='year',ylabel='gas prices',type='l'); abline(lm(gas_price~tdx)) 
title(main='(a) Weekly Gas Price')

tdx1<-c(1:length(gas_price_annual)) + 1993
plot(tdx1,gas_price_annual,xlabel='year',ylabel='gas prices',type='l')  
title(main='(b) Annual Gas Price')

source('~/Downloads/Time Series/ch1data/ma.R') # R documentation provided by Ruey Tsay
x1<-as.numeric(gas_price)
ma(x1,10) # Moving average for past 10 weeks

dgas_price<-diff(gas_price)
doil_price<-diff(oil_prices[,2])
acf(dgas_price,lag=110)
pacf(dgas_price,lag=110)

mm1<-ar(dgas_price,method='mle')  # Order Selection Using AIC Criterion - AR model
mm1$order #Find order using the AIC criterion #order=3

# Unit Root Tests
log_gas<-log(gas_price)  #compute weekly log gas prices
m1<-ar(diff(log_gas),method='mle')
m1$order
adfTest(log_gas,lags=9,type=('ct')) 
# Cannot Reject H0 
# Suggests log-prices do follow a unit-root process with a time trend

# AR Model
m1<-arima(dgas_price,order=c(3,0,0))  
m1   # Notice coefficient on ar2, and intercept is non-significant at any conf.level alpha
c1<-c(NA,0,NA)
t.test(dgas_price) # Mean growth differs from zero
m1<-arima(dgas_price,order=c(3,0,0),include.mean=F,fixed=c1) 
m1
p1<-c(1,-m1$coef[1:3])
r1<-polyroot(p1); r1 # Characteristic roots. Notice they're real-valued. 
                      #In other words the model can be regarded as AR models layered on to each other.
tsdiag(m1,gof=60); Box.test(m1$residuals,lag=60,type='Ljung')
pv<- 1-pchisq(9.6488,9)
pv

# MA model
# Order Selection Using ACF
acf(dgas_price,lag=20)   # Order 6
m2<- arima(dgas_price,order=c(0,0,6))
m2   # All terms are significant(Except intercept)
m2<- arima(dgas_price,order=c(0,0,6),include.mean = F)
m2
tsdiag(m2,gof=60); Box.test(m2$residuals,lag=60,type='Ljung')

#ARMA model
m3 <-eacf(dgas_price,6,12) # X denotes the modulus of the EACF is > or = to 2*S.E
# Pattern of 0's suggest ARMA(4,1), (1,3) (2,3) (3,3) (4,3)
print(m3$eacf,digits= 2) 
se<-2/sqrt(length(dgas_price))# Compare each of suggested to 2/sqrt(length(dgas_price)) = 2/sqrt(TT)
# Using the 1% CI as the selection criteria, narrow down to ARMA (4,1) (1,3) 
# Select (1,3) for simplicity
m3<-arima(dgas_price,order=c(1,0,3))
m3  # Intercept and ma2 terms are non-significant
c2<- c(NA,NA,0,NA)
m3<-arima(dgas_price,order=c(1,0,3),include.mean = F,fixed=c2)
m3 
tsdiag(m3,gof=60)
c2<- c(NA,NA,0,NA,NA,NA,NA)
m3<-arima(dgas_price,order=c(1,0,3),seasonal=list(order=c(0,1,3),period=52),include.mean = F,fixed=c2)  ## With seasonality
m3; tsdiag(m3,gof=60)
Box.test(m3$residuals,lag=60,type='Ljung')


time<-as.matrix(time)
# ARIMA model
m4<-arima(dgas_price, order=c(0,1,6))  # Since an MA(6) was identified for the difference series
m4; tsdiag(m4,gof=60)

# Linear with Time Series Error
m5<-lm(dgas_price~-1+doil_price)
summary(m5)
acf(m5$residuals,lag=60)
pacf(m5$residuals,lag=60) 
m5<-ar(m5$residuals,method='mle'); m5$order
m5<-arima(dgas_price,order=c(9,0,0),xreg=doil_price); m5 # Insignificant ar2,ar5 ,ar6, ar7, and intercept terms
c3<-c(NA,0,NA,NA,0,0,0,NA,NA,NA)
m5<-arima(dgas_price,order=c(9,0,0),xreg=doil_price,include.mean=F,fixed=c3); m5
tsdiag(m5,gof=60)
Box.test(m5$residuals,lag=60,type='Ljung')

# Trend Stationary
TT<-c(1:length(gas_price))
m6<-lm(gas_price~TT); summary(m6)   ### Zt is probably unit-root (High-Valued Acfs)
acf(m6$residuals,lag=60); pacf(m6$residuals,lag=60)
m6 <-eacf(gas_price,6,12); print(m6$eacf,digits=2)
se<-2/sqrt(length(gas_price)); se # Compare each of suggested to 2/sqrt(length(gas_price))
  # For simplicity choose ARMA(2,3)
m6<-arima(gas_price,order=c(2,0,3),seasonal=list(order=c(0,0,1),period=52),xreg=TT); m6
tsdiag(m6,gof=60)
Box.test(m6$residuals,lag=60,type='Ljung')

# Trend-Shift Stationary 
tt<-c(rep(0,500),TT[501:1520])
m7<-lm(gas_price~tt); summary(m7)  
acf(m7$residuals,lag=60); pacf(m7$residuals,lag=60)
m7<-arima(gas_price,order=c(2,0,3),seasonal=list(order=c(0,0,1),period=52),xreg=tt); m7
tsdiag(m7,gof=60); Box.test(m7$residuals, lag=60, type='Ljung')

# BackTests # For a 13-week horizon
pm1<- backtest(m1, dgas_price,1500,13, fixed=c1,inc.mean=F)
pm2<- backtest(m2, dgas_price,1500,13, inc.mean=F)
pm3<- backtest(m3,dgas_price, 1500,13, fixed=c2, inc.mean=F)
pm4<- backtest(m4, dgas_price,1500,13)
c3<-c(NA,0,NA,NA,0,0,0,NA,NA,NA,NA)
pm5<- backtest(m5, dgas_price,1500,13,xre=doil_price,fixed=c3)  #### Not working
TT<-as.matrix(TT);tt<-as.matrix(tt)
pm6<-backtest(m6,gas_price,1500,13, xre=TT)
pm7<-backtest(m7,gas_price,1500,13,xre=tt)

# Fits
pm1fit<-dgas_price[1501:1519]-pm1$error
pm2fit<-dgas_price[1501:1519]-pm2$error
pm3fit<-dgas_price[1501:1519]-pm3$error
pm4fit<-dgas_price[1501:1519]-pm4$error
pm5fit<-dgas_price[1501:1519]-pm5$error
pm6fit<- gas_price[1501:1520]-pm6$error
pm7fit<- gas_price[1501:1520]-pm7$error

# Plots
plot(tdx[1501:1519],dgas_price[1501:1519],xlab='year',ylab='Growth', type='l')
points(tdx[1501:1519],pm1fit,pch='*')

plot(tdx[1501:1519],dgas_price[1501:1519],xlab='year',ylab='Growth', type='l')
points(tdx[1501:1519],pm2fit,pch='*')

plot(tdx[1501:1519],dgas_price[1501:1519],xlab='year',ylab='Growth', type='l')
points(tdx[1501:1519],pm3fit,pch='*')

plot(tdx[1501:1519],dgas_price[1501:1519],xlab='year',ylab='Growth', type='l')
points(tdx[1501:1519],pm4fit,pch='*')

plot(tdx[1501:1519],dgas_price[1501:1519],xlab='year',ylab='Growth', type='l')
points(tdx[1501:1519],pm5fit,pch='*')

plot(tdx[1501:1520],gas_price[1501:1520],xlab='year',ylab='G_Prices', type='l')
points(tdx[1501:1520],pm6fit,pch='*')

plot(tdx[1501:1520],gas_price[1501:1520],xlab='year',ylab='G_Prices', type='l')
points(tdx[1501:1520],pm7fit,pch='*')


# Predictions Using Best Models from -- 13-week(3-month) window
pred_m1<-predict(m1, n.ahead= 13)
tdx<-c(1:13)
upp<- pred_m1$pred + 2*pred_m1$se
low<- pred_m1$pred -2*pred_m1$se
min(low);max(upp)
plot(tdx,pred_m1$pred, xlab='weeks ahead(Starting May 23)',ylab='growth',type='l',
     ylim=c(-0.09707274, 0.1744164))
points(tdx,pred_m1$pred,pch=16); lines(tdx,low,lty=2);lines(tdx,upp,lty=2)

pred_m2<-predict(m2,n.ahead=13)
pred_m3<-predict(m3, n.ahead= 13)
pred_m4<-predict(m4,n.ahead=13)
pred_m5<- predict(m5,n.ahead=13) ## Not working
pred_m6<- predict(m6,n.ahead=13)
pred_m7<-predict(m7, n.ahead=13)


#### Volatility
### Arch(6) Model
acf(abs(dgas_price),lag=60)    # Significant dependence in the series
Box.test(abs(dgas_price),lag=60,type='Ljung')
source("~/Downloads/An-introduction-to-analysis-of-financial-data-with-R-master/archTest.R")
y<-dgas_price-mean(dgas_price)
Box.test(y^2,lag=60,type="Ljung") # Significant ARCH effect
archTest(y,60)
m1<-garchFit(~1+garch(6,0),data=dgas_price,trace=F); summary(m1)
resi<-residuals(m1,standardize=T)
tdx<-c(1:length(resi))/52 +1993
par(mfcol=c(3,1))
plot(tdx,resi,xlab='year',ylab='standard-resi',type='l')
acf(resi, lag=60); pacf(resi^2,lag=60)
plot(m1)
m1<-garchFit(~1+garch(6,0),data=dgas_price,trace=F,cond.dist = 'std');summary(m1) 
#  With Student-t Innovations # No skew
m1<-garchFit(~1+garch(6,0),data=dgas_price,trace=F,cond.dist = 'sstd');summary(m1) 
#  With Student-t Innovations # With skew

#### Garch Model
m2<-garchFit(~1+garch(1,5),data=dgas_price,trace=F);summary(m2)
v1<-volatility(m2)
resi<-residuals(m2,standardized=T)
vol<- ts(v1,freq=52, start=c(1993,14))
res<-ts(resi,freq=52, start=c(1993,14))
par(mfcol=c(2,1))
plot(vol,xlab='year',ylab='volatility',type='l') ; plot(res,xlab='year',ylab='st. resi',type='l')
par(mfcol=c(2,2))
acf(resi,lag=60);pacf(resi^2,lag=60)
par(mfcol=c(1,1))
# Conf. Intervals (for Plotting)
upp<- -5.301e-04  +2*v1 ; low<- -5.301e-04  - 2*v1
tdx<-c(1:length(dgas_price))/52 +1993
plot(tdx,dgas_price,xlab='year',ylab='simp. rtn series',type='l',ylim=c(-0.4,0.5))
lines(tdx,upp, lty=2, col='red'); lines(tdx,low, lty=2, col='red') ;abline(h=c(-5.301e-04 ))
# Student t
m3<-garchFit(~1+garch(1,5),data=dgas_price,trace=F,cond.dist = 'std');summary(m3)
v2<-volatility(m3)
# Skewed Student t
m4<-garchFit(~1+garch(1,5),data=dgas_price,trace=F,cond.dist = 'sstd');summary(m4)
v3<-volatility(m4)
par(mfcol=c(3,1))
plot(tdx,v1,xlab='year',ylab='volatility',type='l',ylim=c(-0.4,0.5))
title(main='(a) Gaussian')
plot(tdx,v2,xlab='year',ylab='volatility',type='l',ylim=c(-0.4,0.5))
title(main='(b) Student t')
plot(tdx,v3,xlab='year',ylab='volatility',type='l',ylim=c(-0.4,0.5))
title(main='(c) Skewed Student-t')

# Testing Skewness
cor(cbind(v1,v2,v3))
basicStats(dgas_price)
tt<-1.032322/sqrt(6/length(dgas_price));tt
tt<- ( 1.511e+00  -1)/6.552e-02 ;tt
pv<-2*pnorm(tt); pv   # Reject hypothesis of no skewness.
plot(m4) # QQ-plot of Std. Resid (Number 13)

# Using 2-pass estimation (Estimating Return series for residuals)
yt<-dgas_price-mean(dgas_price)
m1<-arima(yt^2,order=c(1,0,1)) ;m1 ; mean(dgas_price)
fit<-yt^2-m1$residuals
v3<-volatility(m4) # Volatility from Skew Student T
cor(v3,sqrt(fit))

# Igarch(1,1)
source("~/Downloads/An-introduction-to-analysis-of-financial-data-with-R-master/Igarch.R")
mm<-Igarch(as.numeric(dgas_price))# Convert TS to numeric vector

# Garch(1,1)-M
source("~/Downloads/An-introduction-to-analysis-of-financial-data-with-R-master/garchM.R")
y<-dgas_price*100 # Scaled for numerical stability
m2<-garchFit(~1+garch(1,1),data=dgas_price,trace=F); summary(m2)
garchM(as.numeric(y))

# EGarch(1,1)
source("~/Downloads/An-introduction-to-analysis-of-financial-data-with-R-master/Egarch.R")
m1<-Egarch(dgas_price)
stresi<-m1$residuals/m1$volatility
par(mfcol=c(2,1))
tdx<-c(1:1519)/52 +1993
plot(tdx,dgas_price,xlab='year',ylab='simp. rtn',type='l')
plot(tdx,stresi,xlab='year',ylab='simp. rtn',type='l')  
Box.test(stresi,lag=60,type='Ljung') # Significant serial correlations
Box.test(stresi^2,lag=60,type='Ljung')  # Non-existent serial correlations


# TGarch(1,1)
source("~/Downloads/An-introduction-to-analysis-of-financial-data-with-R-master/Tgarch11.R")
m1<-Tgarch11(as.numeric(dgas_price))
at<-m1$residuals ; sigt<-m1$volatility ; resi<- at/sigt  # Standardized Residuals
Box.test(resi,lag=60, type='Ljung') # Significant serial correlations
Box.test(resi^2,lag=60, type='Ljung') # Non-existent serial correlations
# Model is inadequate in modelling first two conditional moments of the simp. rtn series

# APARCH(1,1)
m2<-garchFit(~1+aparch(1,1),data=dgas_price,trace=F); summary(m2) 
# Delta=  1.3918904   and is statistically significant
#This model cannot be reduced to an EGARCH or TGARCH model 
resi<-residuals(m2,standardized=T)
Box.test(resi,lag=60,type="Ljung"); Box.test(resi^2,lag=60,type="Ljung")
# Model is inadequate in modelling first two conditional moments of the simp. rtn series
plot(m2)

# NGARCH(1,1)
source("~/Downloads/An-introduction-to-analysis-of-financial-data-with-R-master/Ngarch.R")
m3<-Ngarch(as.numeric(dgas_price)*100)  # Scaled for numerical stability
at<-m3$residuals ; sigt<-m3$volatility ; resi<- at/sigt  # Standardized Residuals
Box.test(resi,lag=60,type='Ljung') ; Box.test(resi^2,lag=60,type='Ljung') 
# Model is inadequate in modelling first two conditional moments of the simp. rtn series

# ARMA GARCH (Accounting for Seasonality)
acf(dgas_price,lag=45);pacf(dgas_price,lag=45)
m2<- arima(dgas_price,order=c(3,0,0),seasonal=list(order=c(2,0,0),period=19)); m2
m2<- arima(dgas_price,order=c(3,0,0),seasonal=list(order=c(2,0,0),period=19),include.mean = F);m2
m2<-arima(dgas_price, seasonal=list(order=c(2,0,0),period=19),include.mean=F); m2
adjcp<- dgas_price[38:1519] -0.0642* dgas_price[19:1500] -0.0642* dgas_price[1:1481]
acf(adjcp);pacf(adjcp)
#AR(3)-Garch(1,1)
m3<- garchFit(~arma(3,0)+garch(1,1),data=adjcp,trace=F,include.mean=F); summary(m3)
m4<- garchFit(~arma(3,0)+garch(1,1),data=adjcp,trace=F,include.mean=F,cond.dist='std'); summary(m4)
m5<- garchFit(~arma(1,0)+garch(1,1),data=adjcp,trace=F,include.mean=F,cond.dist='sstd'); summary(m5)
plot(m3); plot(m4); plot(m5)

# BackTest
source("~/Downloads/An-introduction-to-analysis-of-financial-data-with-R-master/backtestGarch.R")
M3<-arima(adjcp,order=c(3,0,0),include.mean=F)
M3F<-backtest(M3,adjcp,1469,13,inc.mean=F)
M4F<-backtestGarch(adjcp,1469,13,inc.mean=F,cdist='sstd')



# Without accounting for seasonality(Using a Skewed Student-T Dist.)
# AR(1)-Garch(1,1)
m6<- garchFit(~arma(2,0)+garch(1,1),data=dgas_price,trace=F,include.mean=F,cond.dist='sstd'); summary(m6)
pred_m2<-predict(m6,n.ahead=13,plot=TRUE)



