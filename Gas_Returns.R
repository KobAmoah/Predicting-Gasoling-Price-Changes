library(quantmod); library(fBasics); library(TSA) ; library(fUnitRoots);library(readxl) ; library(fracdiff)
gas_prices <- data.frame(read_excel("~/Downloads/Project Data/EMM_EPM0_PTE_NUS_DPGw.xls"))
oil_prices<-data.frame(read_excel("~/Downloads/Project Data/RWTCw.xls"))
source("~/Downloads/Time Series/backtest.R")
gas_price<- ts(gas_prices[,2], start=1993-04,freq=52)
gas_rtn<-diff(log(gas_price))  #compute weekly log returns
gas_simp<-diff(gas_price)  #compute simple weekly returns
chartSeries(gas_rtn,theme='white')  # not working
par(mfcol=c(2,1))
plot(gas_rtn,type='l',xlab='year',
     ylab='returns')
title(main='(a): Log returns')
plot(gas_simp,type='l',xlab='year',
     ylab='returns')
title(main='(b): Simple returns')
basicStats(gas_simp)
normalTest(gas_simp,method="jb") # Reject Normality Assumption
t.test(gas_simp) # Is the mean return 0? Maybe
TT<-length(gas_simp)
t3<- skewness(gas_simp)/sqrt(6/TT) # Skewness Test
t4<- kurtosis(gas_simp)/sqrt(24/TT) #Kurtosis Test
c(p_val,p_val1)  # Distribution is thus skewed and leptokurtic(heavy tailed)
# Visualized Returns Series
hist(gas_simp,nclass=40)
d1<-density(gas_simp)
range(gas_simp)
x<-seq(-.5,.5,.001)
y1<-dnorm(x,mean(gas_simp),sd(gas_simp))
plot(d1$x,d1$y,xlab='rtn',
     ylab='density',type='l')
lines(x,y1, lty=2)
apply(log(gas_simp+1),mean)

acf(gas_rtn,lag=12) # sample ACF for the data
Box.test(gas_rtn,lag=log(TT),type='Ljung')  # Computation of Q(m) statistic(Box-Ljung Test) with m as ln(TT), 
                                          # where TT is the number of data points
# P-val rejects the hypothesis of no serial correlations among log gas returns.

# Order Selection Using AIC Criterion - AR model
mm1<-ar(gas_rtn,method='mle') 
mm1$order #Find order using the AIC criterion
            #order=9
aic<-mm1$aic
length(aic)
plot(c(0:12),aic,type='h',xlab='order',ylab='aic')
lines(c(0:12),aic,lty=2)  
print(mm1$aic,digits=3) # Notice Order 9 has the smallest AIC value 

# Order Selection Using PACF - AR model
pacf(gas_rtn,lag=12) # Find order using the PACF  # Order = 3 # For simplicity, I use an Order 3 model
m1<-arima(gas_rtn,order=c(3,0,0))  
m1   # Notice coefficient on ar2 is non-significant at any conf.level alpha
tsdiag(m1,gof=12)
p1<-c(1,-m1$coef[1:3])
r1<-polyroot(p1) # Characteristic root. 
Mod(r1) # Note they are all greater than 1 in absolute value. 
# That is the series is stationary.

# Average stochastic cycle length(Assuming AR(2) )
k<- 2*pi/acos(0.879693/3.065111)
k      # (Average duration of expansionary and contractionary gas returns is about 5 weeks) 
#'Business Cycle of Gas Returns is 5 weeks'

(1-0.5367 - 0.0291 +  0.0771)*mean(gas_rtn) # Intercept term
sqrt(m1$sigma2) #S.E of residuals
Box.test(m1$residuals,lag=12,type='Ljung')
# Model Checking
pv<- 1-pchisq(16.168,9)
pv
m2<-arima(gas_rtn,order=c(3,0,0),fixed=c(NA,0,NA,NA))
m2

(1-0.5488+0.0890)* 0.0005 # Intercept term
sqrt(m2$sigma2)
Box.test(m2$residuals,lag=12,type='Ljung')
pv<-1-pchisq(17.257, 10)
pv

# Prediction AR model
m2<-arima(gas_rtn[1:1499],order=c(3,0,0),fixed=c(NA,0,NA,NA))  # At observation 1499= Jan 3,2022
p1<- data.frame(predict(m2$pred,20))
p2<- cbind(p1, p1$pred-2*p1$se,p1$pred+2*p1$se)
colnames(p2)<-c('pred_val', 's_e','95%_CI_Lower','95%_CI_Upper')

##### MA model
# Order Selection Using ACF
acf(gas_rtn,lag=12)   # Order 6
ma1<- arima(gas_rtn,order=c(0,0,6))
ma1   # All terms are significant(Except intercept)
sqrt(0.0002349) # S.E of model
ma1<-arima(gas_rtn[1:1499],order=c(0,0,6)) 
p3<-data.frame(predict(ma1,20))
p4<-cbind(p3, p3$pred-2*p1$se,p3$pred+2*p1$se)
colnames(p4)<-c('pred_val', 's_e','95%_CI_Lower','95%_CI_Upper')

# Graphical Representation  --- Produced using data frames rather than a ts object
tdx<-c(1:nrow(p1)+1)/52 +2022 # Time Index
plot(tdx, p2$pred_val,ylim=c(min(p2$`95%_CI_Lower`), max(p2$`95%_CI_Upper`))
     ,type='l',xlab='Time Index',ylab='Predicted Values'); points(tdx,p2$pred_val)  
title(main='a) AR(3) Model Forecast Vs. Actual Values')
lines(tdx,gas_rtn[1500:1519],lty=2); points(tdx,gas_rtn[1500:1519],pch=16)

plot(tdx, p4$pred_val,ylim=c(min(p2$`95%_CI_Lower`), max(p2$`95%_CI_Upper`))
     ,type='l',xlab='Time Index',ylab='Predicted Values'); points(tdx,p4$pred_val)  
title(main='b) MA(6) Model Forecast Vs. Actual Values')
lines(tdx,gas_rtn[1500:1519],lty=2); points(tdx,gas_rtn[1500:1519],pch=16)

# Graphical Representation of Model Forecast Only
layout(1:1)
p1.ts<- ts(p1$pred)
plot(p1.ts,type='l',xlab='Time Index',ylab='Predicted Values',col='red'); points(p1.ts,pch=16) 

p3.ts<- ts(p3$pred); points(p3.ts) 
lines(p3.ts,lty=2,xlab='Time Index',ylab='Predicted Values',col='blue')
title(main='AR(3) Model Forecast vs MA(6) Model Forecast')
legend(locator(n=1), legend=c("AR(3) Model", "MA(6) Model"),
       col=c("red", "blue"), lty=1:2, cex=0.8)

#####
#ARMA model
m1<-eacf(gas_rtn,6,12) # X denotes the modulus of the EACF is > or = to 2*S.E
# Pattern of 0's suggest ARMA(5,2), (6,2) (4,3) (3,3) (2,3)
print(m1$eacf,digits= 2) 
# Compare each of suggested to 2/sqrt(length(gas_rtn)) = 2/sqrt(TT)
# Using the 1% CI as the selection criteria, narrow down to ARMA (4,3) (3,3) (2,3)
# Select (2,3) for simplicity
m2<-arima(gas_rtn,order=c(2,0,3))
m2  # All coefficients are significant

#####
#Tests for stationarity --- Can the ARMA(2,3) Model be reduced
#AR Polynomials
p1<-c(1,-m2$coef[1:2])
r1<-polyroot(p1) # Characteristic root. 
Mod(r1) # Note they are all greater than 1 in absolute value and are real. 

#MA Polynomials
p1<-c(1,-m2$coef[3:5])
r1<-polyroot(p1) # Characteristic root. 
Mod(r1) # Note they are all greater than 1 in absolute value and are real. 
# The series is stationary.

# Prediction AR model
m3<-arima(gas_rtn[1:1499],order=c(2,0,3))
p1<- data.frame(predict(m3,20))
p2<- cbind(p1, p1$pred-2*p1$se,p1$pred+2*p1$se)
colnames(p2)<-c('pred_val', 's_e','95%_CI_Lower','95%_CI_Upper')

# Graphical Representation  --- Produced using data frames rather than a ts object
tdx<-c(1:nrow(p1)+1)/52 +2022 # Time Index
plot(tdx, p2$pred_val,ylim=c(min(p2$`95%_CI_Lower`), max(p2$`95%_CI_Upper`))
     ,type='l',xlab='Time Index',ylab='Predicted Values'); points(tdx,p2$pred_val)  
title(main='ARMA(2,3) Model Forecast Vs. Actual Values')
lines(tdx,gas_rtn[1500:1519],lty=2); points(tdx,gas_rtn[1500:1519],pch=16)
legend(locator(n=1),legend=c('ARMA(2,3) forecast','Actual Values'),pch=c(1,16))

# Graphical Representation of Model Forecast Only
p1.ts<- ts(p1$pred)
plot(p1.ts,type='l',xlab='Time Index',ylab='Predicted Values',col='red'); points(p1.ts,pch=16) 


########
# ARIMA(0,1,5) for the log-return series(Exponential Smoothing)
# Damping Cosine waves in differenced log-price series
dlog_gas<-diff(log_gas)  # First differencing
acf(dlog_gas,lag=50)  # MA order(5) 
m1<-arima(log_gas,order=c(0,1,5)) 
m1
Box.test(m1$residuals, lag=10,type='Ljung')
pp<-1-pchisq(10.05,7)
pp   # The model is adequate in modeling the dynamic dependence of the data.
tsdiag(m1,lag=20) # The model is adequate in modeling the dynamic dependence of the data.
pm1<-predict(m1,20)
pred<-pm1$pred
se<-pm1$se
fore<- exp(pred+se^2/2)
v1<-exp(2*pred+se^2)*(exp(se^2)-1)
s1<-sqrt(v1)
pred_gas<-gas_price[1450:1519]
tdx<-c(1:length(pred_gas))/52 + 2021
upp<- c(gas_price[1500],fore+2*s1)
low<- c(gas_price[1500],fore-2*s1)
min(low, pred_gas)
max(upp,pred_gas)
plot(tdx,pred_gas, xlab='year',ylab='gas prices',type='l',ylim=c(2,6.5))
points(tdx[51:70],fore,pch='*')
lines(tdx[50:70],upp,lty=2)
lines(tdx[50:70],low,lty=2)
points(tdx[51:70], gas_price[1500:1519], pch='o',cex=0.7)

##### Seasonality
dlog_gas<-diff(log_gas)  # First differencing
slog_gas<-diff(log_gas,52) # Seasonal differencing 
ddlog_gas<-diff(slog_gas)  # Regular and Seasonal differencing
par(mfcol=c(2,2))
acf(log_gas,lag=200)
acf(dlog_gas,lag=200)
acf(slog_gas,lag=200)
acf(ddlog_gas,lag=200)  # ACF for lower right plot is negative and statiscally significant at  
                        #   lags= 52, 51, 50
# ACF for lower right plot is positive and marginally significant at  
#   lags= 47, 46, 45
      
par(mfcol=c(3,1))
plot(dlog_gas,xlab='year',ylab='diff', type='l')
plot(slog_gas, xlab='year',ylab='sea-diff',type ='l')
plot(ddlog_gas,xlab='year',ylab='diff', type='l')

## Pretty awful fit 
m1<-arima(log_gas,order=c(0,1,5),seasonal=list(order=c(0,1,3),period=52)) 
m1
tsdiag(m1,gof=20)
Box.test(m1$residuals,lag=12,type='Ljung')
pp<-1-pchisq(11.198,4)
pp   # The model is adequate in modeling the dynamic dependence of the data.
pm1<-predict(m1,20)
pred<-pm1$pred
se<-pm1$se
fore<- exp(pred+se^2/2)
v1<-exp(2*pred+se^2)*(exp(se^2)-1)
s1<-sqrt(v1)
pred_gas<-gas_price[1450:1519]
tdx<-c(1:length(pred_gas))/52 + 2021
upp<- c(gas_price[1500],fore+2*s1)
low<- c(gas_price[1500],fore-2*s1)
min(low, pred_gas)
max(upp,pred_gas)
plot(tdx,pred_gas, xlab='year',ylab='gas prices',type='l',ylim=c(2.2,6.1))
points(tdx[51:70],fore,pch='*')
lines(tdx[50:70],upp,lty=2)
lines(tdx[50:70],low,lty=2)
points(tdx[51:70], gas_price[1500:1519], pch='o',cex=0.7)

####Long-Memory Model
## AFRIMA (0,d,5)
m3<-fdGPH(log_gas)
m3
m2<-fracdiff(log_gas,nar=0,nma=5)
summary(m2)
pm1<-predict(m2,20)  # Error applicable method for 'predict' applied to an object of class "fracdiff"
# Cannot apply out-of-sample error test

## ARFIMA (1,d,1)
m2<-fracdiff(log_gas,nar=1,nma=1)
summary(m2)


### Arch Effect
acf(abs(gas_rtn),lag=60)
Box.test(abs(gas_rtn),lag=60,type='Ljung')




