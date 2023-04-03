rm(list=ls())
library(TSA)
library(fUnitRoots)
library(lmtest)
library(tseries)
setwd("C:/Users/antho/Desktop/Uni/2021 S2/Time Series/Final Project")
airmiles #Passenger Miles on Commercial US Airlines, 1937-1960
#one mile travelled by one passenger, as a unit of traffic.
#Import
airmilesTS <- airmiles
#Plot series function
plot_series <- function(series, title) {
  par(mfrow=c(1,1))
  plot(series, ylab = 'Passenger Miles', xlab='Year', type= 'o', main=titl
       e)
}
#Produce ACF and PACF function
acf_pacf <- function(series){
  par(mfrow=c(1,2))
  acf(series, main = "ACF Plot of Passenger Miles Time Series")
  pacf(series, main = "PACF Plot of Passenger Miles Time Series")
}
#Normality checks
normality_sptest <- function(series){
  par(mfrow=c(1,1))
  qqnorm(series,main="QQ Normal Plot of Passenger Miles Series")
  qqline(series)
  
  shapiro.test(series)
}
#Diagnostics
residual_plot <- function(model) {
  
  plot(rstandard(model),ylab ='Standardized Residuals',type='o',
       main="Time series plot of standardised residuals for Passenger Mile
       s series")
  abline(h=0)
  
}
normality_test <- function(model){
  MATH2204: Final Project
  Anthony Huynh s3546318
  x = residuals(model)
  
  normality_sptest(x)
}
#Plot TS
plot_series(airmilesTS,
            "Time series plot of yearly Passenger Miles on Commercial US A
            irlines")
#Plot successive values
y = airmilesTS
x = zlag(airmilesTS)
index = 2:length(x)
plot(y=airmilesTS,x=zlag(airmilesTS),ylab='Passenger Miles',
     xlab='Passenger Miles',
     main= "Scatter plot of neighboring values of Passenger Miles")
cor(y[index],x[index])
#Descriptive Statistics
summary(airmilesTS)
#ACF PACF and check for normality
acf_pacf(airmilesTS)
normality_sptest(airmilesTS)
airmilesTSDiff <- diff(airmilesTS, differences = 1)
plot_series(airmilesTSDiff,"Time series plot of yearly Passenger Miles on
            Commercial US Airlines")
#Stationarity test
adf.test(airmilesTSDiff, alternative = c("stationary"))
acf_pacf(airmilesTSDiff)
#Model specification
eacf(airmilesTSDiff, ar.max = 5, ma.max = 5)
res = armasubsets(y=airmilesTSDiff, nar=3 , nma=3, y.name='P. Miles',
                  ar.method='ols')
plot(res)
#ARMA(0,1,0)
model_010 <- arima(airmilesTS, order =c(0,1,0), method= 'CSS')
#Diagnostics
residual_plot(model_010)
normality_test(model_010)
acf(residuals(model_010), main = 'ACF plot of Residuals')
Box.test(residuals(model_010), type = "Ljung-Box")
MATH2204: Final Project
Anthony Huynh s3546318
tsdiag(model_010,gof=15,omit.initial=F)
#Parameter estimation
model <- stats::arima(airmilesTS, order = c(1,1,0), method='CSS')
coeftest(model)
#Diagnostics
model_110 <- arima(airmilesTS, order = c(1,1,0), method = 'CSS')
residual_plot(model_110)
normality_test(model_110)
acf(residuals(model_110), main = 'ACF plot of Residuals')
Box.test(residuals(model_010), type = "Ljung-Box")
tsdiag(model_010,gof=15,omit.initial=F)
model_fit2 <- stats::arima(airmilesTS, order = c(2,1,0), method='CSS')
coeftest(model_fit2)
#Forecasting
plot(model_110,n.ahead=10,type='b',xlab='Year', ylab='Passenger Miles',
     main = 'Forecasting Passenger Miles for the next 10 years')
#Prediction Values
pred <- predict(model_110, n.ahead=10)
pred$pred
pred$se
#Fitting Quadratic model
t = time(airmilesTS)
model_linear = lm(airmilesTS~t) # label the linear trend model as model1
summary(model_linear)
abline(model_linear)
t2 = t^2
model_quad = lm(airmilesTS~t+t2) # label the linear trend model as model1
summary(model_quad)
plot(ts(fitted(model_quad)), ylim = c(min(c(fitted(model_quad),
                                            as.vector(airmilesTS))), max(c
                                                                         (fitted(model_quad),
                                                                           
                                                                           as.vector(airmilesTS)))),
     ylab='Passenger Miles' , main = "Time series plot of yearly Passenger
     Miles on Commercial US Airlines", type="l",lty=2,col="red")
lines(as.vector(airmilesTS),type="o")
res.model2 = rstudent(model_quad)
win.graph(width=10, height=7,pointsize=8)
par(mfrow=c(3,2))
plot(y = res.model2, x = as.vector(time(airmilesTS)),xlab = 'Time', ylab='
     MATH2204: Final Project
     Anthony Huynh s3546318
     Standardized Residuals',type='l',main = "Standardised residuals from quadr
     atic model.")
hist(res.model2,xlab='Standardized Residuals', main = "Histogram of standa
     rdised residuals.")
qqnorm(y=res.model2, main = "QQ plot of standardised residuals.")
qqline(y=res.model2, col = 2, lwd = 1, lty = 2)
shapiro.test(res.model2)
acf(res.model2, main = "ACF of standardized residuals.")
pacf(res.model2, main = "PACF of standardized residuals.")
par(mfrow=c(1,1))