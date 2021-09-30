# Modeling-Financial-Time-Series-with-R-Chapter-2--Univariate-Time-Series


## Univariate Time Series: 

* AR(p) prosess

* MA(q) prosess

* ARMA prosess


```
> aic.x #  AIC matrix for ARMA(p: 0 to 5 , q: 0 to 5)

          [,1]       [,2]        [,3]       [,4]      [,5]       [,6]
[1,]  119.6381   52.33283   -1.854187  -38.11642  -62.4972  -79.67775
[2,] -112.8027 -131.14947 -136.670651 -137.93389 -137.6903 -141.43686
[3,] -149.2428 -112.48586 -141.142849 -135.11775 -135.1125 -135.48990
[4,] -142.0503 -140.25619 -133.511188 -138.15506 -134.0046 -137.78767
[5,] -140.0651 -139.46296 -137.102064 -136.59828 -136.7591 -135.83473
[6,] -138.4259 -122.29046 -137.021627 -134.77720 -136.5486 -133.92958
> bic.x #  BIC matrix for ARMA(p: 0 to 5 , q: 0 to 5)
          [,1]       [,2]       [,3]       [,4]       [,5]       [,6]
[1,]  123.6888   58.40888    6.24722  -27.98966  -50.34509  -65.50029
[2,] -106.7267 -123.04806 -126.54389 -125.78178 -123.51284 -125.23405
[3,] -141.1414 -102.35910 -128.99074 -120.94029 -118.90965 -117.26174
[4,] -131.9235 -128.10408 -119.33373 -121.95225 -115.77647 -117.53415
[5,] -127.9130 -125.28550 -120.89925 -118.37011 -116.50560 -113.55586
[6,] -124.2484 -106.08764 -118.79346 -114.52368 -114.26971 -109.62536

```

### Unit Root Test: 9 steps ADF test, KPSS Test, and ZA Test

```
#### 9 steps ADF Unit Root test 
#### 9 steps ADF Unit Root test 

library("urca")
data=read.table("Li.txt",header=T)
x <- ts(log(data[,2]), start=c(1357),end=c(1392), frequency=1)
adf.test(x, alternative = "stationary")


####################### step A: (1 & 2 & 3 & 4) ###############################
# Arguments: type = c("none", "drift", "trend") AND selectlags = c("Fixed", "AIC", "BIC")
#drit only intercept
#trend both intercept and trend
unitroot_A<-ur.df(x, lag =2, type = c("trend"), selectlags = c("AIC"))
summary(unitroot_A)
plot(unitroot_A)
unitroot_A@teststat

################### step B: (5&6&7) ###########################
unitroot_B<-ur.df(x, lag =2, type = c("drift"), selectlags = c("AIC"))
summary(unitroot_B)
plot(unitroot_B)
unitroot_B@teststat

################## step C: (8&9) ##############################
unitroot_c<-ur.df(x, lag =2, type = c("none"), selectlags = c("AIC"))
summary(unitroot_c)
plot(unitroot_c)
unitroot_c@teststat

```

* Unit Root Test

```
# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) Unit Root Test

unitroot_kpss=ur.kpss(x, type = c("mu", "tau"), lags = c("short", "long", "nil"),
                      use.lag = NULL)

summary(unitroot_kpss)

# Elliott, Rothenberg & Stock Unit Root Test

unitroot_ers=ur.ers(x, type = c("DF-GLS", "P-test"), model = c("constant", "trend"),
                    lag.max = 4)
summary(unitroot_ers)

# Zivot and andrews Unit Root Test

unitroot_za=ur.za(x, model = c ("both"), lag=NULL)
summary(unitroot_za)

```


### ARIMA modeling 

### ARFIMA with long memory test

<img src="https://user-images.githubusercontent.com/77374087/135507740-fd5b577d-8b5a-4a9a-b5c4-1ccc96bf41c9.png" width="600" height="400">

## univariate time series forecasting and Backtesting 

```
data1=read.table("gold.txt",header=T)
head(data1)
d1=ts(1:198, start = c(1378), frequency =12)
time1 = as.Date(d1)
gold=ts(data1$gold)
plot(time1,gold,type="l", lwd = 3, lty = 1, pch = 1, col="blue")
acf(gold, main="Autocorrelations",lwd=3)
pacf(gold, main="Partial Autocorrelations",lwd=3)
eacf(gold)


#  Long memory
library(fracdiff)
ew=abs(da$vwretd)
# obtain Geweke-Port-Hudak estimate using command fdGPH
m3=fdGPH(abs(gold))
m3
m2=fracdiff(gold,nar=1,nma=1)
summary(m2)
tsdiag(fit)

Box.test(m2$residuals, type = c("Box-Pierce"))
Box.test(m2$residuals, type = c( "Ljung-Box"))
plot(predict(m2, n.ahead = 5), lwd=3, col="dark green")

# model comparison
data1=read.table("gold.txt",header=T)
d1=ts(1:198, start = c(1378), frequency =12)
gold=ts(data1$gold)

# Back testing 
source("backtest.R")
fit2=arima(gold,order=c(1,1,0),season=list(order=c(0,0,0)))
fit3=arima(gold,order=c(2,1,0),season=list(order=c(0,0,0)))
fit4=arima(gold,order=c(3,1,0),season=list(order=c(0,0,0)))

> mm2=backtest(fit2,gold,100,1)
[1] "RMSE of out-of-sample forecasts"
[1] 0.001908898
[1] "Mean absolute error of out-of-sample forecasts"
[1] 0.001344186
> mm2=backtest(fit3,gold,100,1)
[1] "RMSE of out-of-sample forecasts"
[1] 0.001918403
[1] "Mean absolute error of out-of-sample forecasts"
[1] 0.001349251
> mm3=backtest(fit3,gold,100,1)
[1] "RMSE of out-of-sample forecasts"
[1] 0.001918403
[1] "Mean absolute error of out-of-sample forecasts"
[1] 0.001349251

```
