# TRIPLE EXPONENTIAL SMOOTHING

# BACA DATA
data<-read.csv("C:/Users/DIANWL/Documents/# Semester 5/6 PRAK ARW/Data ekspor migas jan15-sep21/ekspor migas.csv")
View(data)

attach(data)

data.ts=ts(Migas, start = c(2015,1), frequency = 12)
data.ts
plot(data.ts) 

# 2. HW DENGAN LIBRARY FORECAST
library(forecast)
hw.migas.add = hw(data.ts, h = 3, seasonal = "additive", damped = TRUE, 
                  alpha = NULL, beta = NULL, gamma = NULL, phi = NULL)
hw.migas.multi = hw(data.ts, h = 3, seasonal = "multiplicative", 
                    damped = TRUE, alpha = NULL, beta = NULL, gamma = NULL, 
                    phi = NULL)
hw.migas.add$model
hw.migas.multi$model
# model/method terbaik dengan nilai AIC terkecil = additive

# Fitted/peramalan
hw.migas.add$fitted
hw.migas.multi$fitted

# Prediksi
hw.migas.add
hw.migas.multi

# Ukuran kesalahan
# Metode Additive
mse.add = mean((hw.migas.add$residuals)^2)
rmse.add = sqrt(mse.add)
mape.add = mean(abs(hw.migas.add$residuals/data.ts), na.rm = TRUE)*100
cbind(mse.add, rmse.add,mape.add)
# Metode Multiplicative
mse.multi = mean((data.ts-hw.migas.multi$fitted)^2)
rmse.multi = sqrt(mse.multi)
mape.multi = mean(abs(data.ts-hw.migas.multi$fitted)/data.ts, na.rm = TRUE)*100
cbind(mse.multi, rmse.multi,mape.multi)

# jadi metode terbaik adalah metode multiplicative

# Plot model 1
library(ggplot2)
library(dplyr)
par(mfrow=c(1,2))
plot(data.ts)
###
autoplot(data.ts, series = "Data Aktual") + 
  autolayer(fitted(hw.migas.add), series = "Fitted HW add") 

autoplot(data.ts, series = "Data Aktual", main = "Data Ekspor Migas 2015-2021") +   
  autolayer(fitted(hw.migas.multi), series =  "Fitted HW Multi")+
  autolayer(hw.migas.multi, series = "Peramalan")

###
autoplot(data.ts, series = "Data Aktual", main = "Data Ekspor Migas 2015-2021") +   
  autolayer(hw.migas.multi, series = "Peramalan", PI = FALSE)
