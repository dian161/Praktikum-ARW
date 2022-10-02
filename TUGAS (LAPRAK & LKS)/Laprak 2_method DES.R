library(forecast)
library(dplyr)

# DATA
data<-read.csv("C:/Users/DIANWL/Documents/# Semester 5/6 PRAK ARW/Data ekspor migas jan15-sep21/ekspor migas.csv")
View(data)
attach(data)
data.ts=ts(Migas, start = c(2015,1), frequency = 12)
data.ts
plot(data.ts, col = "purple", main = "Data RW Ekspor Migas 2015-2021") 
summary(data.ts)

# PERAMALAN DENGAN METODE HOLT
# 1 Dengan fungsi dasar
holt.migas.fd=HoltWinters(data.ts, alpha = NULL, 
                          beta = NULL, gamma = F)

holt.migas.fd
# peramalan
holt.migas.fd$fitted

# prediksi
pred.fd=predict(holt.migas.fd, 3)
pred.fd

# pengukuran kesalahan
holt.migas.fd$SSE
mse.fd=holt.migas.fd$SSE/NROW(holt.migas.fd$fitted)
rmse.fd=sqrt(mse.fd)
mape.fd=mean(abs(data.ts-holt.migas.fd$fitted[,1])/data.ts, na.rm = TRUE)*100
cbind(mse.fd, rmse.fd, mape.fd)

# Plot
plot(data.ts, main = "Data Ekspor Migas [Holt Method]", lwd = 2, col = "blue", xlim = c(2015,2022), type = "o", pch = 15)
limitDate = end(data.ts)[1]+(end(data.ts)[2]-1)/frequency(inflasi.ts)
abline(v=limitDate ,lty=4)
lines(holt.migas.fd$fitted[,1], lwd = 2, col = "red", type = "o", pch = 12)
lines(pred.fd, col = "green", type = "o", pch = 10)
legend("bottomleft", legend = c("Data aktual", "Fitted Value", "Peramalan"), col = c("blue", "red", "green"), 
       lty = 1, pch = c(15, 12, 10), cex = 0.8, inset = 0.02)

# 2. MENGGUNAKAN FUNGSI DALAM PACKAGE FORECAST
# DENGAN DAMPED
holt.migas = holt(data.ts, h = 3, damped = TRUE, 
                  alpha = NULL, beta = NULL, phi = NULL)
# forecast
holt.migas

# Model
holt.migas$model

# peramalan
holt.migas$fitted

# Ukuran 
# DENGAN DAMPED
mse = mean(holt.migas$residuals^2)
rmse = sqrt(mse)
mape = mean(abs(holt.migas$residuals)/data.ts, na.rm = TRUE)*100
cbind(mse, rmse, mape)

# Plot dengan autoplot
autoplot(data.ts, series = "Data Aktual", main = "Data Ekspor Migas 2015-2021")+
  autolayer(fitted(holt.migas), series = "Fitted HW Add")+
  autolayer(holt.migas, series = "Peramalan")


