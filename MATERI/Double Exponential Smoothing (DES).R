install.packages("forecast")
install.packages("dplyr")
library("forecast")
library("dplyr")

# DATA
inflasi = read.csv(file.choose(), header = T, sep=",")
View(inflasi)

# Ubah data menjadi time series dengan fungsi "ts"
inflasi.ts = ts(inflasi$Inflasi, start = c(2017, 1), frequency = 12)
print(inflasi.ts)

# 1. MENGGUNAKAN FUNGSI DASAR
# DSE sering disebut jg dengan metode Holt
# Metode ini cocok untuk data runtun waktu dengan pola trend
# Pada holt terjadi 2 pemulusan = tingkat level alpha dan tingkat trend beta

# Holt dengan initial value l dan b yang ditentukan
holtb.inflasi = HoltWinters(inflasi.ts, alpha = NULL, beta = NULL, gamma = FALSE,
                            l.start = inflasi.ts[1], #nilai awal untuk level
                            b.start = inflasi.ts[2]-inflasi.ts[1]) #nilai awal untuk trend
# alpha&beta NULL artinya parameter yg digunakan adalah "parameter optimum"
holtb.inflasi

# Holt dengan initial value default (nilai awal)
holtb.inflasi = HoltWinters(inflasi.ts, alpha = NULL, beta = NULL, gamma = FALSE)
holtb.inflasi

# fitted value << hasil prediksi
holtb.inflasi$fitted

# plot dari nilai pemulusan trend dn level
plot(holtb.inflasi$fitted[,3], ylab = "trend")
plot(holtb.inflasi$fitted[,2], ylab = "level")

# Ukuran kesalahan dari hasil peramalan
holtb.inflasi$SSE 
mse = holtb.inflasi$SSE/NROW(holtb.inflasi$fitted)
rmse = sqrt(mse) 
mape = mean(abs(inflasi.ts-holtb.inflasi$fitted[,1])/inflasi.ts, na.rm = TRUE)*100
mse #sum square error
rmse #mean square error
mape

# Prediksi
pred.holtb = predict(holtb.inflasi, 5) # 5 = periode kedepan
pred.holtb

# Plot prediksi
plot(inflasi.ts, main = "Inflasi Kota Yogyakarta", lwd = 2, col = "blue", xlim = c(2017,2022), type = "o", pch = 15)
limitDate = end(inflasi.ts)[1]+(end(inflasi.ts)[2]-1)/frequency(inflasi.ts)
abline(v=limitDate ,lty=4)
lines(holtb.inflasi$fitted[,1], lwd = 2, col = "red", type = "o", pch = 12)
lines(pred.holtb, col = "green", type = "o", pch = 10)
legend("bottomleft", legend = c("Data aktual", "Fitted Value", "Peramalan"), col = c("blue", "red", "green"), 
       lty = 1, pch = c(15, 12, 10), cex = 0.8, inset = 0.02)


# 2. MENGGUNAKAN FUNGSI DALAM PACKAGE FORECAST
holt.inflasi = holt(inflasi.ts, h = 5, damped = TRUE, alpha = NULL, beta = NULL, phi = NULL)
holt.inflasi

# Model
holt.inflasi$model

# Ukuran kesalahan
mse = mean(holt.inflasi$residuals^2)
rmse = sqrt(mse)
mape = mean(abs(holt.inflasi$residuals)/inflasi.ts, na.rm = TRUE)*100
mse
rmse
mape

# Plot
plot(holt.inflasi, main = "Inflasi Kota Yogyakarta", col = "purple", lwd = 2, 
     type = "b", pch = 10, cex = 0.6)
lines(holt.inflasi$fitted, col = "green", lwd = 2, type = "b", pch = 15, cex = 0.6)
legend("topright", legend = c("Data Aktual", "Fitted Value"), col = c("purple", "green"),
       lty = 1, cex = 0.8, inset = 0.02, pch = c(10,15))

# Plot dengan autoplot
autoplot(inflasi.ts, series = Data Aktual")
autolayer(fitted(holt.inflasi), series = "Fitted HW Add")
autolayer(fitted(holt.inflasi), series = "Fitted HW Multi")
