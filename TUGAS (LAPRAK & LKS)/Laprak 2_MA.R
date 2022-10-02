library(TTR)
library(quantmod)

# memanggil data secara online
getSymbols(Symbols = "ISAT.JK", scr = "yahoo", from = "2020-10-18", to = "2021-10-18")
View(ISAT.JK)

# mengekstrasion data
y = Ad(ISAT.JK)

# untuk cek missing value
summary(is.na(y))

# plot
plot(time(y), y, col="green", main = "ISAT.JK", type = "b", pch = 21, cex=0.5)

# imputation MA pada data ts
# 1 simple moving average (sma)
sma10=SMA(y, n=10)
datasma10=data.frame(y, sma10)
View(datasma10)
# plot data aktual Vs sma(10)
lines(time(sma10), sma10, col = "red", lwd = 2, type = "b", pch = 12, cex = 0.8)
legend("bottomright", legend = c("ISAT.JK", "SMA10"), col = c("green", "red"), 
       pch = c(21,12), lty = 1, 
       cex = 0.7, inset = 0.05)

# 2 weighted moving average (wma)
wma10=WMA(y, n=10, wts = 1:10) # wts>>bobot
datawma10=data.frame(y, wma10)
View(datawma10)
# plot data aktual Vs sma(10) dan wma(10)
lines(time(wma10), wma10, col = "purple", lwd = 2, type = "b", pch = 12, cex = 0.8)
legend("bottomleft", legend = c("ISAT.JK", "SMA10", "WMA10"), col = c("green", "red", "purple"), 
       pch = c(21,12,3), lty = 1, 
       cex = 0.7, inset = 0.05)

# 3 exponential moving average (wma)
ema10=EMA(y, n=10)
dataema10=data.frame(y, sma10, ema10)
View(dataema10)
# plot data aktual Vs sma, wma, ema (10)
lines(time(ema10), ema10, col = "orange", lwd = 2, type = "b", pch = 12, cex = 0.8)
legend("bottomleft", legend = c("ISAT.JK", "SMA10", "WMA10", "EMA10"), col = c("green", "red", "purple", "orange"), 
       pch = c(21,12,3), lty = 1, 
       cex = 0.7, inset = 0.05)

# menghitung error MSE
mse.sma10 = mean((y-sma10)^2, na.rm = TRUE)
mse.wma10 = mean((y-wma10)^2, na.rm = TRUE)
mse.ema10 = mean((y-ema10)^2, na.rm = TRUE)

# menghitung error MAPE (%)
n<-nrow(y)
mape.sma10=1/n*sum(abs((y-mse.sma10)/y))*100
mape.wma10=1/n*sum(abs((y-mse.wma10)/y))*100
mape.ema10=1/n*sum(abs((y-mse.ema10)/y))*100