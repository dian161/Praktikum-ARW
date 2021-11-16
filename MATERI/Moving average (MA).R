install.packages("TTR")
install.packages("quantmod")
library(TTR)
library(quantmod)

# DATA
getSymbols(Symbols = "GOOG", scr = "yahoo", from = "2018-09-01", to = "2019-09-01")
View(GOOG)

# MENGEKSTRAK HARGA PENUTUPAN
y = Ad(GOOG)
y

summary(is.na(y))

# SIMPLE MOVING AVERAGE
sma10 = SMA(y, n = 10)
datasma10 = data.frame(y, sma10)
View(datasma10)

# PLOT DATA AKTUAL VS SMA(10)
plot(time(y), y, col = "blue", main = "GOOG", type = b, pch = 21, cex = 0.5)
lines(time(sma10), sma10, col = "red", lwd = 2, type = b, pch = 12, cex = 0.8)
legend("bottomleft", legend = c("GOOG", "SMA10"), col = c("blue", "red"), 
       pch = c(21,12), lty = 1, 
       cex = 0.7, inset = 0.05)


# WEIGHTED MOVING AVERAGE
wma10 = WMA(y, n = 10, wts = 1:10)
datawma10 = data.frame(y, wma10)
View(datawma10)

# PLOT, MENAMBAHKAN WMA(10)
lines(time(wma10), wma10, col = "green", lwd = 2, type = b, pch = 3, cex = 0.8)
legend("bottomleft", legend = c("GOOG", "SMA10","WMA10"), col = c("blue", "red", "green"), 
     pch = c(21,12, 3), lty = 1, 
     cex = 0.7, inset = 0.05)

# EXPONENTIAL SMOOTHING
ema10 = EMA(y, n = 10)
dataema10 = data.frame(y, sma10, ema10)
View(dataema10)

line(ema10, col = "green", lwd = 2)

# MENGHITUNG UKURAN ERROR, MEAN SQUARED ERROR
mse.sma10 = mean((y-sma10)^2, na.rm = TRUE)
mse.wma10 = mean((y-wma10)^2, na.rm = TRUE)
mse.ema10 = mean((y-ema10)^2, na.rm = TRUE)
