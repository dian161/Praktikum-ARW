library(forecast)
library(tseries)

# Membaca Data
hotel = read.csv("C:/Users/DIANWL/Documents/# Semester 5/6 PRAK ARW/hotel diy.csv")
head(hotel, 5)
View(hotel)
attach(hotel)

hotel.ts = ts(DI.YOGYAKARTA, start = c(2016, 1), frequency = 12)
autoplot(hotel.ts, color = "orange", main = "Data ts Hotel DIY")

# Auto ARIMA
auto.arima(hotel.ts)

# Diferensi musiman order 1 (d=1)
hotel.ds = diff(hotel.ts, differences = 1, lag = 12)
adf.test(hotel.ds) # pvalue 0.1335 > 5% maka data blm stasioner lakukan dif kembali

# Diferensi non-musiman order 1 (D=1)
hotel.dds = diff(hotel.ds, differences = 1)
adf.test(hotel.dds) # pvalue 0.01 < 5% data sdh stasioner

# Plot Acf dan Pacf untuk identifikasi model
par(mfrow = c(1, 2))
Acf(hotel.dds, lag.max = 36)
Pacf(hotel.dds, lag.max = 36)

# Fungsi untuk uji signifikasi koefisien
printstatarima <- function (x, digits = 4,se=TRUE,...){
  if (length(x$coef) > 0) {
    cat("\nCoefficients:\n")
    coef <- round(x$coef, digits = digits)
    if (se && nrow(x$var.coef)) {
      ses <- rep(0, length(coef))
      ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits = digits)
      coef <- matrix(coef, 1, dimnames = list(NULL, names(coef)))
      coef <- rbind(coef, s.e. = ses)
      statt <- coef[1,]/ses
      pval  <- 2*pt(abs(statt), df=length(x$residuals)-1, lower.tail = FALSE)
      coef <- rbind(coef, t=round(statt,digits=digits),sign.=round(pval,digits=digits))
      coef <- t(coef)
    }
    print.default(coef, print.gap = 2)
  }
}
# plot----------Order non musiman (- lag 4)------order musiman (pada lag 12)
# PCAF            p(AR) = 1                           P(SAR) = 0
# Differensi      d     = 1                           D      = 1
# ACF             q(MA) = 1                           Q(SMA) = 1

# Estimasi model
model1 = arima(hotel.ts, order = c(1,1,1),seasonal = list(order = c(0,1,1)),include.mean = FALSE)
summary(model1)
# hipotesis 
# Ho: koef-koef tdk sig trhdp model
# H1: koef-koef sig trhdp model
printstatarima(model1)

model2 = arima(hotel.ts, order = c(0,1,1), seasonal = list(order = c(0,1,1)), include.mean = FALSE)
summary(model2)
printstatarima(model2)

# Uji Diagnostik
tsdiag(model1) # garis putus2 biru adalah sqrt(1.96)/n
sqrt(1.96)/70
tsdiag(model2)

# Prediksi
hotel.predict = forecast(model1, 5)
hotel.predict
plot(hotel.predict)

# akurasi peramalan
hotel.fitt = fitted(model1)
mape = mean(abs(hotel.ts-hotel.fitt)/hotel.ts)*100
akurasi = 100-mape
cbind(mape, akurasi)
