data<-read.csv("C:/Users/DIANWL/Documents/# Semester 5/6 PRAK ARW/Data ekspor migas jan15-sep21/ekspor migas.csv")
View(data)

attach(data)
data.ts=ts(Migas, start = c(2015,1), frequency = 12)
data.ts
plot(data.ts) 

# HW Addictive
hwb.migas.add = HoltWinters(data.ts, alpha = NULL, 
                beta = NULL, gamma = NULL, seasonal = "additive")
hwb.migas.add
# HW Multiplicative
hwb.migas.multi = HoltWinters(data.ts, alpha = NULL, 
                  beta = NULL, gamma = NULL, seasonal = "multiplicative")
hwb.migas.multi 

# Pengukuran kesalahan [error]
####Metode addictive####
mse.add = hwb.migas.add$SSE/frequency(hwb.migas.add$fitted)
rmse.add = sqrt(mse.add)
mape.add = mean(abs(data.ts-hwb.migas.add$fitted[,1])/data.ts)*100
cbind(mse.add, rmse.add, mape.add)
####Metode multiplicative####
mse.multi = hwb.migas.multi$SSE/frequency(hwb.migas.multi$fitted)
rmse.multi = sqrt(mse.multi)
mape.multi = mean(abs(data.ts-hwb.migas.multi$fitted[,1])/data.ts)*100
cbind(mse.multi, rmse.multi, mape.multi)

# Prediksi add dan multi
pred.migas.multi=predict(hwb.migas.multi, 3)
pred.migas.multi
pred.migas.add=predict(hwb.migas.add, 3)
pred.migas.add

# Fitted value add dan multi
hwb.migas.add$fitted
hwb.migas.multi$fitted

# Plot
####Multiplicative####
par(mfrow=c(1,2))
plot(data.ts, main = "Data Ekspor Migas [Multiplicative]", lwd = 2, col = "blue", xlim = c(2015,2022), 
     type = "l", pch = 15)
limitDate = end(data.ts)[1]+(end(data.ts)[2]-1)/frequency(data.ts)
abline(v=limitDate ,lty=4)
lines(hwb.migas.multi$fitted[,1], lwd = 2, col = "red", type = "l", pch = 12)
lines(pred.migas.multi, col = "green", type = "l", pch = 10)
legend("bottomleft", legend = c("Data aktual", "Fitted Value", "Peramalan"), col = c("blue", "red", "green"), 
       lty = 1, pch = c(15, 12, 10), cex = 0.8, inset = 0.02)
####Addictive####
plot(data.ts, main = "Data Ekspor Migas [Addicitve]", lwd = 2, col = "blue", xlim = c(2015,2022), 
    type = "l", pch = 15)
limitDate = end(data.ts)[1]+(end(data.ts)[2]-1)/frequency(data.ts)
abline(v=limitDate ,lty=4)
lines(hwb.migas.add$fitted[,1], lwd = 2, col = "red", type = "l", pch = 12)
lines(pred.migas.add, col = "green", type = "l", pch = 10)
legend("bottomleft", legend = c("Data aktual", "Fitted Value", "Peramalan"), col = c("blue", "red", "green"), 
       lty = 1, pch = c(15, 12, 10), cex = 0.8, inset = 0.02)
