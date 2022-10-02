library(nnfor)
library(forecast)

# Data tanpa musiman
ka = read.csv("C:/Users/DIANWL/Documents/# Semester 5/6 PRAK ARW/dataKA.csv")
View(ka)
ka.ts = ts(ka$Total, start = c(2006,1), freq = 12)
plot(ka.ts) #bukan musiman

# Data train dan test dg rasio 80%:20%
n = round(0.80*NROW(ka.ts), 0)
train = window(ka.ts, start = c(2006,1), end = c(2013, 12)) # data ke 96
test = window(ka.ts, start = c(2014,1))

# Model ELM
# model 1 hd=2 #########################################
model1 = elm(train, 
             m = frequency(ka.ts), 
             hd = 2, 
             type = c("lasso", "ridge","step", "lm"), 
             reps = 20, comb = c("median", "mean", "mode"),
             lags = NULL, keep = NULL, difforder = NULL, 
             outplot = TRUE, sel.lag = FALSE, 
             direct = FALSE, 
             allow.det.season = FALSE, 
             det.type = c("auto", "bin","trg"), 
             xreg = NULL, xreg.lags = NULL, xreg.keep = NULL,
             barebone = F, 
             model = NULL, 
             retrain = TRUE)
plot(model1)
model1$MSE
model1$lags
model1$fitted
pred1 = forecast(model1, 24)
# Ukuran kesalahan untuk data testing
error_1 = test-pred1$mean
rmse_1 = sqrt(model1$MSE)
mape_1 = mean(abs(error_1)/test)*100

# model 2 hd=3 #########################################
model2 = elm(train, 
             m = frequency(ka.ts), 
             hd = 3, 
             type = c("lasso", "ridge","step", "lm"), 
             reps = 20, comb = c("median", "mean", "mode"),
             lags = NULL, keep = NULL, difforder = NULL, 
             outplot = TRUE, sel.lag = FALSE, 
             direct = FALSE, 
             allow.det.season = FALSE, 
             det.type = c("auto", "bin","trg"), 
             xreg = NULL, xreg.lags = NULL, xreg.keep = NULL,
             barebone = F, 
             model = NULL, 
             retrain = TRUE)
plot(model2)
model2$MSE
model2$lags
model2$fitted
pred2 = forecast(model2, 24)
# Ukuran kesalahan untuk data testing
error_2 = test-pred2$mean
rmse_2 = sqrt(model2$MSE)
mape_2 = mean(abs(error_2)/test)*100

# model 3 hd=4 #########################################
model3 = elm(train, 
             m = frequency(ka.ts), 
             hd = 4, 
             type = c("lasso", "ridge","step", "lm"), 
             reps = 20, comb = c("median", "mean", "mode"),
             lags = NULL, keep = NULL, difforder = NULL, 
             outplot = TRUE, sel.lag = FALSE, 
             direct = FALSE, 
             allow.det.season = FALSE, 
             det.type = c("auto", "bin","trg"), 
             xreg = NULL, xreg.lags = NULL, xreg.keep = NULL,
             barebone = F, 
             model = NULL, 
             retrain = TRUE)
plot(model3)
model3$MSE
model3$lags
model3$fitted
pred3 = forecast(model3, 24)
# Ukuran kesalahan untuk data testing
error_3 = test-pred3$mean
rmse_3 = sqrt(model3$MSE)
mape_3 = mean(abs(error_3)/test)*100

# model 4 hd=5 #########################################
model4 = elm(train, 
             m = frequency(ka.ts), 
             hd = 5, 
             type = c("lasso", "ridge","step", "lm"), 
             reps = 20, comb = c("median", "mean", "mode"),
             lags = NULL, keep = NULL, difforder = NULL, 
             outplot = TRUE, sel.lag = FALSE, 
             direct = FALSE, 
             allow.det.season = FALSE, 
             det.type = c("auto", "bin","trg"), 
             xreg = NULL, xreg.lags = NULL, xreg.keep = NULL,
             barebone = F, 
             model = NULL, 
             retrain = TRUE)
plot(model4)
model4$MSE
model4$lags
model4$fitted
pred4 = forecast(model4, 24)
# Ukuran kesalahan untuk data testing
error_4 = test-pred4$mean
rmse_4 = sqrt(model4$MSE)
mape_4 = mean(abs(error_4)/test)*100

# model 5 hd=6 #########################################
model5 = elm(train, 
             m = frequency(ka.ts), 
             hd = 6, 
             type = c("lasso", "ridge","step", "lm"), 
             reps = 20, comb = c("median", "mean", "mode"),
             lags = NULL, keep = NULL, difforder = NULL, 
             outplot = TRUE, sel.lag = FALSE, 
             direct = FALSE, 
             allow.det.season = FALSE, 
             det.type = c("auto", "bin","trg"), 
             xreg = NULL, xreg.lags = NULL, xreg.keep = NULL,
             barebone = F, 
             model = NULL, 
             retrain = TRUE)
plot(model5)
model5$MSE
model5$lags
model5$fitted
pred5 = forecast(model5, 24)
# Ukuran kesalahan untuk data testing
error_5 = test-pred5$mean
rmse_5 = sqrt(model5$MSE)
mape_5 = mean(abs(error_5)/test)*100

# model 6 hd=7 #########################################
model6 = elm(train, 
             m = frequency(ka.ts), 
             hd = 7, 
             type = c("lasso", "ridge","step", "lm"), 
             reps = 20, comb = c("median", "mean", "mode"),
             lags = NULL, keep = NULL, difforder = NULL, 
             outplot = TRUE, sel.lag = FALSE, 
             direct = FALSE, 
             allow.det.season = FALSE, #data bukan musiman maka FALSE
             det.type = c("auto", "bin","trg"), 
             xreg = NULL, xreg.lags = NULL, xreg.keep = NULL,
             barebone = F, 
             model = NULL, 
             retrain = TRUE)
plot(model6)
model6$MSE
model6$lags
model6$fitted
pred6 = forecast(model6, 24)
# Ukuran kesalahan untuk data testing
error_6 = test-pred6$mean
rmse_6 = sqrt(model6$MSE)
mape_6 = mean(abs(error_6)/test)*100

# model 7 hd=8 #########################################
model7 = elm(train, 
             m = frequency(ka.ts), 
             hd = 8, 
             type = c("lasso", "ridge","step", "lm"), 
             reps = 20, comb = c("median", "mean", "mode"),
             lags = NULL, keep = NULL, difforder = NULL, 
             outplot = TRUE, sel.lag = FALSE, 
             direct = FALSE, 
             allow.det.season = FALSE, 
             det.type = c("auto", "bin","trg"), 
             xreg = NULL, xreg.lags = NULL, xreg.keep = NULL,
             barebone = F, 
             model = NULL, 
             retrain = TRUE)
plot(model7)
model7$MSE
model7$lags
model7$fitted
pred7 = forecast(model7, 24)
# Ukuran kesalahan untuk data testing
error_7 = test-pred7$mean
rmse_7 = sqrt(model7$MSE)
mape_7 = mean(abs(error_7)/test)*100

# model 8 hd=9 #########################################
model8 = elm(train, 
             m = frequency(ka.ts), 
             hd = 9, 
             type = c("lasso", "ridge","step", "lm"), 
             reps = 20, comb = c("median", "mean", "mode"),
             lags = NULL, keep = NULL, difforder = NULL, 
             outplot = TRUE, sel.lag = FALSE, 
             direct = FALSE, 
             allow.det.season = FALSE, 
             det.type = c("auto", "bin","trg"), 
             xreg = NULL, xreg.lags = NULL, xreg.keep = NULL,
             barebone = F, 
             model = NULL, 
             retrain = TRUE)
plot(model8)
model8$MSE
model8$lags
model8$fitted
pred8 = forecast(model8, 24)
# Ukuran kesalahan untuk data testing
error_8 = test-pred8$mean
rmse_8 = sqrt(model8$MSE)
mape_8 = mean(abs(error_8)/test)*100

# model 9 hd=10 #########################################
model9 = elm(train, 
             m = frequency(ka.ts), 
             hd = 10, 
             type = c("lasso", "ridge","step", "lm"), 
             reps = 20, comb = c("median", "mean", "mode"),
             lags = NULL, keep = NULL, difforder = NULL, 
             outplot = TRUE, sel.lag = FALSE, 
             direct = FALSE, 
             allow.det.season = FALSE, #data bukan musiman maka FALSE
             det.type = c("auto", "bin","trg"), 
             xreg = NULL, xreg.lags = NULL, xreg.keep = NULL,
             barebone = F, 
             model = NULL, 
             retrain = TRUE)
plot(model9)
model9$MSE
model9$lags
model9$fitted
pred9 = forecast(model9, 24)
# Ukuran kesalahan untuk data testing
error_9 = test-pred9$mean
rmse_9 = sqrt(model9$MSE)
mape_9 = mean(abs(error_9)/test)*100

##################################################################################
# menentukan nilai error terkecil
mape_1
mape_2
mape_3
mape_4 # model terbaik dgn nilai mape terkecil
mape_5
mape_6
mape_7
mape_8
mape_9

# Plot data aktual, fitted, prediksi
autoplot(ka.ts, color = 'blue') + 
  autolayer(model9$fitted, color = 'red') +
  autolayer(pred9, color = 'green')

