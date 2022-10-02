# nomor 4
d=read.csv("C:/Users/DIANWL/Documents/# Semester 5/6 PRAK ARW/LAPRAK & ASSESMENT(s)/Assesment 2.csv")
head(d, 5)

d.ts=ts(d$tpt, start = 2011, frequency = 2)
d.ts

d.hw = HoltWinters(d.ts, alpha = NULL, beta = FALSE, gamma = FALSE)
d.hw

sse = d.hw$SSE
mse = d.hw$SSE/NROW(d.hw$fitted)
rmse = sqrt(mse)
mape = mean(abs((d.ts-d.hw$fitted[,1])/d.ts), na.rm = TRUE)*100
cbind(sse, mse, rmse, mape)

# nomor 7
e=read.csv("C:/Users/DIANWL/Documents/# Semester 5/6 PRAK ARW/LAPRAK & ASSESMENT(s)/Assesment 2_2.csv")
head(e, 5)

e.ts=ts(e$data, start = 2006)

e.hw = HoltWinters(e.ts, alpha = 0.1, beta = NULL, gamma = FALSE)
e.hw
sse = e.hw$SSE
mse = e.hw$SSE/NROW(e.hw$fitted)
rmse = sqrt(mse)
mape = mean(abs((e.ts-e.hw$fitted[,1])/e.ts), na.rm = TRUE)*100
cbind(sse, mse, rmse, mape)

e2.hw = HoltWinters(d.ts, alpha = 0.7, beta = FALSE, gamma = FALSE)
e2.hw
sse = e2.hw$SSE
mse = e2.hw$SSE/NROW(e2.hw$fitted)
rmse = sqrt(mse)
mape = mean(abs((e.ts-e2.hw$fitted[,1])/e.ts), na.rm = TRUE)*100
cbind(sse, mse, rmse, mape)
