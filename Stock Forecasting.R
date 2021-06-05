library(tidyverse)  #Mandatory library
library(fpp2)       #Tools forecasting
library(lubridate)  #Manipulate date object
library(quantmod)   #Get historical stock price
library(xts)        #Extended time series
library(smooth)     #Forecasting method
library(MLmetrics)  #Make model evaluation
library(tseries)    #For stationary test

# Set Parameter ----
kode_saham <- c("^JKSE")
mulai <- "2011/01/01"
akhir <- "2021/06/04"

# Get Dataset
portofolio <- lapply(
  kode_saham, 
  function(x) {
    getSymbols(x, from = mulai, to = akhir, periodicity = "monthly", auto.assign = F)
  }
)

# Prapemrosesan Data
portofolio <- lapply(portofolio, Ad)
portofolio <- do.call(merge, portofolio)

class(portofolio)
first(portofolio, 5)
last(portofolio, 5)

portofolio$avg <- c()
for (i in 1:nrow(portofolio)) {
  portofolio$avg[i] <- mean(portofolio[i, 1:ncol(portofolio)])
}

# Memvisualisasikan Nilai Portofolio
autoplot(portofolio$avg) +
  labs(
    title = "Portofolio Saham",
    x = "Tahun",
    y = "Harga"
  ) 

# Set Data and Period for Forecasting ----
ntest <- 6
npred <- 2
ntrain <- length(portofolio$avg) - ntest

# Partisi Data
data.ts <- ts(portofolio$avg, frequency = 12, start = c(year(mulai), month(mulai)))

train.data <- ts(
  portofolio$avg[1:ntrain], 
  frequency = 12, 
  start = c(year(mulai), month(mulai))
)

test.data <- ts(
  portofolio$avg[(ntrain+1):length(portofolio$avg)], 
  frequency = 12, 
  start = c(
    year(as.Date(mulai)+months(ntrain+1)), 
    month(as.Date(mulai)+months(ntrain+1))
  )
)

# Fit Model
fit1 <- auto.arima(train.data)
summary(fit1)

fit2 <- ets(train.data, "ZAZ", damped = F, restrict = F, allow.multiplicative.trend = T)
summary(fit2)

dfit1 <- mstl(train.data) |>
  seasadj() |>
  auto.arima()
summary(dfit1)

dfit2 <- mstl(train.data) |>
  seasadj() |>
  ets(model = "ZAZ", damped = F, restrict = F, allow.multiplicative.trend = T)
summary(dfit2)

# Visualisasi Forecasting
autoplot(data.ts) +
  autolayer(forecast(fit1, h = ntest+npred), series = "ARIMA", PI = F) +
  autolayer(forecast(fit2, h = ntest+npred), series = "ETS", PI = F) +
  autolayer(forecast(dfit1, h = ntest+npred), series = "STL+ARIMA", PI = F) +
  autolayer(forecast(dfit2, h = ntest+npred), series = "STL+ETS", PI = F) +
  geom_vline(
    alpha = 0.5,
    linetype = "longdash",
    xintercept = c(
      year(as.Date(mulai)+months(ntrain-1)) + (month(as.Date(mulai)+months(ntrain-1))/12),
      year(as.Date(akhir)-months(1)) + (month(as.Date(akhir)-months(1))/12)
    )
  ) +
  scale_x_continuous(n.breaks = 12) +
  labs(x = "Period", y = "Price")

# Evaluasi Model
c.matrix <- list(
  fit1.acc = accuracy(forecast(fit1, npred), test.data),
  fit2.acc = accuracy(forecast(fit2, npred), test.data),
  fit3.acc = accuracy(forecast(dfit1, npred), test.data),
  fit4.acc = accuracy(forecast(dfit2, npred), test.data)
)
c.matrix

