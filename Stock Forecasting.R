library(tidyverse)  #Mandatory library
library(lubridate)  #Manipulate date object
library(fpp2)       #Forecasting tools
library(quantmod)   #Get historical stock price
library(xts)        #Extended time series
library(ggpubr)     #Visualization tools
library(smooth)     #Forecasting method
library(MLmetrics)  #Make model evaluation
library(tseries)    #For stationary test

# Set Parameter ----
kode_saham <- c("BBRI.JK", "TLKM.JK", "TINS.JK", "ANTM.JK")
mulai <- "2006/01/01"
akhir <- "2021/05/01"

# Get Dataset
portofolio <- lapply(
  kode_saham, 
  function(x) {
    getSymbols(
      x, 
      from = mulai, 
      to = akhir, 
      periodicity = "monthly", 
      auto.assign = FALSE
    )
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
ntest <- 24
npred <- 12
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
fit1

fit2 <- ets(train.data, restrict = FALSE, allow.multiplicative.trend = F)
fit2

# Visualisasi Forecasting
autoplot(data.ts) +
  autolayer(forecast(fit1, h = ntest+npred), series = "ARIMA", PI = FALSE) +
  autolayer(forecast(fit2, h = ntest+npred), series = "ETS", PI = FALSE) +
  labs(x = "Period", y = "Price")

# Evaluasi Model
c.matrix <- list(
  fit1.acc = accuracy(forecast(fit1, npred), test.data),
  fit2.acc = accuracy(forecast(fit2, npred), test.data)
)
c.matrix
