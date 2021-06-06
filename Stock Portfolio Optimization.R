library(tidyverse)           #It's tidyverse
library(quantmod)            #Get stock data
library(PortfolioAnalytics)  #Optimize portfolio
library(derivmkts)           #Calculate volatility

kode_saham <- c()
mulai <- "2011/01/01"
akhir <- "2021/06/04"

## Get Dataset ----
portofolio <- lapply(
  kode_saham, 
  function(x) {
    getSymbols(x, from = mulai, to = akhir, periodicity = "daily", auto.assign = F)
  }
) 

portofolio <- na.omit(portofolio)
portofolio <- lapply(portofolio, Ad)
portofolio <- do.call(merge, portofolio)

class(portofolio)
first(portofolio, 5)
last(portofolio, 5)

## Menghitung Return Saham dan Portofolio ----
s.return <- Return.calculate(portofolio)[-1]
head(s.return)

p.return <- Return.portfolio(s.return, verbose = T)
head(p.return)

p.return.rebalanced <- Return.portfolio(s.return, rebalance_on = "months", verbose = T)
head(p.return.rebalanced)

p.return.comparison <- cbind(p.return, p.return.rebalanced)
colnames(p.return.comparison) <- c("non-rebalanced", "rebalanced")
head(p.return.comparison)

table.AnnualizedReturns(p.return.comparison, Rf = 0.1/252)

## Optimisasi Portofolio ----
# Spesifikasi optimasi
p.opt <- portfolio.spec(colnames(s.return)) |>
  add.constraint(type = "full_investment") |>
  add.constraint(type = "long_only") |>
  add.constraint(type = "box", min = 0, max = 0.7)
p.opt

p.minvar <- p.opt |>
  add.objective(type = "risk", name = "StdDev")
p.minvar

p.meanvar <- p.minvar |>
  add.objective(type = "return", name = "mean")
p.meanvar

# Menghitung hasil optimisasi
minvar.op <- optimize.portfolio(s.return, portfolio = p.minvar, optimize_method = "random")
minvar.op

meanvar.op <- optimize.portfolio(s.return, portfolio = p.meanvar, optimize_method = "random")
meanvar.op

minvar.rt <- Return.portfolio(
  R = s.return, 
  weights = extractWeights(minvar.op), 
  rebalance_on = "months",
  verbose = T
)
head(minvar.rt)

meanvar.rt <- Return.portfolio(
  R = s.return, 
  weights = extractWeights(meanvar.op), 
  rebalance_on = "months",
  verbose = T
)
head(meanvar.rt)

optimised.p <- cbind(minvar.rt, meanvar.rt)
colnames(optimised.p) <- c("min. variance", "mean variance")
table.AnnualizedReturns(optimised.p, Rf = 0.1/252)
