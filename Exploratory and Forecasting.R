library(tidyverse)
library(quantmod)
library(fpp3)

# Set Parameter Pengambilan Data ----
kode_saham <- c("^JKSE", "BBRI.JK", "TLKM.JK", "ADRO.JK", "TINS.JK")
mulai <- "2016/06/01"
akhir <- "2021/06/18"

# Mengambil Data ----
portofolio <- lapply(
  kode_saham, 
  \(x) {
    getSymbols(x, from = mulai, to = akhir, periodicity = "monthly", auto.assign = F)
  }
)

# Prapemrosesan Data ----
portofolio <- lapply(portofolio, Ad)
portofolio <- do.call(merge, portofolio)
head(portofolio)

tidy_portofolio <- portofolio |>
  as_tibble(rownames = "periode") |>
  mutate(periode = yearmonth(periode)) |> 
  pivot_longer(-periode, names_to = "indeks", values_to = "harga") 

# Eksplorasi Performa Growth ----
tidy_portofolio |>
  group_by(indeks) |>
  mutate(growth = harga/harga[1]) |> 
  as_tsibble(index = periode, key = indeks) |> 
  arrange(periode) |>
  autoplot(growth) +
  ggtitle("Performa Pertumbuhan Saham Terhadap IHSG")
  
tidy_portofolio |>
  mutate(komposit = if_else(indeks == "JKSE.Adjusted", "IHSG", "Portofolio")) |> 
  group_by(periode, komposit) |>
  summarise(total_value = sum(harga)) |>
  group_by(komposit) |>
  mutate(portofolio_growth = total_value/total_value[1]) |> 
  as_tsibble(index = periode, key = komposit) |> 
  autoplot(portofolio_growth) +
  ggtitle("Performa Pertumbuhan Portofolio Terhadap IHSG")

# Forecasting Nilai Portofolio ----
sum_portofolio <- tidy_portofolio |> 
  filter(indeks != "JKSE.Adusted") |> 
  group_by(periode) |> 
  summarise(nilai_portofolio = sum(harga)) |> 
  as_tsibble(index = periode)

# Set Parameter Data Split ----
ntest <- 4
npred <- 1
ntrain <- nrow(sum_portofolio) - ntest

# Fitting Model ----
training_set <- sum_portofolio |>
  slice_min(periode, n = ntrain)

model_fit <- training_set |>
  model(
    arima = ARIMA(nilai_portofolio),
    ets = ETS(nilai_portofolio),
    arima_stl = ARIMA(nilai_portofolio ~ season()),
    ets_stl = ETS(nilai_portofolio ~ season())
  )

forecast_value <- model_fit |>
  forecast(h = ntest+npred)

forecast_value |>
  autoplot(sum_portofolio, level = NULL) +
  ggtitle("Nilai Forecast Portofolio")

forecast_accuracy <- forecast_value |>
  accuracy(sum_portofolio)

# Output Report ----
forecast_value
forecast_accuracy
