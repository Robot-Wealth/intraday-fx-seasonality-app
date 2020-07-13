library(tidyverse)
theme_set(theme_bw())

timezone_list <- c("USD in ET" = "USD", "EUR in CET" = "EUR", "JPY in JST" = "JPY")
timezone_map <- c("USD" = "ET", "EUR" = "CET", "JPY" = "JST")
assets_list <- list(
  "USD" = c("AUDUSD", "NZDUSD", "CHFUSD", "EURUSD", "GBPUSD", "JPYUSD", "CADUSD"),
  "EUR" = c("AUDEUR", "NZDEUR", "CHFEUR", "GBPEUR", "JPYEUR", "USDEUR", "CADEUR"),
  "JPY" = c("AUDJPY", "NZDJPY", "CHFJPY", "EURJPY", "GBPJPY", "USDJPY", "CADJPY")
)
hm_assets_list <- list(
  "USD" = c("AUDUSD", "NZDUSD", "USDCHF", "EURUSD", "GBPUSD", "USDJPY"),
  "EUR" = c("EURAUD", "EURNZD", "EURCHF", "EURGBP", "EURJPY", "EURUSD"),
  "JPY" = c("AUDJPY", "NZDJPY", "CHFJPY", "EURJPY", "GBPJPY", "USDJPY")
)
hm_date_ranges <- c("2009-2011", "2012-2014", "2015-2017", "2018-2020", "2009-2020")
vol_regime_choices <- c("Low", "Normal", "High", "Extreme")
