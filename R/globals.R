library(tidyverse)
theme_set(theme_bw())

timezone_list <- c("USD in ET" = "USD", "EUR in CET" = "EUR", "JPY in JST" = "JPY")
timezone_map <- c("USD" = "ET", "EUR" = "CET", "JPY" = "JST")
assets_list <- list(
  "USD" = c("AUDUSD", "CADUSD", "CHFUSD", "EURUSD", "GBPUSD", "JPYUSD"),
  "EUR" = c("AUDEUR", "CADEUR", "CHFEUR", "GBPEUR", "JPYEUR", "USDEUR"),
  "JPY" = c("AUDJPY", "CADJPY", "CHFJPY", "EURJPY", "GBPJPY", "USDJPY")
)
hm_assets_list <- list(
  "USD" = c("AUDUSD", "USDCAD", "USDCHF", "EURUSD", "GBPUSD", "USDJPY"),
  "EUR" = c("EURAUD", "EURCAD", "EURCHF", "EURGBP", "EURJPY", "EURUSD"),
  "JPY" = c("AUDJPY", "CADJPY", "CHFJPY", "EURJPY", "GBPJPY", "USDJPY")
)
hm_date_ranges <- c("2009-2011", "2012-2014", "2015-2017", "2018-2020", "2009-2020")