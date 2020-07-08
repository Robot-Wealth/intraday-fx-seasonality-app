library(tidyverse)
library(lubridate)
library(here)

# load(here::here("usd_prices_df.RData"))
load(here::here("data", "returns_df.RData"))
load(here::here("data", "performance_df.RData"))

theme_set(theme_bw())

