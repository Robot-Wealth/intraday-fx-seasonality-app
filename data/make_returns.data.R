library(tidyverse)
# Calculate Returns and Convert to Eastern Time (this is now loaded on startup)

DATA_FOLDER <- 'C:/Users/Kris/ResilioSync/FXBootcamp'
source(paste0(DATA_FOLDER,'/Code/tools/r-tools/data-utils.R'))

asset_list <- get_asset_list("AssetsDWX-FX-USD")
raw_prices_df <- get_hourly_OHLC("NZDJPY")
usd_prices_df <- convert_common_quote_currency(raw_prices_df, quote_currency = 'JPY')
assetNames <- distinct(usd_prices_df, Ticker)

usd_returns <-  usd_prices_df %>%
  group_by(Ticker) %>%
  mutate(returns = Close / lag(Close) - 1)

datetime <- as.POSIXct(paste(usd_returns$Date, usd_returns$Time), format="%Y-%m-%d %H:%M", tz='UTC')
attributes(datetime)$tzone <- 'Japan'  # 'America/New_York'
usd_returns$datetime <- datetime

df <- usd_returns %>%
  filter(Ticker %in% c("NZDJPY")) %>%
  mutate(hour = hour(datetime),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime)) %>%
  select(Ticker, datetime, hour, year, month, day, returns) %>%
  na.omit()

df <- returns_df

load(here::here("data", "returns_df.RData"))
returns_df <- bind_rows(list(returns_df, df))
returns_df <- returns_df %>% arrange(datetime)

save(returns_df, file = here::here("data", "returns_df.RData"))

# strategy ensemble volatility data =====

library(timetk)

# backtest performance data
perf <- read_csv(here::here("data", "intraday_seasonality_ensemble_pnl.csv"))
perf <- perf %>%
  mutate(returns = PnL - dplyr::lag(PnL)) %>%
  na.omit()

# logrange data
tickers <- c("EURAUD", "EURCHF", "EURNZD", "EURUSD", "GBPUSD", "USDCHF", "USDJPY")
prices_df <- get_daily_OHLC(tickers)

prices_df %>%
  ggplot(aes(x = Date, y = Close)) +
  geom_line() +
  facet_wrap(~Ticker, scales='free')

# Calculate log range
logrange <- prices_df %>%
  mutate(logrange = (log(High/Low) * 100)) %>%
  group_by(Date) %>%
  summarise(logrange = mean(logrange))

# Augment time signature
sdata <- tk_augment_timeseries_signature(perf) %>% 
  select(Date, PnL, returns, wday, wday.lbl)

cdata <- sdata %>% 
  left_join(logrange, by = "Date")

# Calculate returns by vol regimes
breaks <- quantile(cdata$logrange, na.rm = TRUE)

cdata <- cdata %>%
  mutate(volregime = factor(case_when(
    logrange <= breaks[2] ~ "Low", 
    (logrange > breaks[2] & logrange <= breaks[3]) ~ "Normal",
    (logrange > breaks[3] & logrange < breaks[4]) ~ "High",
    TRUE ~ "Extreme"), levels = c("Low", "Normal", "High", "Extreme"), ordered = TRUE)
  )

save(cdata, file = here::here("data", "vol_analysis_df.RData"))
