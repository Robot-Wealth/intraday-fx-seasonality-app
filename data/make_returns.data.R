library(tidyverse)
# Calculate Returns and Convert to Eastern Time (this is now loaded on startup)
usd_returns <-  usd_prices_df %>%
  group_by(Ticker) %>%
  mutate(returns = Close / lag(Close) - 1)

datetime <- as.POSIXct(paste(usd_returns$Date, usd_returns$Time), format="%Y-%m-%d %H:%M", tz='UTC')
attributes(datetime)$tzone <- 'America/New_York'
usd_returns$datetime <- datetime

returns_df <- usd_returns %>%
  filter(Ticker %in% c("AUDUSD", "CADUSD", "CHFUSD", "EURUSD", "GBPUSD", "JPYUSD")) %>%
  mutate(hour = hour(datetime),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime)) %>%
  select(Ticker, datetime, hour, year, month, day, returns) %>%
  na.omit()

save(returns_df, file = "usd_returns_df.RData")