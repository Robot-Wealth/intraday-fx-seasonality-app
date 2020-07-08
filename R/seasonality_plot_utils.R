library(gganimate)
library(transformr)
library(glue)

make_seasonal_data <- function(returns_df, tickers = "EURUSD", years = 2009:2020) {
  returns_df %>%
    filter(
      Ticker %in% tickers,
      year %in% years
    ) %>%
    group_by(Ticker) %>% 
    mutate(returns_detrended = returns - mean(returns, na.rm = TRUE)) %>% 
    group_by(Ticker, hour) %>%
      summarise(
        meanreturns = mean(returns * 100),
        meanreturns_detrended = mean(returns_detrended * 100)) %>%
      mutate(
        cumreturns = cumsum(meanreturns),
        cumreturns_detrended = cumsum(meanreturns_detrended)) %>% 
      pivot_longer(cols = c(meanreturns, cumreturns, meanreturns_detrended, cumreturns_detrended), names_to = "return_type", values_to = "return")
  
}

seasonality_barlineplot <- function(returns_df, tickers = "EURUSD", timezone = "ET", years = 2009:2020, detrend = TRUE) {
  
  seasonal_df <- returns_df %>%
    make_seasonal_data(tickers, years) 
  
  if(detrend) 
  {
    r_type = "meanreturns_detrended"
    cumr_type = "cumreturns_detrended"
    title = glue::glue('Detrended Mean and Cumulative Returns by Hour ({timezone}) {years[1]} - {years[length(years)]}')
    y_label = "Detrended Returns %"
  } else {
    r_type = "meanreturns"
    cumr_type = "cumreturns"
    title = glue::glue('Detrended Mean and Cumulative Returns by Hour ({timezone}) {years[1]} - {years[length(years)]}')
    y_label = "Returns %"
  }
  
    ggplot() +
      geom_bar(
        data = seasonal_df %>% filter(return_type == r_type),
        aes(x = as.factor(hour), y = return, fill = Ticker),
        stat='identity', 
        position = position_dodge()
      ) +
      geom_line(
        data = seasonal_df %>% filter(return_type == cumr_type),
        aes(x = as.factor(hour), y = return, colour = Ticker, group = Ticker),
        size = 1.
      ) +
      geom_vline(xintercept = "9", colour = "blue", size = 1., linetype = 2) +
      geom_vline(xintercept = "16", colour = "blue", size = 1., linetype = 2) +
      labs(
        title = title,
        x = glue::glue("Hour ({timezone})"),
        y = y_label
      ) +
      coord_cartesian(ylim = c(-0.06, 0.06)) +
      facet_wrap(~Ticker) +
      theme(legend.position = 'none')
}


# seasonal_df <- returns_df %>%
#   make_seasonal_data(ticker_subset, 2009:2020)
# 
# seasonality_barlineplot(returns_df)
# ticker_subset <- c('EURUSD','GBPUSD','CADUSD', 'JPYUSD','AUDUSD','CHFUSD')
# seasonality_barlineplot(returns_df, ticker_subset, years = 2009:2009)
# 
# # cumulative mean return by year

seasonality_facet_year_plot <- function(returns_df, tickers = "EURUSD", timezone = "ET", detrend = TRUE) {
  
  if(detrend) 
  {
    cumr_type = "cumreturns_detrended"
    title = glue::glue("Detrended Cumulative Mean Hourly Returns in {timezone}")
    y_label = "Detrended Returns %"
  } else {
    cumr_type = "cumreturns"
    title = glue::glue("Cumulative Mean Hourly Returns in {timezone}")
    y_label = "Returns %"
  }
  
  returns_df %>%
    filter(Ticker %in% tickers) %>% 
    group_by(Ticker, year) %>%
    mutate(returns_detrended = returns - mean(returns, na.rm = TRUE)) %>% 
    group_by(Ticker, year, hour) %>%
    summarise(
      meanreturns = mean(returns * 100),
      meanreturns_detrended = mean(returns_detrended*100)
    ) %>%
    mutate(
      cumreturns = cumsum(meanreturns),
      cumreturns_detrended = cumsum(meanreturns_detrended)) %>%
    pivot_longer(cols = c(cumreturns, cumreturns_detrended), names_to = "return_type", values_to = "return") %>% 
    filter(return_type == cumr_type) %>%
    ggplot(aes(x = hour, y = return, colour = Ticker)) + #, group = Ticker)) +
    geom_line() +
    geom_vline(xintercept = 9, colour = "blue", size = 1., linetype = 2) +
    geom_vline(xintercept = 16, colour = "blue", size = 1., linetype = 2) +
    facet_wrap(~year) +
    labs(
      x = glue::glue("Hour ({timezone})"),
      y = y_label,
      colour = "Ticker",
      title = title,
      subtitle = "Year by Asset"
    ) +
    theme(legend.position = "bottom")
}

 
# cumulative mean return by asset
seasonality_facet_asset_plot <- function(returns_df, tickers = "EURUSD", timezone = "ET", detrend = TRUE) {
  
  if(detrend) 
  {
    cumr_type = "cumreturns_detrended"
    title = glue::glue("Detrended Cumulative Mean Hourly Returns in {timezone}")
    y_label = "Detrended Returns %"
  } else {
    cumr_type = "cumreturns"
    title = glue::glue("Cumulative Mean Hourly Returns in {timezone}")
    y_label = "Returns %"
  }
  
  returns_df %>%
    filter(Ticker %in% tickers) %>% 
    group_by(Ticker, year) %>%
    mutate(returns_detrended = returns - mean(returns, na.rm = TRUE)) %>% 
    group_by(Ticker, year, hour) %>%
    summarise(
      meanreturns = mean(returns * 100),
      meanreturns_detrended = mean(returns_detrended*100)
    ) %>%
    mutate(
      cumreturns = cumsum(meanreturns),
      cumreturns_detrended = cumsum(meanreturns_detrended)) %>%
    pivot_longer(cols = c(cumreturns, cumreturns_detrended), names_to = "return_type", values_to = "return") %>% 
    filter(return_type == cumr_type) %>%
    ggplot(aes(x = hour, y = return, colour = as.factor(year))) + #, group = Ticker)) +
    geom_line() +
    geom_vline(xintercept = 9, colour = "blue", size = 1., linetype = 2) +
    geom_vline(xintercept = 16, colour = "blue", size = 1., linetype = 2) +
    facet_wrap(~Ticker) +
    labs(
      x = glue::glue("Hour ({timezone})"),
      y = y_label,
      colour = "Year",
      title = title,
      subtitle = "Asset by Year"
    ) +
    theme(legend.position = "bottom")
}

## Seasonality boxplot - these take too long to run really...
seasonality_boxplot <- function(ticker = 'EURUSD', years = 2009:2019) {
  returns_df %>%
    filter(Ticker %in% ticker) %>%
    filter(year %in% years) %>%
    ggplot(aes(x=as.factor(hour), y=returns)) +
    geom_boxplot() +
    geom_jitter(width=0.1, alpha=0.2) +
    xlab("Hour (ET)") + 
    ylab("Hourly returns %") +
    labs(title = paste(ticker, 'Returns by Hour (ET)', years[1], '-', years[length(years)]))
}
# seasonality_boxplot('EURUSD', 2009:2010)


# animated Asset by Year plot
# anim <- returns_df %>%  
#   group_by(Ticker, year, hour) %>%
#   summarise(meanreturns = mean(returns * 100)) %>%
#   mutate(cumreturns = cumsum(meanreturns)) %>%
#   ggplot(aes(x = hour, y = cumreturns, colour = as.factor(year))) + #, group = Ticker)) +
#   geom_line(size = 1.) +
#   transition_time(as.integer(year)) +
#   ease_aes('linear') +
#   shadow_mark(colour = "grey70") +
#   labs(title = ) +
#   facet_wrap(~Ticker) +
#   labs(
#     x = "Hour",
#     y = "Return",
#     colour = "Year",
#     title = "Mean Cumulative Hourly Returns: Asset by Year"#,
#     #subtitle = 'Year: {frame_time}'
#   ) +
#   theme_bw() +
#   theme(legend.position = "none")
# 
# animate(anim, height = 6, width = 6, units = "in", res = 120)
# anim_save(filename = "asset_by_year_large5_nolegend.gif")
# 
# # animated Year by Asset plot
# anim2 <- returns_df %>%
#   group_by(Ticker, year, hour) %>%
#   summarise(meanreturns = mean(returns * 100)) %>%
#   mutate(cumreturns = cumsum(meanreturns)) %>%
#   ggplot(aes(x = hour, y = cumreturns, colour = Ticker)) + #, group = Ticker)) +
#   geom_line(size = 1.) +
#   transition_time(as.integer(year)) +
#   ease_aes('linear') +
#   shadow_mark(colour = "grey70") +
#   labs(
#     x = "Hour",
#     y = "Return",
#     colour = "Ticker",
#     title = "Mean Cumulative Hourly Returns: Year by Asset",
#     subtitle = 'Year: {frame_time}'
#   ) +
#   theme_bw() +
#   theme(legend.position = "bottom")
# 
# animate(anim2, height = 6, width = 6, units = "in", res = 120)
# anim_save(filename = "year_by_asset_large4.gif")
