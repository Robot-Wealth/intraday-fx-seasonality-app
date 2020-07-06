library(gganimate)
library(transformr)

# THE FLOW:
# calculate returns
# convert to timezone of interest
# calculate mean and cumulative hourly returns
# plot mean hourly and cumulative together

#


make_seasonal_data <- function(returns_df, tickers = "EURUSD", years = 2009:2020) {
  returns_df %>%
    filter(
      Ticker %in% tickers,
      year %in% years
    ) %>%
    group_by(Ticker, hour) %>%
    summarise(meanreturns = mean(returns * 100)) %>%
    mutate(cumreturns = cumsum(meanreturns)) %>% 
    pivot_longer(cols = c(meanreturns, cumreturns), names_to = "return_type", values_to = "return")
  
}

seasonality_barlineplot <- function(returns_df, tickers = 'EURUSD', years = 2009:2020) {
  
  seasonal_df <- returns_df %>%
    make_seasonal_data(tickers, years) 
  
    ggplot() +
      geom_bar(
        data = seasonal_df %>% filter(return_type == "meanreturns"),
        aes(x = as.factor(hour), y = return, fill = Ticker),
        stat='identity', 
        position = position_dodge()
      ) +
      geom_line(
        data = seasonal_df %>% filter(return_type == "cumreturns"),
        aes(x = as.factor(hour), y = return, colour = Ticker, group = Ticker),
        size = 1.
      ) +
      labs(
        title = paste('Mean and Cumulative Returns by Hour (ET)', years[1], '-', years[length(years)]),
        x = "Hour (ET)",
        y = "Returns %"
      ) +
      coord_cartesian(ylim = c(-0.11, 0.11)) +
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

seasonality_facet_year_plot <- function(returns_df) {
  returns_df %>%
    group_by(Ticker, year, hour) %>%
    summarise(meanreturns = mean(returns * 100)) %>%
    mutate(cumreturns = cumsum(meanreturns)) %>%
    ggplot(aes(x = hour, y = cumreturns, colour = Ticker)) + #, group = Ticker)) +
    geom_line() +
    facet_wrap(~year) +
    labs(
      x = "Hour",
      y = "Return",
      colour = "Ticker",
      title = "Mean Cumulative Hourly Returns",
      subtitle = "Year by Asset"
    ) +
    theme(legend.position = "bottom")
}

 
# cumulative mean return by asset
seasonality_facet_asset_plot <- function(returns_df) {
  returns_df %>%
    group_by(Ticker, year, hour) %>%
    summarise(meanreturns = mean(returns * 100)) %>%
    mutate(cumreturns = cumsum(meanreturns)) %>%
    ggplot(aes(x = hour, y = cumreturns, colour = as.factor(year))) + #, group = Ticker)) +
    geom_line() +
    facet_wrap(~Ticker) +
    labs(
      x = "Hour",
      y = "Return",
      colour = "Year",
      title = "Mean Cumulative Hourly Returns",
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
