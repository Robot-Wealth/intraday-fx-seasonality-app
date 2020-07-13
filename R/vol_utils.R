# Utilities for volatility analysis

library(tidyverse)
library(patchwork)

plot_equity_curve <- function(perf_df, title = "Intraday Seasonality After-Cost Equity Curve") {
  perf_df %>% 
    ggplot(aes(x = Date, y = PnL)) +
    geom_line() +
    labs(
      x = "Date",
      y = "Profit ($)",
      title = title
    )
}

# Plot day of the week return seasonality
dow_return_seasonality_plot <- function(perf_df) {
  perf_df %>%
    group_by(wday.lbl) %>%
    summarise(meanreturns = mean(returns)) %>%
    ggplot(aes(x=wday.lbl, y=meanreturns)) +
      geom_bar(stat='identity') +
      labs(
        x = "Weekday",
        y = "Mean $ returns",
        title = "Strategy Mean $ Returns by Day of Week" 
      ) 
}

# Plot day of the week volatility seasonality
dow_volatility_seasonality_plot <- function(perf_df) {
  perf_df %>%
    ggplot(aes(x=wday.lbl, y=logrange)) +
      geom_boxplot() +
      labs(
        title = "Day of the week volatility seasonality",
        x = "Weekday",
        y = "Log Range"
      )
}

# Plot mean/median range by day of week
dow_average_range_plot <- function(perf_df) {
  perf_df %>%
    group_by(wday.lbl) %>%
    mutate(
      Mean = mean(logrange, na.rm = TRUE),
      Median = median(logrange, na.rm = TRUE)
    ) %>%
    select(wday.lbl, Mean, Median) %>% 
    pivot_longer(
      cols = -wday.lbl,
      names_to = "Ave.Type",
      values_to = "value"
    ) %>%
    ggplot(aes(x = wday.lbl, y = value, fill = Ave.Type)) +
      geom_bar(stat='identity', position = 'dodge') +  
      labs(
        title = "Day of the week volatility seasonality",
        x = "Weekday",
        y = "Average Range",
        fill = "Ave.Type"
      )
}

compose_dow_seasonality_plot <- function(perf_df) {
  p1 <- dow_return_seasonality_plot(perf_df)
  p2 <- dow_volatility_seasonality_plot(perf_df)
  p3 <- dow_average_range_plot(perf_df)
  
  p1 + (p2 / p3)
}

source(here::here("R", "global.R"))

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

regime_cols <- gg_color_hue(4)
names(regime_cols) <- vol_regime_choices
regime_fill_scale <- scale_fill_manual(name = "volregime", values = regime_cols)

dow_vol_regime_plot <- function(perf_df, regimes = c("Low", "Normal", "High", "Extreme")) {
  perf_df %>%
    filter(volregime %in% regimes) %>% 
    group_by(volregime, wday.lbl) %>%
    summarise(
      meanreturns = mean(returns),
      count = n()
    ) %>%
    ggplot(aes(x = wday.lbl, y = meanreturns, fill = volregime)) +
      geom_bar(stat='identity', position = 'dodge') +
      # geom_text(stat = "identity",  aes(label = count), check_overlap = TRUE) +
      regime_fill_scale +
      ggtitle('Strategy Mean $ Returns by Day of Week') +
      xlab('Weekday') +
      ylab('Mean $ returns')
}

# usage
# plot_equity_curve(cdata)
# dow_return_seasonality_plot(cdata)
# dow_return_seasonality_plot(cdata)
# dow_average_range_plot(cdata)
# compose_dow_seasonality_plot(cdata)
# dow_vol_regime_plot(cdata, regimes = c("Low", "Normal", "High", "Extreme"))

  
