library(tidyverse)
library(stringr)
library(patchwork)
library(viridis)

local_ccy_from_tz <- function(timezone) {
  switch(
    timezone,
    "ET" = "USD",
    "CET" = "EUR",
    "JST" = "JPY"
  )
}

# heatmap from performance dataframe faceted by Ticker
heatmap_plot <- function(performance_df, tickers, timezone, years = "2009-2020", detrend = FALSE, hour_offset = 0, show_values) {
  
  localccy <- local_ccy_from_tz(timezone)
  
  performance_df %>% 
    filter(
      Ticker %in% tickers, 
      LocalCcy == localccy,
      Years == years,
      Detrended == detrend,
      Offset == hour_offset
    ) %>% 
    ggplot(aes(StartHour, EndHour)) + 
      geom_tile(aes(fill = IR)) +  # , colour = "white"
      {if (show_values) geom_text(aes(label = round(IR, 1)))} +
      scale_fill_viridis_c() +
      labs(
        title = glue::glue("{localccy} in {timezone}, {years}, Offset {hour_offset}"),
        subtitle = "Information Ratio (x100)",
        x = "Start Hour",
        y = "End Hour"
      ) +
      facet_wrap(~Ticker)
}

# heatmap_plot(performance_df, c("EURAUD", "EURCAD"), "CET")

# heatmap from performance dataframe faceted on years
heatmap_facet_year_plot <- function(performance_df, ticker, timezone, detrend = FALSE, hour_offset = 0, ir_range, show_values) {
  
  performance_df %>% 
    filter(Ticker == ticker) %>% 
    ggplot(aes(StartHour, EndHour)) + 
      geom_tile(aes(fill = IR)) +  # , colour = "white"
    {if (show_values) geom_text(aes(label = round(IR, 1)))} +
      # scale_fill_viridis_c() +
      scale_fill_gradientn(colors = viridis_pal()(9), limits = ir_range, na.value = "#FDE725FF") + 
      labs(
        title = glue::glue("{ticker}"),
        x = "Start Hour",
        y = "End Hour"
      ) +
      facet_wrap(~Years) + 
      theme(legend.position = "none")
  
}

# compose a plot of multiple heatmap_facet_year_plots
compose_facet_year_heatmaps <- function(performance_df, tickers, timezone, detrend = FALSE, hour_offset = 0, show_values) {
  
  localccy <- local_ccy_from_tz(timezone)
  
  df <- performance_df %>% 
    filter(
      Ticker %in% tickers, 
      LocalCcy == localccy,
      Detrended == detrend,
      Offset == hour_offset,
      Years != "2009-2020"
    ) 
  
  # determine range for colour scheme - needs to cover IR range across all assets in plot
  ir_range <- df %>% 
    summarise(min = min(IR), max = max(IR))
  
  plots <- list()
  for(i in seq_along(tickers)) {
    p <- heatmap_facet_year_plot(df, tickers[i], timezone, detrend, hour_offset, c(ir_range$min, ir_range$max), show_values)
    
    if(i == length(tickers)) {
      p <- p + theme(legend.position = "right")
    }
    
    plots[[i]] <- p
  }
  
  if(detrend) {
    title = glue::glue("Information Ratio (x100) {timezone}, Offset {hour_offset}, Detrended")
  } else {
    title =  glue::glue("Information Ratio (x100) {timezone}, Offset {hour_offset}")
  }
  
  wrap_plots(plots) + 
    plot_layout(guides = "collect", ncol = 2) +
    plot_annotation(title = title)
}

# heatmap from performance dataframe faceted on ticker
heatmap_facet_asset_plot <- function(performance_df, year_subset, timezone, detrend = FALSE, hour_offset = 0, ir_range, show_values) {
  
  performance_df %>% 
    filter(Years == year_subset) %>% 
    ggplot(aes(StartHour, EndHour)) + 
    geom_tile(aes(fill = IR)) +  # , colour = "white"
    {if (show_values) geom_text(aes(label = round(IR, 1)))} +
    # scale_fill_viridis_c() +
    scale_fill_gradientn(colors = viridis_pal()(9), limits = ir_range, na.value = "#FDE725FF") + 
    labs(
      title = glue::glue("{year_subset}"),
      x = "Start Hour",
      y = "End Hour"
    ) +
    facet_wrap(~Ticker) + 
    theme(legend.position = "none")
  
}

# compose a plot of multiple heatmap_facet_asset_plot
# determining ir range:
  # filter on tickers in case we want to restrict any tickers in the full dataset
  # filter on year subsets - user will select several using checkbox - need dynamic ui
  # disable asset selector on time subset by tickers tab
compose_facet_asset_heatmaps <- function(performance_df, tickers, year_subsets, timezone, detrend = FALSE, hour_offset = 0, show_values) {
  
  localccy <- local_ccy_from_tz(timezone)
  
  df <- performance_df %>% 
    filter(
      Ticker %in% tickers, 
      LocalCcy == localccy,
      Detrended == detrend,
      Offset == hour_offset,
      Years %in% year_subsets
    ) 
  
  # determine range for colour scheme - needs to cover IR range across all assets in plot
  ir_range <- df %>% 
    summarise(min = min(IR), max = max(IR))
  
  plots <- list()
  for(i in seq_along(year_subsets)) {
    p <- heatmap_facet_asset_plot(df, year_subsets[i], timezone, detrend, hour_offset, c(ir_range$min, ir_range$max), show_values)
    
    if(i == length(year_subsets)) {
      p <- p + theme(legend.position = "right")
    }
    
    plots[[i]] <- p
  }
  
  if(detrend) {
    title = glue::glue("Information Ratio (x100) {timezone}, Offset {hour_offset}, Detrended")
  } else {
    title =  glue::glue("Information Ratio (x100) {timezone}, Offset {hour_offset}")
  }
  
  wrap_plots(plots) + 
    plot_layout(guides = "collect", ncol = 2) +
    plot_annotation(title = title)
}
# compose_facet_asset_heatmaps(performance_df, c("EURUSD", "AUDUSD", "GBPUSD", "USDCAD", "USDCHF", "USDJPY"), c("2009-2011", "2012-2014"), "ET", detrend, hour_offset)
# compose_heatmaps(performance_df, c("EURUSD", "AUDUSD", "GBPUSD"), "ET", detrend = FALSE, hour_offset = 0)
