library(tidyverse)
library(here)

# get list of performance files, calculate lengths of filenames
temp <- list.files(path = here::here("data", "perf_logs"), pattern = "*.csv")
map_dbl(temp, ~length(flatten_chr(str_split(.x, "_"))))
# length == 8: detrended, length == 7: not detrended

# read in and process each file
process_perf_log <- function(filename) {
  chars <- str_split(filename, "_") %>% flatten_chr()
  if(length(chars) == 8)
  {
    detrended = TRUE
    ccy = chars[3]
    offset = chars[6]
    years = glue::glue("{chars[7]}-{str_split(chars[8], '.csv')[[1]][1]}")
  } else if(length(chars) == 7) {
    detrended = FALSE
    ccy = chars[3]
    offset = chars[5]
    years = glue::glue("{chars[6]}-{str_split(chars[7], '.csv')[[1]][1]}")
  }
  
  df <- read_csv(here::here("data", "perf_logs", filename))
  df %>% 
    mutate(
      Detrended = detrended,
      LocalCcy = ccy,
      Offset = offset,
      Years = years
    )
}

performance_df <- temp %>% 
  map(process_perf_log) %>% 
  bind_rows()

performance_df <- performance_df %>% 
  mutate(
    Timezone = case_when(
      LocalCcy == "USD" ~ "ET",
      LocalCcy == "EUR" ~ "CET",
      LocalCcy == "JPY" ~ "JST"
    ),
    Asset = str_remove(Asset, "/")
  ) %>% 
  rename("Ticker" = Asset)

# check we have expected number of observations
assets_per_tz <- 7
params <- list(
  timezones = c("ET", "CET", "JST"),
  offsets = c(0, 17, 33),
  detrended = c(TRUE, FALSE),
  year_subsets = c("2009-2011", "2012-2014", "2015-2017", "2018-2020", "2009-2020")
)
start_hours <- 7
end_hours <- 7

expected <- assets_per_tz * start_hours * end_hours * map_dbl(params, length) %>% prod()
actual <- performance_df %>% nrow()

if(expected == actual) {
  save(performance_df, file = here::here("data", "performance_df.RData"))  
}
