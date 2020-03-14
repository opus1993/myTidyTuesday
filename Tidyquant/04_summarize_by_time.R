# LEARNING LAB 30 ----
# Shiny + Tidyquant 
# Summarize by Time ----

# 1.0 LIBRARIES ----
library(tidyverse)
library(tidyquant)

source("scripts/plot_series.R")

# 2.0 GET DATA ----

sp500_lookup_tbl <- tq_index("SP500")

stock_data_tbl <- c("AAPL", "GOOG", "NFLX", "FB") %>%
    tq_get(from = "2020-01-01") %>%
    mutate(company = symbol %>% VLOOKUP(sp500_lookup_tbl, symbol, company)) 

stock_data_tbl %>%
    plot_series(date, adjusted, company, scales = "free_y")

# 3.0 SUMMARIZE BY TIME ----

stock_data_tbl %>%
    group_by(company) %>%
    summarize_by_time(
        .date_var = date, 
        .by       = "month",
        total_volume = MEDIAN(volume)
    )
