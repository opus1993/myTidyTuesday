# LEARNING LAB 30 ----
# Shiny + Tidyquant 
# Pivot Table ----

# 1.0 LIBRARIES ----
library(tidyverse)
library(tidyquant)
library(tictoc)

source("scripts/plot_series.R")

# 2.0 GET DATA ----

sp500_lookup_tbl <- tq_index("SP500")

stock_data_tbl <- c("AAPL", "GOOG", "NFLX", "FB") %>%
    tq_get(from = "2020-01-01") %>%
    mutate(company = symbol %>% VLOOKUP(sp500_lookup_tbl, symbol, company)) 

stock_data_tbl %>%
    plot_series(date, adjusted, company, scales = "free_y")

# 3.0 PIVOTING DATA ----

stock_data_tbl 

# 3.1 Simple Pivots ----

# No Rows/Columns
stock_data_tbl %>%
    pivot_table(
        .values  = ~ MEDIAN(volume)
    )

# Row-Groups 
# - Note: "Tidy" Format
stock_data_tbl %>%
    pivot_table(
        .rows    = company,
        .values  = ~ MEDIAN(volume)
    )

# Column-Groups
# - Note: "Human" Format
stock_data_tbl %>%
    pivot_table(
        .columns = company,
        .values  = ~ MEDIAN(volume)
    )

# 3.2 Stacking Pivots ----

# Row (Tidy/Long) Format
stock_data_tbl %>%
    pivot_table(
        .rows   = c(symbol, ~ YEAR(date), ~ MONTH(date)),
        .values = ~ PCT_CHANGE_FIRSTLAST(adjusted)
    )

# Column (Human) Format
stock_data_tbl %>%
    pivot_table(
        .rows    = c(~ YEAR(date), ~ MONTH(date)),
        .columns = symbol,
        .values  = ~ PCT_CHANGE_FIRSTLAST(adjusted)
    )

# Column (Human) Format
stock_data_tbl %>%
    pivot_table(
        .rows    = symbol,
        .columns = c(~ YEAR(date), ~ MONTH(date)),
        .values  = ~ PCT_CHANGE_FIRSTLAST(adjusted)
    )

# 4.0 PERFORMANCE ----

# #NOTRUN - Takes 2min to download 1.2M ROWS of historical prices for 500+ stocks
# sp500_index <- tq_index("SP500")
# sp500_historical_prices <- sp500_index %>% tq_get()

sp500_historical_prices <- read_rds("data/sp500_historical_prices_tbl.rds")

tic()
sp500_historical_prices %>%
    pivot_table(
        .rows    = c(~ YEAR(date), ~ QUARTER(date)),
        .columns = symbol,
        .values  = ~ PCT_CHANGE_FIRSTLAST(adjusted)
    )
toc()
