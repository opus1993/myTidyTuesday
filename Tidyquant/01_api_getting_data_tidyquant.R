# LEARNING LAB 30 ---- 
# Shiny + Tidyquant     
# Getting Data ----

# 1.0 LIBRARIES ----
library(tidyverse)
library(tidyquant)
library(config)

source("./Tidyquant/scripts/plot_series.R")

# 2.0 STOCK LISTS - INDEX & EXCHANGES

# 2.1 Stock Index ----

# SP500
tq_index_options()
tq_index("SP600") %>% filter(str_detect(symbol, "CN"))

# NYSE
tq_exchange_options()
tq_exchange("NYSE")

# 3.0 API: tq_get() ----

?tq_get()

tq_get_options()

# 2.1 YAHOO FINANCE ----

tq_get("CNHI", from = "2015-01-01", to = "2020-01-01")

tq_get("CNHI", get = "stock.prices", from = "2015-01-01") %>%
    plot_series(date, adjusted, symbol)

# 2.2 FRED ECONOMIC DATA ----
c("UNRATE", "FEDFUNDS") %>%
    tq_get(get = "economic.data", from = "2005-01-01") %>%
    rename(value = price) %>%
    
    # ggplot
    plot_series(date, value, symbol)
    

# 2.3 QUANDL ----
# quandl_api_key("<your-api-key>")
config::get("quandl", file = "../config.yml") %>% quandl_api_key()

tq_get("EIA/PET_MTTIMUS1_M", get = "quandl", from = "2010-01-01")

tq_get("EIA/PET_MTTIMUS1_M", get = "quandl", from = "2010-01-01") %>%
    plot_series(date, value, symbol)

# 2.4 TIINGO (NEW) ----
# tiingo_api_key("<your-api-key>")     # has intra-day pricing
config::get("tiingo", file = "../config.yml") %>% tiingo_api_key()

tq_get(c("AAPL", "GOOG"), get = "tiingo", from = "2010-01-01") %>%
    plot_series(date, adjusted, symbol)

# 2.5 ALPHA VANTAGE ----
# av_api_key("<your-api-key>")         # has intra-day pricing
config::get("alphavantage", file = "../config.yml") %>% av_api_key()

c("AAPL", "GOOG") %>%
    tq_get(
        get        = "alphavantage",
        av_fun     = "TIME_SERIES_INTRADAY",
        interval   = "15min",
        outputsize = "full"
    ) %>% 
    plot_series(timestamp, close, symbol, scales = "free_y")
