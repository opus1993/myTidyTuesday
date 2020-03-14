# LEARNING LAB 30 ----
# Shiny + Tidyquant 
# Excel VLOOKUP ----

# 1.0 LIBRARIES ----
library(tidyverse)
library(tidyquant)

source("scripts/plot_series.R")

# 2.0 VLOOKUP ----

sp500_lookup_tbl <- tq_index("SP500")

# 2.1 Simple Vlookups ----

"AAPL" %>% VLOOKUP(sp500_lookup_tbl, symbol, company)

"AAPL" %>% VLOOKUP(sp500_lookup_tbl, symbol, sector)

"AAPL" %>% VLOOKUP(sp500_lookup_tbl, symbol, weight)

# 2.2 Charting with VLOOKUPs ----

"AAPL" %>%
    tq_get(from = "2020-01-01") %>%
    mutate(company = symbol %>% VLOOKUP(sp500_lookup_tbl, symbol, company)) %>%
    plot_series(date, adjusted, company)

c("AAPL", "GOOG") %>%
    tq_get(from = "2020-01-01") %>%
    mutate(company = symbol %>% VLOOKUP(sp500_lookup_tbl, symbol, company)) %>%
    plot_series(date, adjusted, company, scales = "free_y")
