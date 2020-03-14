# LEARNING LAB 30 ----
# Shiny + Tidyquant 
# 100+ Excel Functions ----

# 1.0 LIBRARIES ----
library(tidyverse)
library(tidyquant)


# 2.0 SUMMARIZATION FUNCTIONS ----
?SUM

1:10 %>% SUM()
1:10 %>% FIRST()
c(100, 300, 500) %>% CHANGE_FIRSTLAST()
c(100, 300, 500) %>% PCT_CHANGE_FIRSTLAST()

FANG %>%
    pivot_table(
        .rows    = ~ YEAR(date),
        .columns = symbol, 
        .values  = ~ SUM(volume)
    )

# 3.0 SUM-IFS FUNCTIONS ----
?SUM_IFS
1:10 %>% SUM_IFS(x > 5)

FANG %>%
    pivot_table(
        .rows    = ~ YEAR(date),
        .columns = symbol, 
        .values  = ~ SUM_IFS(volume, volume > 10e6, YEAR(date) > 2015)
    )

# Create your own IFS functions
Q75_IFS <- CREATE_IFS(quantile, na.rm = TRUE, probs = 0.75)

1:10 %>% Q75_IFS(x > 5)

# 4.0 MUTATION FUNCTIONS ----

?ABS

c(100, 300, 500) %>% CHANGE()
c(100, 300, 500) %>% PCT_CHANGE()
c(100, 300, 500) %>% CUMULATIVE_SUM()

FANG %>%
    group_by(symbol) %>%
    mutate(returns = PCT_CHANGE(adjusted))

# 5.0 DATE & DATE-TIME ----

?AS_DATE

HOLIDAY_SEQUENCE(start_date = "2020-01-01", end_date = "2020-12-31", calendar = "NYSE")

NET_WORKDAYS(start_date = "2020-01-01", end_date = "2020-12-31")

NET_WORKDAYS(start_date = TODAY(), end_date = "2020-12-31", 
             holidays = HOLIDAY_SEQUENCE(start_date = TODAY(), end_date = "2020-12-31"))

YEAR("2020-01-01")

MONTH("2020-01-01", label = TRUE)

FANG %>%
    pivot_table(
        .rows    = c(~YEAR(date), ~MONTH(date)),
        .columns = symbol,
        .values  = ~ PCT_CHANGE_FIRSTLAST(adjusted)
    )

# 6.0 FINANCIAL & INVESTMENT CALC'S ----

?NPV

c(500, 400, 300, 150) %>% NPV(rate = 0.05)

c(-1000, 500, 400, 300, 150) %>% NPV(rate = 0.05)

c(-1000, 500, 400, 300, 150) %>% IRR()
