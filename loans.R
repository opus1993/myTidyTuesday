
library(tidyverse)
library(here)
library(readxl)

# Read in and aquire names of specific files if Excel
short_file_names <- list.files(here::here("2019", "2019-11-26")) %>%
  str_remove_all(".xls|.xlsx") %>%
  str_remove_all("PCA_Report_|pca-report-") %>%
  tolower() %>%
  str_replace("default_recoveries_pca", "fy15q4")

# Get short file names
short_files <-  short_file_names %>% 
  str_detect(".r") %>%
  discard(short_file_names, .)

# all file names
all_files <- list.files(here::here("2019", "2019-11-26")) %>%
  map_chr(~ paste0("2019/2019-11-26/", .x))

# files excluding .R files
files <- all_files %>% 
  str_detect("loans.R") %>% 
  discard(all_files, .)

# clean names for input
names_clean <- files[[1]] %>%
  read_excel(skip = 4) %>%
  janitor::clean_names() %>%
  names()

# read in names, filter erroneous lines
# convert to numeric
# separate out quarter and year
all_df <- map(.x = files, .f = read_excel, skip = 4) %>%
  setNames(short_files) %>%
  map(set_names, nm = names_clean) %>%
  map(~ filter(.x, !is.na(starting))) %>%
  map(~ filter(.x, starting != "At Start of Quarter")) %>%
  map(~ mutate_at(.x,
                  .vars = vars(starting:total),
                  .funs = as.double
  )) %>%
  map2(.x = ., .y = names(.), ~ mutate(.x, quarter = .y)) %>%
  map(~ select(.x, agency_name, quarter, everything()))

# Combine all datasets
# clean up rows
clean_df <- all_df %>%
  reduce(.f = bind_rows) %>% 
  mutate(quarter = str_remove(quarter, "fy")) %>% 
  separate(col = quarter,
           into = c("year", "quarter"),
           sep = "q") %>% 
  mutate(quarter = str_remove(quarter, "x")) %>% 
  mutate(quarter = as.integer(quarter),
         year = as.integer(year)) %>%
  filter(agency_name != "Total") %>% 
  na_if(0)

# check out data
#clean_df %>% 
#  View()

# write final data
clean_df %>% 
  write_csv(here("2019", "2019-11-26", "loans.csv"))