library(tidyverse)
library(sandwich) # for clustered errors
library(lmtest) # for clustered errors
library(ordinal) # for ordered logistic regression
# Clear up namespace collisions:
filter <- dplyr::filter

data_path <- file.path('..', 'data', 'processed')

dat_main_task <- read_delim(
  file.path(data_path, 'main_task_data.csv'), delim = ';')
dat_all_wide <- read_delim(
  file.path(data_path, 'dat_all_wide.csv'), delim = ';')
de_table <- read_delim(
  file.path(data_path, 'de_table.csv'), delim = ';')

if (!'master_list' %in% ls()) master_list <- list()
if (!'desc' %in% names(master_list)) master_list$desc <- list()