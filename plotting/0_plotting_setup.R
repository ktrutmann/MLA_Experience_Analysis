library(tidyverse)
library(patchwork)
library(networkD3)
library(ggbeeswarm)

data_path <- file.path('..', 'data', 'processed')

dat_main_task <- read_delim(
  file.path(data_path, 'main_task_data.csv'), delim = ';')
dat_all_wide <- read_delim(
  file.path(data_path, 'dat_all_wide.csv'), delim = ';')
de_table <- read_delim(
  file.path(data_path, 'de_table.csv'), delim = ';')
 
theme_set(theme_minimal())

# Set up the master list:
if (!'master_list' %in% ls()) master_list <- list()
if (!'plots' %in% names(master_list)) master_list$plots <- list()