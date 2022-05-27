library(tidyverse)
library(patchwork)
library(networkD3)
library(ggbeeswarm)
library(lmtest)
library(sandwich)
# Clear up namespace collisions:
filter <- dplyr::filter

data_path <- file.path('..', 'data', 'processed')

dat_main_task <- read_delim(
  file.path(data_path, 'main_task_data.csv'), delim = ';')
dat_all_wide <- read_delim(
  file.path(data_path, 'dat_all_wide.csv'), delim = ';')
de_table <- read_delim(
  file.path(data_path, 'de_table.csv'), delim = ';')
 
# Theming:
theme_set(theme_minimal())
greyscale <- c('lightgrey', 'darkgrey', '#555555')
unibas_cols <- c('#A5D7D2', '#6AB0AA', '#4A908A')
color_set <- greyscale  # Change color set here!

# Set up the master list:
if (!'master_list' %in% ls()) master_list <- list()
if (!'plots' %in% names(master_list)) master_list$plots <- list()