data_path <- file.path('..', 'data', 'processed')
data_file_name_long <- 'main_task_data.csv'
data_file_name_wide <- 'dat_all_wide.csv'

dat_main_task <- read_delim(
  file.path(data_path, data_file_name_long), delim = ';')
dat_all_wide <- read_delim(
  file.path(data_path, data_file_name_wide), delim = ';')
 
theme_set(theme_minimal())

# Set up the master list:
if (!'master_list' %in% ls()) master_list <- list()
if (!'plots' %in% names(master_list)) master_list$plots <- list()