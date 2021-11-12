library(tidyverse)

# What positions did they update from in phase 2?
dat_main_task %>%
  filter(i_round_in_path == 0) %>%
	group_by(condition) %>%
  count(majority_updates_p2)
