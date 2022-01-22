library(tidyverse)

master_list$desc <- list()

# What positions did they update from in phase 2?
dat_main_task %>%
  filter(i_round_in_path == 0) %>%
	group_by(condition) %>%
  count(majority_updates_p2)


# What fraction of wrong updates was there per participant?
dat_main_task  %>%
  filter(condition == 'Baseline') %>%
  select(price_up_since_last, belief_diff_since_last_flipped,
    i_round_in_path, participant) %>%
  group_by(participant) %>%
  summarise(
    avg_wrong_updates = mean(belief_diff_since_last_flipped < 0, na.rm = TRUE)
  ) %>%
  print(n = 30)

# TODO: (5) Check on participant 25syxa48 in the pilot -> 0 wrong updates!
