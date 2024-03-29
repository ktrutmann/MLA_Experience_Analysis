# TODO: (4) Do some sanity checks split by condition! Do the numbers change?

# Save the whole wide dataset to use in the text:
master_list$df <- dat_all_wide

# Wrong updates in the initial Quiz:
summary(dat_all_wide$wrong_quiz_answers)

dat_all_wide %>%
  count(wrong_quiz_answers) %>%
  mutate(percentage = n / sum(n) * 100,
    cumulative_percentage = cumsum(percentage))

# What positions did they mainly update from in phase 2?
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
  )# %>%
  # print(n = 400) # Comment for sourcing

# Save participant comments:
master_list$desc$comments <- na.omit(dat_all_wide$participant_comments)
master_list$desc$purpose <- na.omit(dat_all_wide$purpose)
master_list$desc$strat <- na.omit(dat_all_wide$strategy)
master_list$desc$saw_pattern <- na.omit(dat_all_wide$recognised_pattern)

# Amount of investing in general:
master_list$desc$avg_inv <- dat_main_task %>%
  filter(!str_detect(round_label, 'p1'),
    condition == 'Baseline') %>%
  summarize(avg_inv = mean(abs(hold)))

# In the last round, did they short more often during a down drift?
dat_main_task %>%
  filter(round_label == 'extra_round') %>%
  with(table(drift, hold_type)) %>%
  prop.table(1) %>%
  `*`(100) %>%
  round(2)

# In the last round, did they have a lower belief during a down drift?
dat_main_task %>%
  filter(round_label == 'end_p2') %>%
  group_by(drift, condition) %>%
  summarise(avg_belief = mean(belief),
    sd_belief = sd(belief),
    avg_bayes = mean(rational_belief))

# In any investable round, did their belief match their investment?
dat_main_task %>%
  filter(investable) %>%
  group_by(hold_type_after_trade) %>%
  summarise(avg_belief = mean(belief),
    sd_belief = sd(belief))