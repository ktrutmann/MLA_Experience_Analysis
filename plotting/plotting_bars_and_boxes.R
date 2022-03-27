# Holding at T2 by Condition ---------------------------------------------

ggplot(filter(dat_main_task, round_label == 'end_p2'),
  aes(x = ifelse(drift > .5, 'Drift Up', 'Drift Down'), y = hold_after_trade)) +
    facet_grid(cols = vars(condition)) +
    geom_jitter(width = .2, height = .1, alpha = .5) +
    geom_boxplot(aes(fill = condition), alpha = .75) +
    stat_summary(fun = mean, geom = 'point', shape = 23,
      size = 5, fill = '#dd0000', color = '#dd0000') +
    theme_minimal() +
    theme(legend.position = 'none')


# Beliefs at T2 by Condition ---------------------------------------------
ggplot(filter(dat_main_task, round_label == 'end_p2'),
  aes(x = ifelse(drift > .5, 'Drift Up', 'Drift Down'), y = belief)) +
    facet_grid(cols = vars(condition)) +
    geom_beeswarm() +
    geom_boxplot(aes(fill = condition), alpha = .65) +
    stat_summary(aes(y = rational_belief), fun = mean, color = '#00cc00',
      size = 1.5) +
    stat_summary(fun = mean, geom = 'point', shape = 23,
      size = 5, fill = '#dd0000', color = '#dd0000') +
    theme(legend.position = 'none') +
    labs(y = 'Beliefs')

# Bayes Corrected Belief updating in the second phase:
master_list$plots$updating_p2 <- dat_main_task %>%
  mutate(return_pos_end_last_round = lag(return_type_after_trade)) %>%
  filter(condition %in% c('Baseline', 'Blocked Trades'),
    round_label %in% c('p2', 'end_p2'),
    # !inverse_update,
    return_pos_end_last_round != 'None',
    favorable_move_since_last != 'None') %>%
  group_by(return_pos_end_last_round, favorable_move_since_last, condition) %>%
  summarise(avg_update = mean(belief_updates_bayes_corrected),
    se = sd(abs(belief_updates_bayes_corrected)) / sqrt(n()),
    n = n()) %>%
  ggplot(aes(return_pos_end_last_round, avg_update,
      fill = favorable_move_since_last)) +
    geom_col(position = 'dodge') +
    geom_errorbar(aes(ymin = avg_update - se, ymax = avg_update + se),
      width = .2, position = position_dodge(.9)) +
    facet_grid(cols = vars(condition)) +
    scale_fill_manual(values = c('#A5D7D2', '#6AB0AA')) +
    labs(x = 'Position and Condition',
      y = 'Mean Bayes Corrected Update',
      fill = 'Favorability')
master_list$plots$updating_p2

# "Uncorrected" Belief updating in the second phase:
dat_prepared <- dat_main_task %>%
  mutate(return_pos_end_last_round = lag(return_type_after_trade),
    hold_type_end_last_round = lag(hold_type_after_trade)) %>%
  filter(condition %in% c('Baseline', 'Blocked Trades'),
    round_label %in% c('p2', 'end_p2'),
    # !inverse_update,
    return_pos_end_last_round != 'None',
    favorable_move_since_last != 'None')

master_list$plots$updating_p2_raw <- dat_prepared %>%
  group_by(return_pos_end_last_round, favorable_move_since_last, condition) %>%
  summarise(avg_update = mean(belief_diff_since_last_flipped),
    se = sd(abs(belief_diff_since_last_flipped)) / sqrt(n()),
    n = n()) %>%
  ggplot(aes(return_pos_end_last_round, avg_update,
      fill = favorable_move_since_last)) +
    geom_col(position = 'dodge') +
    geom_errorbar(aes(ymin = avg_update - se, ymax = avg_update + se),
      width = .2, position = position_dodge(.9)) +
    facet_grid(cols = vars(condition)) +
    scale_fill_manual(values = c('#A5D7D2', '#6AB0AA')) +
    labs(x = 'Position and Condition',
      y = 'Mean Bayes Corrected Update',
      fill = 'Favorability')

# Add dots that split this View by "Holding" and "Shorting"
dat_prepared <- dat_prepared %>%
  group_by(return_pos_end_last_round, favorable_move_since_last,
    hold_type_end_last_round, condition) %>%
  summarise(avg_update = mean(belief_diff_since_last_flipped))

master_list$plots$updating_p2_raw +
  geom_point(data = dat_prepared, aes(
    x = return_pos_end_last_round,
    color = interaction(hold_type_end_last_round, favorable_move_since_last)),
  position = position_dodge(.9), cex = 3) +
  scale_color_manual(name = 'Hold Type',
    labels = c('Holding', 'Shorting'),
    breaks = c('Holding.Favorable', 'Shorting.Favorable'),
    values = c(Holding.Favorable = 'darkblue', Shorting.Favorable = 'darkred',
      Holding.Unfavorable = 'darkblue', Shorting.Unfavorable = 'darkred'))


# Belief diff end_p1 end_p2 -----------------------------------------
# Check updating from end_p1 to end_p2 and compare it to Bayes
dat_prepared <- dat_main_task %>%
  filter(round_label %in% c('end_p1', 'end_p2')) %>%
  select(participant, condition, drift, belief, rational_belief,
    distinct_path_id, round_label, majority_updates_p2) %>%
  pivot_wider(id_cols = c('participant', 'drift',
    'majority_updates_p2', 'distinct_path_id', 'condition'),
    names_from = round_label, values_from = c('belief', 'rational_belief')) %>%
  mutate(belief_diff_phase_2 = belief_end_p2 - belief_end_p1,
    rational_belief_diff_phase_2 = rational_belief_end_p2 -
      rational_belief_end_p1)

ggplot(dat_prepared, aes(x = ifelse(drift > .5, 'Drift Up', 'Drift Down'),
  y = belief_diff_phase_2)) +
    facet_grid(cols = vars(condition)) +
    geom_beeswarm() +
    geom_boxplot(aes(fill = condition), alpha = .65) +
    stat_summary(aes(y = rational_belief_diff_phase_2),
      fun = mean, color = '#00cc00', size = 1.5, shape = 3) +
    stat_summary(fun = mean, geom = 'point', shape = 23,
      size = 3, fill = '#dd0000', color = '#dd0000') +
    theme(legend.position = 'none') +
    labs(x = 'Drift and Condition', y = 'Belief Diff Start to End Phase 2')

# The same thing, but a bit more "digestable" as barplots:
ggplot(dat_prepared, aes(x = ifelse(drift > .5, 'Drift Up', 'Drift Down'),
  y = belief_diff_phase_2)) +
    facet_grid(cols = vars(condition)) +
    stat_summary(aes(fill = condition), fun = mean, geom = 'col') +
    stat_summary(aes(group = condition), fun.data = mean_se, geom = 'errorbar',
      width = .15) +
    stat_summary(aes(y = rational_belief_diff_phase_2),
      fun = mean, color = '#00cc00', size = 3, shape = 3) +
    theme(legend.position = 'none') +
    labs(x = 'Drift and Condition', y = 'Belief Diff Start to End Phase 2')

# Further divide it up by the main holding in p2.
filter(dat_prepared, str_detect(majority_updates_p2, 'Hold|Short'))  %>%
  mutate(majority_updates_p2_inv = str_extract(
    majority_updates_p2, 'Hold|Short'),
    drift = ifelse(drift > .5, 'Drift Up', 'Drift Down')) %>%
  ggplot(aes(x = majority_updates_p2_inv,
    y = belief_diff_phase_2)) +
      facet_grid(cols = vars(drift)) +
      stat_summary(aes(fill = condition),
        fun = mean, geom = 'col', position = 'dodge') +
      stat_summary(aes(group = condition), fun.data = mean_se, geom = 'errorbar',
        width = .15, position = position_dodge(.925)) +
      stat_summary(aes(y = rational_belief_diff_phase_2),
        fun = mean, color = '#00cc00', shape = 3, cex = 2) +
      labs(x = 'Drift and Hold during Phase 2',
        y = 'Belief Diff Start to End Phase 2', fill = 'Condition')


# DE Numbers ----------------------------------------------------

# DE Boxplot
ggplot(de_table, aes(x = 0, y = de)) +
  geom_boxplot(fill = "skyblue4", outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  geom_hline(yintercept = 0, color = 'darkgrey', size = 1) +
  labs(title = 'Disposition Effect', x = '') +
  theme(axis.text.x = element_blank())
	 
# calculate PLR & PGR
ggplot(de_table, aes(0, plr)) +
  geom_boxplot(fill = "skyblue4", outlier.shape = NA) +
  geom_jitter(width = 0.1, height = 0) +
  geom_hline(yintercept = 0, color = 'darkgrey', size = 1) +
  labs(title = 'Propensity to sell Losses', x = '') +
  theme(axis.text.x = element_blank())

ggplot(de_table, aes(0, pgr)) +
  geom_boxplot(fill = "skyblue4", outlier.shape = NA) +
  geom_jitter(width = 0.1, height = 0) +
  geom_hline(yintercept = 0, color = 'darkgrey', size = 1) +
  labs(title = 'Propensity to sell Gains', x = '') +
  theme(axis.text.x = element_blank())

# WC Style DE:
  ggplot(de_table, aes(x = 0, y = de_last_period)) +
  geom_boxplot(fill = "skyblue4", outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  geom_hline(yintercept = 0, color = 'darkgrey', size = 1) +
  labs(title = 'Disposition Effect', x = '') +
  theme(axis.text.x = element_blank())


# Time Spent on Exp ----------------------------------------------------
ggplot(dat_all_wide,
	aes(x = participant, y = experiment_time_in_minutes, group = 1)) +
	geom_bar(fill = 'skyblue4', stat = 'identity') +
	geom_hline(yintercept = mean(dat_all_wide$experiment_time_in_minutes),
		color = 'darkred', size = 1) +
	labs(title = 'Time spent on Experiment', x = 'Participant IDs', y = 'Minutes')


# Payoffs ----------------------------------------------------
ggplot(dat_all_wide,
	aes(x = participant, y = payoff, group = 1)) +
	geom_bar(fill = 'skyblue4', stat = 'identity') +
	geom_hline(yintercept = mean(dat_all_wide$payoff),
		color = 'darkred', size = 1) +
	labs(title = 'Payoff in Points', x = 'Participant IDs', y = 'Points')
