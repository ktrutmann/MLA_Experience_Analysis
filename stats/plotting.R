library(tidyverse)

theme_set(theme_minimal())

# TODO: (2) Implement the other conditions!

# Belief updating in the second phase of Baseline paths:
# FIXME: (4) This does not yet consider inverse updates!
dat_main_task %>%
  mutate(return_pos_end_last_round = lag(return_type_after_trade)) %>%
  filter(condition == 'Baseline',
    round_label %in% c('p2', 'end_p2'),
    return_pos_end_last_round != 'None',
    favorable_move_since_last != 'None') %>%
  group_by(return_pos_end_last_round, favorable_move_since_last) %>%
  summarise(avg_update = mean(abs(belief_diff_since_last)),
    se = sd(abs(belief_diff_since_last)) / sqrt(n()),
    n = n()) %>%
  ggplot(aes(return_pos_end_last_round, avg_update,
      fill = favorable_move_since_last)) +
    geom_col(position = 'dodge') +
    geom_errorbar(aes(ymin = avg_update - se, ymax = avg_update + se),
      width = .2, position = position_dodge(.9)) +
    scale_fill_brewer(palette = 'Paired')


# Comparison between participants and Bayes diff_end_p1_end_p2
# For now only in the baseline:
dat_main_task %>%
  filter(round_label == 'end_p2',
    condition == 'Baseline') %>%
  full_join({
    dat_main_task %>%
    group_by(participant, condition, distinct_path_id) %>%
    summarise(belief_diff_phases = belief[round_label == 'end_p2'] -
        belief[round_label == 'end_p1'],
      rational_belief_diff_phases = (rational_belief[round_label == 'end_p2'] -
        rational_belief[round_label == 'end_p1']) * 100) %>%
    ungroup()}) %>%
  select(return_type, rational_belief_diff_phases,
    belief_diff_phases, hold_type) %>%
  filter(return_type != 'None') %>%
  mutate(rational_belief_diff_phases = if_else(hold_type == 'Shorting',
      - rational_belief_diff_phases, rational_belief_diff_phases),
    belief_diff_phases = if_else(hold_type == 'Shorting',
      - belief_diff_phases, belief_diff_phases)) %>%
  group_by(return_type) %>%
  summarise(mean_belief_diff = mean(belief_diff_phases),
    mean_rational_belief_diff = mean(rational_belief_diff_phases),
    se = sd(belief_diff_phases) / sqrt(n()),
    n = n()) %>%
  ggplot(aes(return_type, mean_belief_diff)) +
    geom_col(position = 'dodge', fill = 'lightblue') +
    geom_point(aes(y = mean_rational_belief_diff)) +
    geom_errorbar(aes(ymin = mean_belief_diff - se, ymax = mean_belief_diff + se),
      width = .2, position = position_dodge(.9)) +
    scale_fill_brewer(palette = 'Paired')
