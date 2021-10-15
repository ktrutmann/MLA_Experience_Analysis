library(tidyverse)
library(patchwork)
library(networkD3)

theme_set(theme_minimal())

# Belief updating in the second phase of Baseline paths:
# TODO: (4) FIXME: This does not yet consider inverse updates!
dat_main_task %>%
  mutate(return_pos_end_last_round = lag(return_type_after_trade)) %>%
  filter(condition != 'Blocked Info',
    round_label %in% c('p2', 'end_p2'),
    return_pos_end_last_round != 'None',
    favorable_move_since_last != 'None') %>%
  group_by(return_pos_end_last_round, favorable_move_since_last, condition) %>%
  summarise(avg_update = mean(abs(belief_diff_since_last)),
    se = sd(abs(belief_diff_since_last)) / sqrt(n()),
    n = n()) %>%
  ggplot(aes(return_pos_end_last_round, avg_update,
      fill = favorable_move_since_last)) +
    geom_col(position = 'dodge') +
    geom_errorbar(aes(ymin = avg_update - se, ymax = avg_update + se),
      width = .2, position = position_dodge(.9)) +
    facet_grid(cols = vars(condition)) +
    scale_fill_brewer(palette = 'Paired') +
    ylim(c(0, 15))


# Comparison between participants and Bayes diff_end_p1_end_p2
dat_main_task %>%
  filter(round_label == 'end_p2',
    condition != 'Blocked Info') %>%
  full_join({
    dat_main_task %>%
    filter(condition != 'Blocked Info') %>%
    group_by(participant, condition, distinct_path_id) %>%
    summarise(belief_diff_phases = belief[round_label == 'end_p2'] -
        belief[round_label == 'end_p1'],
      rational_belief_diff_phases = (rational_belief[round_label == 'end_p2'] -
        rational_belief[round_label == 'end_p1']) * 100) %>%
    ungroup()}) %>%
  select(return_type_after_trade, rational_belief_diff_phases,
    belief_diff_phases, majority_updates_p2, condition) %>%
  filter(majority_updates_p2 == 'Holding Gain') %>%
  group_by(condition) %>%
  summarise(mean_belief_diff = mean(belief_diff_phases),
    mean_rational_belief_diff = mean(rational_belief_diff_phases),
    se = sd(belief_diff_phases) / sqrt(n()),
    n = n()) %>%
  ggplot(aes(condition, mean_belief_diff)) +
    geom_col(position = 'dodge') +
    geom_point(aes(y = mean_rational_belief_diff), position = position_dodge(.9),
      shape = 5, size = 5, show.legend = FALSE) +
    geom_errorbar(aes(ymin = mean_belief_diff - se, ymax = mean_belief_diff + se),
      width = .2, position = position_dodge(.9)) +
    scale_fill_brewer(palette = 'Paired')


# Individual Plots of price and belief:
p1 <- dat_main_task %>%
  filter(participant == unique(participant)[5],
    distinct_path_id == i,
    condition != 'Blocked Info') %>%
  ggplot(aes(i_round_in_path, price)) +
    geom_hline(yintercept = 1000, alpha = .5) +
    geom_vline(xintercept = 4, color = 'red', alpha = .5) +
    geom_vline(xintercept = 8, color = 'red', alpha = .5) +
    geom_line(position = position_dodge(.1), size = 1) +
    geom_point(aes(shape = hold_type_after_trade, color = condition),
      size = 5, position = position_dodge(.1)) +
    scale_color_brewer(palette = 'Paired') +
    scale_x_continuous(breaks = c(0, 4, 8)) +
    scale_shape_manual(values = c(2, 1, 6)) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank())

p2 <- dat_main_task %>%
  filter(participant == unique(participant)[5],
    distinct_path_id == i,
    condition != 'Blocked Info') %>%
  mutate(belief = belief / 100) %>%
  ggplot(aes(i_round_in_path, belief, color = condition)) +
    geom_hline(yintercept = .5, alpha = .5) +
    geom_vline(xintercept = 4, color = 'red', alpha = .5) +
    geom_vline(xintercept = 8, color = 'red', alpha = .5) +
    geom_line(size = 1, show.legend = FALSE) +
    geom_point(aes(shape = return_type_after_trade),
      size = 5, position = position_dodge(.1)) +
    scale_color_brewer(palette = 'Paired') +
    scale_x_continuous(breaks = c(0, 4, 8)) +
    scale_shape_manual(values = c(2, 6, 1)) +
    ylim(c(0, 1))

p1 +
p2 +
plot_layout(ncol = 1)


# Movement of the majority_updated_in_p2:
plot_prep <- dat_main_task %>%
  filter(i_round_in_path == 0, condition != 'Blocked Info') %>%
  mutate(condition = recode(condition, `Blocked Trades` = 'Blocked_Trades')) %>%
  select(participant, majority_updates_p2, condition, distinct_path_id) %>%
  pivot_wider(id_cols = c('participant', 'distinct_path_id'),
    names_from = condition, values_from = majority_updates_p2) %>%
  mutate(update_pos_movement = str_c(Baseline, Blocked_Trades, sep = ' / ')) %>%
  count(update_pos_movement) %>%
  mutate(source = str_c(unlist(lapply(str_split(update_pos_movement, pattern = ' / '), function(x) x[1])), ' BL'),
    target = str_c(unlist(lapply(str_split(update_pos_movement, pattern = ' / '), function(x) x[2])), ' BT'),
    id_source = match(source, unique(c(source, target))) - 1,
    id_target = match(target, unique(c(source, target))) - 1) %>%
  as.data.frame()

plot_prep2 <- data.frame(name = unique(c(plot_prep$source, plot_prep$target)))

sankeyNetwork(Links = plot_prep, Nodes = plot_prep2,
  Source = 'id_source', Target = 'id_target',
  Value = 'n', NodeID = 'name',
  fontSize = 14)
