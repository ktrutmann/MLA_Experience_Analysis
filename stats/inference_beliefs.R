library(tidyverse)
library(sandwich)
library(lmtest)


# A function to retreive the belief in the last investable period in the
# baseline as well as a specified condition
get_this_dat <- function(cond) {
    filter(dat_main_task,
           round_label == 'end_p2',
           condition %in% c('Baseline', cond)) %>%
    droplevels() %>%
    mutate(treatment = if_else(condition == 'Baseline', -.5, .5),
           drift_up = ifelse(drift < .5, -.5, .5))
}

belief_errors <- dat_main_task %>%
  mutate(belief_error = abs(rational_belief - belief)) %>%
  filter(round_label == 'end_p2') %>%
  select(participant, condition, distinct_path_id, belief_error) %>%
  pivot_wider(names_from = 'condition', values_from = 'belief_error')

# Q: Does blocking investments bring their beliefs closer to bayesian at the end of p2?
master_list$belief_err_diff_bt <-
  lm((`Blocked Trades` - Baseline) ~ 1, data = belief_errors) %>%
  coeftest(vcov = vcovCL, cluster = ~participant)

# Q: What about the delayed info condition?
master_list$belief_err_diff_di <-
lm((`Delayed Info` - Baseline) ~ 1, data = belief_errors) %>%
  coeftest(vcov = vcovCL, cluster = ~participant)

# Q: What about the blocked info condition?
master_list$belief_err_diff_bi <-
lm((`Blocked Info` - Baseline) ~ 1, data = belief_errors) %>%
  coeftest(vcov = vcovCL, cluster = ~participant)


# Is there an effect of (and interaction between) position and favorability in
    # the second phase of the baseline condition?
this_dat <- dat_main_task %>%
  mutate(return_pos_end_last_round = lag(return_type_after_trade)) %>%
  filter(condition %in% c('Baseline'),
    round_label %in% c('p2', 'end_p2'),
    return_pos_end_last_round != 'None',
    favorable_move_since_last != 'None') %>%
  mutate(return_pos_end_last_round = if_else(
    return_pos_end_last_round == 'Gain', 1, -1),
    favorable_move_since_last = if_else(
      favorable_move_since_last == 'Favorable', 1, -1))

this_model <- lm(belief_updates_bayes_corrected ~ return_pos_end_last_round *
  favorable_move_since_last,
  data = this_dat)
this_model_clust <- coeftest(this_model,
  vcov = vcovCL, type = 'HC1', cluster = ~participant)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4]
master_list$updating_p2_baseline <- this_model


# Is there an effect of (and interaction between) position and favorability in
    # the second phase of the blocked trades condition?
this_dat <- dat_main_task %>%
  mutate(return_pos_end_last_round = lag(return_type_after_trade)) %>%
  filter(condition %in% c('Blocked Trades'),
    round_label %in% c('p2', 'end_p2'),
    return_pos_end_last_round != 'None',
    favorable_move_since_last != 'None') %>%
  mutate(return_pos_end_last_round = if_else(
    return_pos_end_last_round == 'Gain', 1, -1),
    favorable_move_since_last = if_else(
      favorable_move_since_last == 'Favorable', 1, -1))

this_model <- lm(belief_updates_bayes_corrected ~ return_pos_end_last_round *
  favorable_move_since_last,
  data = this_dat)
this_model_clust <- coeftest(this_model,
  vcov = vcovCL, type = 'HC1', cluster = ~participant)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4]
master_list$updating_p2_blocked_trades <- this_model


# Is there a three way interaction with the condition?
this_dat <- dat_main_task %>%
  mutate(return_pos_end_last_round = lag(return_type_after_trade)) %>%
  filter(condition %in% c('Baseline', 'Blocked Trades'),
    round_label %in% c('p2', 'end_p2'),
    return_pos_end_last_round != 'None',
    favorable_move_since_last != 'None') %>%
  mutate(return_pos_end_last_round = if_else(
    return_pos_end_last_round == 'Gain', 1, -1),
    favorable_move_since_last = if_else(
      favorable_move_since_last == 'Favorable', 1, -1))

this_model <- lm(belief_updates_bayes_corrected ~ return_pos_end_last_round *
  favorable_move_since_last * condition,
  data = this_dat)
this_model_clust <- coeftest(this_model,
  vcov = vcovCL, type = 'HC1', cluster = ~participant)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4]
master_list$updating_p2_by_condition <- this_model
