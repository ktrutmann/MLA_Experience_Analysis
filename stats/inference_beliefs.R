library(tidyverse)
library(sandwich)
library(lmtest)

# TODO: (3) Also check updating between end p1 and end p2 for each condition!


# Q: How do the conditions influence the "belief error" at end_p2?
this_model <- dat_main_task %>%
  mutate(belief_error = abs(rational_belief - belief)) %>%
  filter(round_label == 'end_p2') %>%
  {lm(belief_error ~ condition, data = .)} #nolint

this_model_clust <- coeftest(this_model, vcov = vcovCL,
  cluster = ~participant + distinct_path_id)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4] / 2
master_list$cond_on_belief_err_end_p2 <- this_model


# Is there an effect of (and interaction between) position and favorability in
    # the second phase of the baseline condition?
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
  favorable_move_since_last, data = filter(this_dat, condition == 'Baseline'))
this_model_clust <- coeftest(this_model,
  vcov = vcovCL, cluster = ~participant)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4]
master_list$updating_p2_baseline <- this_model


# Is there an effect of (and interaction between) position and favorability in
    # the second phase of the blocked trades condition?
this_model <- lm(belief_updates_bayes_corrected ~ return_pos_end_last_round *
  favorable_move_since_last, data = filter(this_dat, condition == 'Blocked Trades'))
this_model_clust <- coeftest(this_model,
  vcov = vcovCL, cluster = ~participant)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4]
master_list$updating_p2_blocked_trades <- this_model


# Is there a three way interaction with the condition?
this_model <- lm(belief_updates_bayes_corrected ~ return_pos_end_last_round *
  favorable_move_since_last * condition, data = this_dat)
this_model_clust <- coeftest(this_model,
  vcov = vcovCL, cluster = ~participant)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4]
master_list$updating_p2_by_condition <- this_model

# TODO: (4) Also check for effects of the IMs.
