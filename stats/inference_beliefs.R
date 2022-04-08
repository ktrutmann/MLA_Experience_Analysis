# Absolute Beliefs ------------------------------------
# Sanity Check: Are beliefs the same per cond at end_p1? -> They are not!
# However, the difference is only 1.9 and .9 percentage points!
this_model <- dat_main_task %>%
  filter(round_label == 'end_p1') %>%
  {lm(belief ~ condition, data = .)} # nolint

this_model_clust <- coeftest(this_model, vcov = vcovCL,
  cluster = ~participant + distinct_path_id)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4]
master_list$cond_on_belief_end_p1 <- this_model


# Are the bliefs significantly different at end_p2?
this_model <- dat_main_task %>%
  filter(round_label == 'end_p2') %>%
  mutate(drift = ifelse(drift > .5, 'Up', 'Down')) %>%
  {lm(belief ~ condition * drift, data = .)} # nolint

this_model_clust <- coeftest(this_model, vcov = vcovCL,
  cluster = ~participant + distinct_path_id)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4]
master_list$cond_on_belief_end_p2 <- this_model
# They are generally more optimistic in the treatment conditions


# Belief Error ------------------------------------

# Q: How do the conditions influence the "belief error" at end_p2?
dat_prepared <- dat_main_task %>%
  mutate(belief_error = abs(rational_belief - belief)) %>%
  mutate(drift = ifelse(drift > .5, 'Up', 'Down')) %>%
  filter(round_label == 'end_p2')

this_model <- lm(belief_error ~ condition, data = dat_prepared)
this_model_drift <- lm(belief_error ~ condition * drift, data = dat_prepared)

this_model_clust <- coeftest(this_model, vcov = vcovCL,
  cluster = ~participant + distinct_path_id)
this_model_clust_drift <- coeftest(this_model_drift, vcov = vcovCL,
  cluster = ~participant + distinct_path_id)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4] / 2
master_list$cond_on_belief_err_end_p2 <- this_model

this_model_drift$clust_str_err <- this_model_clust_drift[, 2]
this_model_drift$p_val_clust <- this_model_clust_drift[, 4] / 2
master_list$cond_drift_belief_err_end_p2 <- this_model_drift


# Belief Updating ------------------------------------

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


# Does the amount they invest (absolute or not) influence belief updating?
lm(belief_diff_since_last ~ abs(hold), data = dat_main_task) %>%
  summary()
# If anything it reduces the belief updating...
# But this could also be because they invest more when they are already very sure!

# TODO: (4) Also check for effects of the IMs.


# Belief Updating end_p1 to end_p2 ------------------------------------
this_model <- dat_main_task %>%
  filter(round_label %in% c('end_p1', 'end_p2')) %>%
    pivot_wider(id_cols = c('participant', 'distinct_path_id', 'condition',
      'drift'), values_from = 'belief', names_from = 'round_label',
      names_prefix = 'belief_') %>%
  mutate(drift = as.factor(if_else(drift == .65, 'Up', 'Down')),
    belief_diff = belief_end_p2 - belief_end_p1) %>%
  {lm(belief_diff ~ condition * drift, data = .)}

this_model_clust <- coeftest(this_model,
  vcov = vcovCL, cluster = ~participant + distinct_path_id)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4]
master_list$updating_p1_to_p2_by_condition <- this_model
