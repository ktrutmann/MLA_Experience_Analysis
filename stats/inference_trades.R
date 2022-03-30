# TODO: (2) Jörg: Also look at all oucomes "split" by drift.
# TODO: (2) Did they short more in the treatment conditions?

# Q: What were the earnings per condition and how do they compare?
this_model <- dat_main_task %>%
	filter(i_round_in_path == max(i_round_in_path)) %>%
	select(condition, participant, distinct_path_id, payoff) %>%
  {lm(payoff ~ condition, data = .)} #nolint

this_model_clust <- coeftest(this_model, vcov = vcovCL,
	cluster = ~participant + distinct_path_id)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4] / 2
master_list$earnings_per_cond <- this_model


# Q: Earnings per condition and drift:
dat_prepared <- dat_main_task %>%
	filter(i_round_in_path == max(i_round_in_path)) %>%
	select(condition, participant, distinct_path_id, payoff, drift) %>%
	mutate(drift = ifelse(drift > .5, 'Up', 'Down'))

this_model <- lm(payoff ~ condition, data = dat_prepared)
this_model_drift <- lm(payoff ~ condition * drift, data = dat_prepared)

this_model_clust <- coeftest(this_model, vcov = vcovCL,
	cluster = ~participant + distinct_path_id)
this_model_clust_drift <- coeftest(this_model_drift, vcov = vcovCL,
	cluster = ~participant + distinct_path_id)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4] / 2
master_list$earnings_per_cond <- this_model

this_model_drift$clust_str_err <- this_model_clust_drift[, 2]
this_model_drift$p_val_clust <- this_model_clust_drift[, 4] / 2
master_list$earnings_per_cond_drift <- this_model_drift


# Q: How do the conditions influence the "investment error" at end_p2?
this_model <- dat_main_task %>%
  mutate(hold_error = abs(rational_hold_after_trade - hold_after_trade)) %>%
  filter(round_label == 'end_p2') %>%
	mutate(drift = ifelse(drift > .5, 'Up', 'Down')) %>%
  {lm(hold_error ~ condition * drift, data = .)} #nolint

this_model_clust <- coeftest(this_model, vcov = vcovCL,
	cluster = ~participant + distinct_path_id)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4] / 2
master_list$cond_on_hold_err_end_p2 <- this_model


# Analyse DE:
this_model <- de_table %>%
  {lm(de ~ condition, data = .)} #nolint

this_model_clust <- coeftest(this_model, vcov = vcovCL, cluster = ~participant)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4] / 2
master_list$de_per_cond <- this_model

# Analyse DE_last_period:
this_model <- de_table %>%
  {lm(de_last_period ~ condition, data = .)} #nolint

this_model_clust <- coeftest(this_model, vcov = vcovCL, cluster = ~participant)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4] / 2
master_list$de_last_period_per_cond <- this_model

# TODO: (4) Compare to rational DE! That should also be affected by blocked trading!


# Hit rate of the final decision by condition:
this_model <- dat_main_task %>%
	filter(round_label == 'extra_round') %>%
	mutate(hit_rate_final_round = if_else(price_up_since_last,
		hold, -hold)) %>%
	{lm(hit_rate_final_round ~ condition, data = .)} # nolint

this_model_clust <- coeftest(this_model, vcov = vcovCL,
	cluster = ~participant + distinct_path_id)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4] / 2
master_list$cond_on_final_hit <- this_model


# Drift-Hit rate of the final decision by condition:
# I.e. how often did they invest according to the drift?
this_model <- dat_main_task %>%
	filter(round_label == 'extra_round') %>%
	mutate(hit_rate_final_round = if_else(drift > .5, hold, -hold)) %>%
	group_by(drift, condition) %>%
	{lm(hit_rate_final_round ~ condition, data = .)} # nolint

this_model_clust <- coeftest(this_model, vcov = vcovCL,
	cluster = ~participant + distinct_path_id)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4] / 2
master_list$cond_on_final_drift_hit <- this_model


# "Binary" Drift-Hit rate of the final decision by condition:
# I.e. how often did they invest according to the drift?
this_model <- dat_main_task %>%
	filter(round_label == 'extra_round', hold != 0) %>%
	mutate(hit_rate_final_round = if_else((drift > .5) == (hold > 0), 1, 0)) %>%
	{glm(hit_rate_final_round ~ condition, data = ., family = 'binomial')} # nolint

this_model_clust <- coeftest(this_model, vcov = vcovCL,
	cluster = ~participant + distinct_path_id)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4] / 2
master_list$cond_on_final_binary_drift_hit <- this_model


# Q: Did they just generally invest less in the treatment conditions?
this_model <- dat_main_task %>%
	filter(round_label == 'extra_round') %>%
	mutate(abs_hold = abs(hold)) %>%
	{lm(abs_hold ~ condition, data = .)} # nolint

this_model_clust <- coeftest(this_model, vcov = vcovCL,
	cluster = ~participant + distinct_path_id)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4]
master_list$cond_on_abs_inv_amount <- this_model


# Check for Myopic loss aversion (MLA)
this_model <- dat_main_task %>%
	filter(round_label == 'end_p1') %>%
	mutate(abs_hold_after_trade = abs(hold_after_trade)) %>%
	{lm(abs_hold_after_trade ~ condition, data = .)} # nolint

this_model_clust <- coeftest(this_model, vcov = vcovCL,
	cluster = ~participant + distinct_path_id)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4] / 2
master_list$mla_inv_by_cond <- this_model

# TODO: (4) How dependent are the investments on the beliefs per condition?