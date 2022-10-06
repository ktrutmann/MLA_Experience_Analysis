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
	filter(round_label == 'extra_round') %>%
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


# Analyse DE: -------------------------------------------------
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


# Compare to rational DE. That should also be affected by blocked trading!
this_dat <- de_table %>%
	select(participant, condition, de, rational_de, de_last_period,
		rational_de_last_period) %>%
	mutate(de_diff_to_rational = de - rational_de,
		 de_last_period_diff_to_rational = de_last_period - rational_de_last_period)

# "Corrected" DE during p2:
this_model <- this_dat %>%
  {lm(de_diff_to_rational ~ condition, data = .)} #nolint

this_model_clust <- coeftest(this_model, vcov = vcovCL, cluster = ~participant)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4] / 2
master_list$de_vs_rational <- this_model
# The DE gets smaller, even when controlling for the rational de!

# "Corrected" DE final round:
this_model <- this_dat %>%
	na.omit() %>%
  {lm(de_last_period_diff_to_rational ~ condition, data = .)} #nolint

this_model_clust <- coeftest(this_model, vcov = vcovCL, cluster = ~participant)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4] / 2
master_list$de_vs_rational_last_period <- this_model
# No effect for the final-round DE measure.


# Hit rates: ----------------------------------------------------------
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
this_dat <- dat_main_task %>%
	filter(round_label == 'extra_round') %>%
	mutate(hit_rate_final_round = if_else(drift > .5, hold, -hold)) %>%
  left_join(select(dat_all_wide, c('participant', 'wrong_quiz_answers')))

this_model <-	lm(hit_rate_final_round ~ condition, data = this_dat)
this_model_clust <- coeftest(this_model, vcov = vcovCL,
	cluster = ~participant + distinct_path_id)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4] / 2
this_model$data <- this_dat
master_list$cond_on_final_drift_hit <- this_model


# "Binary" Drift-Hit rate of the final decision by condition:
# I.e. how often did they invest according to the drift?
this_model <- dat_main_task %>%
	filter(round_label == 'extra_round', hold != 0) %>%
	mutate(hit_rate_final_round = if_else((drift > .5) == (hold > 0), 1, 0)) %>%
  left_join(select(dat_all_wide, c('participant', 'wrong_quiz_answers'))) %>%
	{glm(hit_rate_final_round ~ condition, data = ., family = 'binomial')} # nolint

this_model_clust <- coeftest(this_model, vcov = vcovCL,
	cluster = ~participant + distinct_path_id)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
# Correct p-value since we had a directed hypothesis:
this_model$p_val_clust <- if_else(this_model$coefficients < 0,
	1 - (this_model_clust[, 4] / 2), this_model_clust[, 4] / 2)
names(this_model$p_val_clust) <- names(this_model_clust[, 4])
master_list$cond_on_final_binary_drift_hit <- this_model


# Same, but with the drift as a predictor:
this_model <- dat_main_task %>%
	filter(round_label == 'extra_round') %>%
	mutate(hit_rate_final_round = if_else(drift > .5, hold, -hold),
		drift = ifelse(drift > .5, 'Up', 'Down')) %>%
	{lm(hit_rate_final_round ~ condition * drift, data = .)} # nolint

this_model_clust <- coeftest(this_model, vcov = vcovCL,
	cluster = ~participant + distinct_path_id)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4] / 2
this_model$data <- filter(dat_main_task, round_label == 'extra_round')
master_list$cond_drift_on_final_drift_hit <- this_model


# Binarized drift hit rate including drift as a predictor:
this_model <- dat_main_task %>%
	filter(round_label == 'extra_round', hold != 0) %>%
	mutate(hit_rate_final_round = if_else((drift > .5) == (hold > 0), 1, 0),
		drift = ifelse(drift > .5, 'Up', 'Down')) %>%
	{glm(hit_rate_final_round ~ condition * drift, data = ., family = 'binomial')} # nolint

this_model_clust <- coeftest(this_model, vcov = vcovCL,
	cluster = ~participant + distinct_path_id)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4] / 2
master_list$cond_drift_final_binary_drift_hit <- this_model  # nolint


# Same, but with the portfolio type as a predictor:
this_model <- dat_main_task %>%
	filter(round_label == 'extra_round') %>%
	mutate(hit_rate_final_round = if_else(drift > .5, hold, -hold),
    portf_type = str_extract(majority_updates_p2, 'Shorting|Holding')) %>%
	{lm(hit_rate_final_round ~ condition * portf_type, data = .)} # nolint

this_model_clust <- coeftest(this_model, vcov = vcovCL,
	cluster = ~participant + distinct_path_id)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4] / 2
master_list$cond_portf_on_final_drift_hit <- this_model


# Binarized drift hit rate including drift as a predictor:
this_model <- dat_main_task %>%
	filter(round_label == 'extra_round', hold != 0) %>%
	mutate(hit_rate_final_round = if_else((drift > .5) == (hold > 0), 1, 0),
    portf_type = str_extract(majority_updates_p2, 'Shorting|Holding')) %>%
	{glm(hit_rate_final_round ~ condition * portf_type, data = ., family = 'binomial')} # nolint

this_model_clust <- coeftest(this_model, vcov = vcovCL,
	cluster = ~participant + distinct_path_id)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4] / 2
master_list$cond_portf_final_binary_drift_hit <- this_model  # nolint


# Other Analyses: --------------------------------------------------
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


# Did they short more at the end of treatment conditions?
dat_main_task %>%
  filter(round_label == 'end_p2',
		hold_type_after_trade != 'None') %>%
	mutate(drift = ifelse(drift > .5, 'Up', 'Down'),
		hold = ifelse(hold_type_after_trade == 'Holding', 1, -1)) %>%
  {lm(hold ~ condition, data = .)} %>% #nolint
  summary()
 # Apparently they do not. Makes sense since they're _less_ over-convinced.


# Sanity check: Investment by condition.
lm(hold_after_trade ~ drift,
	data = filter(dat_main_task, round_label == 'end_p2')) %>%
	summary()


# Influence of beliefs on trades: ---------------------------------------
rerun_olog <- FALSE  # Set to true if the regression should be rerun
if (rerun_olog) {
	dat_prepared <- filter(dat_main_task,
		investable,
		round_label != 'end_p1') %>%
		mutate(hold_after_trade = factor(
			hold_after_trade, levels = -4:4, ordered = TRUE))

	olog_beliefs <- clmm(hold_after_trade ~ belief * drift + (1 | participant),
		data = dat_prepared)

	saveRDS(olog_beliefs, file.path('output', 'ordered_log_reg_beliefs_on_trades.RDS'))
} else {
	olog_beliefs <- readRDS(file.path('output', 'ordered_log_reg_beliefs_on_trades.RDS'))
}
master_list$logreg_beliefs <- coeftest(olog_beliefs)
master_list$logreg_beliefs