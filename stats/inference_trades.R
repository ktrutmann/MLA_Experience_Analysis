library(tidyverse)
library(lmtest)
library(sandwich)

# A function to retreive the last investment in the baseline as well as a
# specified condition
get_this_dat <- function(cond) {
	filter(dat_main_task,
		   round_label == 'extra_round',
		   condition %in% c('Baseline', cond)) %>%
	droplevels() %>%
	mutate(drift_direction = as.factor(ifelse(drift < .5, 'Down', 'Up')))
}


# Q: Did they invest more in line with the drift in the last decision
# of the blocked condition than in the basline full controll?
# lmer(formula = hold ~ condition * drift_direction + (1 | participant),
# 	 data = get_this_dat('Blocked Trades'),
# 	 contrasts = list(condition = c(-.5, .5),
# 	 				  drift_direction = c(-.5, .5))) %>%
# 	 summary()


# # Q: What about the blocked info treatment?
# lmer(formula = hold ~ drift_direction * condition + (1 | participant),
# 	 data = get_this_dat('Blocked Info')) %>%
# 	 summary()

# Q: What were the earnings per condition and how do they compare?
this_model <- dat_main_task %>%
	filter(i_round_in_path == max(i_round_in_path)) %>%
	select(condition, participant, distinct_path_id, payoff) %>%
  {lm(payoff ~ condition, data = .)} #nolint

this_model_clust <- coeftest(this_model, vcov = vcovCL,
	cluster = ~participant + distinct_path_id)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4]
master_list$earnings_per_cond <- this_model


# Q: How do the conditions influence the "investment error" at end_p2?
this_model <- dat_main_task %>%
  mutate(hold_error = abs(rational_hold_after_trade - hold_after_trade)) %>%
  filter(round_label == 'end_p2') %>%
  {lm(hold_error ~ condition, data = .)} #nolint

this_model_clust <- coeftest(this_model, vcov = vcovCL,
	cluster = ~participant + distinct_path_id)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4]
master_list$cond_on_hold_err_end_p2 <- this_model


# Analyse DE:
this_model <- de_table %>%
  {lm(de ~ condition, data = .)} #nolint

this_model_clust <- coeftest(this_model, vcov = vcovCL, cluster = ~participant)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4]
master_list$de_per_cond <- this_model


# Hit rate of the final decision by condition:
this_model <- dat_main_task %>%
	filter(round_label == 'extra_round') %>%
	mutate(hit_rate_final_round = if_else(price_up_since_last,
		hold, -hold)) %>%
	{lm(hit_rate_final_round ~ condition, data = .)}

this_model_clust <- coeftest(this_model, vcov = vcovCL,
	cluster = ~participant + distinct_path_id)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4]
master_list$cond_on_final_hit_rate <- this_model


# Check for Myopic loss aversion (MLA)
this_model <- dat_main_task %>%
	filter(round_label == 'end_p1') %>%
	mutate(abs_hold_after_trade = abs(hold_after_trade)) %>%
	{lm(abs_hold_after_trade ~ condition, data = .)}

this_model_clust <- coeftest(this_model, vcov = vcovCL,
	cluster = ~participant + distinct_path_id)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4]
master_list$mla_inv_by_cond <- this_model
