library(tidyverse)
library(lmerTest)
library(sandwich)

# TODO: (1) Try out car::contr.Sum(levels=derp). Tweet tip by Jana!

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
lmer(formula = hold ~ condition * drift_direction + (1 | participant),
	 data = get_this_dat('Blocked Trades'),
	 contrasts = list(condition = c(-.5, .5),
	 				  drift_direction = c(-.5, .5))) %>%
	 summary()


# Q: What about the blocked info treatment?
lmer(formula = hold ~ drift_direction * condition + (1 | participant),
	 data = get_this_dat('Blocked Info')) %>%
	 summary()

# Q: What were the earnings per condition and how do they compare?
this_dat <- dat_main_task %>%
	filter(i_round_in_path == max(i_round_in_path)) %>%
	select(condition, participant, distinct_path_id, payoff) %>%
	pivot_wider(names_from = 'condition', values_from = 'payoff')

master_list$payoff_diff_bt <- lm((`Blocked Trades` - Baseline) ~ 1, data = this_dat) %>%
	coeftest(vcov = vcovCL, cluster = ~participant)
master_list$payoff_diff_di <- lm((`Delayed Info` - Baseline) ~ 1, data = this_dat) %>%
	coeftest(vcov = vcovCL, cluster = ~participant)
master_list$payoff_diff_bi <- lm((`Blocked Info` - Baseline) ~ 1, data = this_dat) %>%
	coeftest(vcov = vcovCL, cluster = ~participant)


# Q: How do the conditions influence the "investment error" at end_p2?
this_model <- dat_main_task %>%
  mutate(hold_error = abs(rational_hold_after_trade - hold_after_trade)) %>%
  filter(round_label == 'end_p2') %>%
  {lm(hold_error ~ condition, data = .)} #nolint

this_model_clust <- coeftest(this_model, vcov = vcovCL, cluster = ~participant)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4]
master_list$cond_on_hold_err_end_p2 <- this_model