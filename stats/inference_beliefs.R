library(tidyverse)
# library(lme4)
# library(lmerTest)
# library(sandwich)

# The list where all objects for the paper will be stored
r_obj_list <- list()

# A function to retreive the belief in the last investable period in the
# baseline as well as a specified condition
get_this_dat <- function(cond) {
	filter(dat_main_long,
		   i_round_in_path == rounds_per_phase * 2,
		   condition %in% c('Baseline', cond)) %>%
	droplevels() %>%
	mutate(condition = C(as.factor(condition), sum),
		   drift_up = ifelse(drift < .5, -.5, .5))
}


# Q: Do they have more realistic beliefs after having experienced a "blocked" path?
lmer(formula = belief ~ (condition == 'Blocked Trades') + (1 | participant),
	data = filter(get_this_dat('Blocked Trades'), drift_up == .5)) %>%
	summary()


# Q: What about the fully blocked condition?
lmer(formula = belief ~ drift_up * condition + (1 | participant),
	data = get_this_dat('Blocked Info')) %>%
	summary()