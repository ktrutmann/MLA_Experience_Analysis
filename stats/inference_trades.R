library(tidyverse)
# library(lme4)
# library(lmerTest)
# library(sandwich)

# TODO: (1) Try out car::contr.Sum(levels=derp). Tweet tip by Jana!

# A function to retreive the last investment in the baseline as well as a
# specified condition
get_this_dat <- function(cond) {
	filter(dat_main_long,
		   i_round_in_path == rounds_per_phase * 2 + 1,
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
lmer(formula = hold ~ drift_up * condition + (1 | participant),
	 data = get_this_dat('Blocked Info')) %>%
	 summary()


# Q: What about the MLA effect at the end?
lmer(formula = hold ~ drift_up * condition + (1 | participant),
	 data = get_this_dat('MLA')) %>%
	 summary()
