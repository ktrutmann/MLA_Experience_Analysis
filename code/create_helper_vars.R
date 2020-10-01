dat_main_long <- mutate(dat_main_long,
	hold_drift_flipped = ifelse(drift > .5, hold, -hold),
	belief_drift_flipped = ifelse(drift > .5, belief, 100 - belief))

# TODO: Create a variable with some key indicators for a rational trader