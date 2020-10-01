# A list containing explanations for the most important variables of the analysis

cb_dat_all_wide <- list(
	# DE measures
	de = str_c('The DE measure for each participant, calculated for each period ',
		'in which they could make a decision.'),
	de_last_period = str_c('The DE for each participant calculated from only the ',
		'last period in the block in which they could make a decision ',
		'(similar to the original Weber & Camerer (1998) design.'),
	# Other measures
	first_hold_condition = str_c('How many assets did the participant hold',
		'on average after making the first trading decition in a particular ',
		'condition.'),
	last_hold_condition = str_c('How many assets did the participant hold',
		'on average after making the last trading decition in a particular condition.'),
	first_belief_condition = str_c('What was the average expectation for a price ',
		'increase after making the first decision in this condition.'),
	last_belief_condition = str_c('What was the average expectation for a price',
		'increase after making the last decision in this condition.'),
	cor_belief_drift = str_c('How well do the beliefs and drifts correlate for',
		'this participant (including all reported beliefs)?'),
	cor_belief_drift_p = str_c('The p-value of the correlation test between the',
		'reported expectations and the drift of the path for this participant.'),
	experiment_time_in_minutes = str_c('How long did it take this participant to',
		'complete the experiment?')
	)

cb_dat_main_long <- list(
	hold_drift_flipped = str_c('The hold variable "corrected" for the drift of the',
		'path. If the drift is < .5, one should short, so "more shorting is better".'),
	belief_drift_flipped = str_c('The belief variable "corrected" for the drift of',
		'the path. If the drift is < .5, one should have a negative belief, meaning',
		'that "less is better".')
	)