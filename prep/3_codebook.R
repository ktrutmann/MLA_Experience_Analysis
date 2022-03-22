# A list containing explanations for the most important variables of the analysis

cb_dat_all_wide <- list(
	# DE measures
	de = paste('The DE measure for each participant, calculated for each period',
		'in which they could make a decision.'),
	de_last_period = paste('The DE for each participant calculated from only the',
		'last period in the block in which they could make a decision',
		'(similar to the original Weber & Camerer (1998) design.'),
	# Other measures
	first_hold_condition = paste('How many assets did the participant hold',
		'on average after making the first trading decition in a particular',
		'condition.'),
	last_hold_condition = paste('How many assets did the participant hold',
		'on average after making the last trading decition in a particular condition.'),
	first_belief_condition = paste('What was the average expectation for a price',
		'increase after making the first decision in this condition.'),
	last_belief_condition = paste('What was the average expectation for a price',
		'increase after making the last decision in this condition.'),
	cor_belief_drift = paste('How well do the beliefs and drifts correlate for',
		'this participant (including all reported beliefs)?'),
	cor_belief_drift_p = paste('The p-value of the correlation test between the',
		'reported expectations and the drift of the path for this participant.'),
	experiment_time_in_minutes = paste('How long did it take this participant to',
		'complete the experiment?')
	)

cb_dat_main_task <- list(
	belief_diff_since_last = paste('The difference between the belief in a price',
		' increase reported in the current round minus the one reported in the',
		' previous round.'),
	belief_diff_since_last_flipped = paste('Takes the belief_diff_since_last variable but',
		' flipps its sign whenever the update was made from a price decrease.'),
	belief_updates_bayes_corrected = paste('Here the "correct" belief update was subtracted',
		' from the flipped reported update, resulting in a measure of under or over updating.'),
	round_label = paste('Labels special points in each path. p here stands for phase.',
		' end_p1 therefore stands for "end of phase 1".'),
	hold_after_trade = paste('What did the portfolio look like after the investment',
		' decision in the current round?'),
	return_type_after_trade = paste('If one sells a gain, then the return type',
		' goes back to "None" after the decision. This is encoded in this variable.')
	)