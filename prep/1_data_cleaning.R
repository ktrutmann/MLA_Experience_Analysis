library(tidyverse)
# TODO: (1) Get renv to work properly...

# Setup and load data
raw_file_path <- file.path('..', 'data', 'raw', 'pre-pilot',
	'all_apps_wide_2020-05-03.csv')
processed_data_path <- file.path('..', 'data', 'processed')

dat <- read_csv(raw_file_path) %>%
	filter(participant._current_page_name == 'payoff_page')


# Filtering task data and reshaping
dat_main_task <- dplyr::select(dat,
		'participant.code',
		starts_with('Investment_Task.'),
		-contains('subsession.round_number')) %>%
	rename_with(pattern = 'Investment_Task.|player.|group.',
		.fn = str_remove_all) %>%
	pivot_longer(cols = -participant.code,
	names_to = c('round_number', '.value'),
	names_sep = '\\.')

# Beautify variable coding
dat_main_task <- mutate(dat_main_task,
	investable = as.logical(investable),
	condition_name = factor(condition_name,
	   levels = c('full_control', 'full_control_with_MLA',
	   		      'blocked_full_info', 'blocked_blocked_info'),
	   labels = c('Baseline', 'MLA', 'Blocked Trades', 'Blocked Info'))) %>%
	rename(condition = condition_name,
		participant = participant.code)


# Only for the first pre-pilot:
if (str_detect(raw_file_path, '2020-05-03')) {
	# The order of the "global path id" is mixed up together with the conditions.
	new_global_path_ids <- c()
	for (this_subj in unique(dat_main_task$participant)) {
		old_ids <- filter(dat_main_task, participant == this_subj) %>%
			pull(global_path_id)
		n_paths <- length(unique(old_ids))
		path_lengths <- rle(old_ids)$lengths
		for (i in seq_len(n_paths)) {
			new_global_path_ids <- c(new_global_path_ids, rep(i, each = path_lengths[i]))
		}
	}
	# TODO: (2) Fixme?
	dat_main_task$global_path_id <- new_global_path_ids
	rm(new_global_path_ids)

	# Cut the last path, because it is incomplete!
	dat_main_task <- filter(dat_main_task, global_path_id != max(global_path_id))

	# Last rounds in 'full_control_with_MLA' blocks are wrongly marked as investable!
	dat_main_task$investable[dat_main_task$i_round_in_path == 12] <- FALSE
}


# Creating the wide table --------------------------------------------------
dat_all_wide <- dat %>%
	dplyr::select(c(
		participant = 'participant.code',
		payoff = 'participant.payoff',
		strategy = 'Demographics.1.player.strategy',
		strategy_random = 'Demographics.1.player.strategy_random',
		strategy_feeling = 'Demographics.1.player.strategy_feeling',
		strategy_rational = 'Demographics.1.player.strategy_rational',
		strategy_short_rational = 'Demographics.1.player.strategy_short_rational',
		strategy_risk_averse = 'Demographics.1.player.strategy_risk_averse',
		strategy_short_risk_averse = 'Demographics.1.player.strategy_short_risk_averse',
		strategy_inertia = 'Demographics.1.player.strategy_inertia',
		strategy_DE = 'Demographics.1.player.strategy_DE',
		strategy_anti_DE = 'Demographics.1.player.strategy_anti_DE',
		age = 'Demographics.1.player.age',
		gender = 'Demographics.1.player.gender',
		is_student = 'Demographics.1.player.is_student',
		study_field = 'Demographics.1.player.study_field',
		investment_experience = 'Demographics.1.player.investment_experience',
		purpose = 'Demographics.1.player.purpose',
		engagement = 'Demographics.1.player.engagement',
		interest = 'Demographics.1.player.interest',
		participant_comments = 'Demographics.1.player.general_comments'))

# Saving  --------------------------------------------------
write_delim(dat_all_wide, file.path(processed_data_path, 'dat_all_wide.csv'),
	delim = ';')
write_delim(dat_main_task, file.path(processed_data_path,
	'main_task_data.csv'), delim = ';')
