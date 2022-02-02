library(tidyverse)

master_list <- list() # See Readme

# Setup and load data
raw_file_path <- file.path('..', 'data', 'raw', 'prolific_pilot',
	'all_apps_wide-2022-01-19.csv')
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

# Beautify variable coding and remove non-informatives:
dat_main_task <- mutate(dat_main_task,
	investable = as.logical(investable),
	condition_name = factor(condition_name,
	    levels = c('full_control', 'blocked_full_info', 'blocked_delayed_info',
	   		'blocked_blocked_info'),
	    labels = c('Baseline', 'Blocked Trades', 'Delayed Info', 'Blocked Info'))) %>%
	rename(condition = condition_name,
		participant = participant.code) %>%
	select(-c('id_in_group', 'role', 'completion_code',
		'i_wining_block', 'id_in_subsession'))


# Creating the wide table --------------------------------------------------
dat_all_wide <- dat %>%
	dplyr::select(c(
		participant = 'participant.code',
		payoff = 'participant.payoff',
		strategy = 'Strategy.1.player.strategy',
		strategy_random = 'Strategy.1.player.strategy_random',
		strategy_feeling = 'Strategy.1.player.strategy_feeling',
		strategy_rational = 'Strategy.1.player.strategy_rational',
		strategy_short_rational = 'Strategy.1.player.strategy_short_rational',
		strategy_risk_averse = 'Strategy.1.player.strategy_risk_averse',
		strategy_short_risk_averse = 'Strategy.1.player.strategy_short_risk_averse',
		strategy_inertia = 'Strategy.1.player.strategy_inertia',
		strategy_DE = 'Strategy.1.player.strategy_DE',
		strategy_anti_DE = 'Strategy.1.player.strategy_anti_DE',
		age = 'Demographics.1.player.age',
		gender = 'Demographics.1.player.gender',
		is_student = 'Demographics.1.player.is_student',
		study_field = 'Demographics.1.player.study_field',
		investment_experience = 'Demographics.1.player.investment_experience',
		purpose = 'Demographics.1.player.purpose',
		attentiveness = 'Demographics.1.player.attentiveness',
		attention_check = 'Demographics.1.player.attention_check',
		engagement = 'Demographics.1.player.engagement',
		recognised_pattern =  'Demographics.1.player.pattern',
		participant_comments = 'Demographics.1.player.general_comments',
		dont_use_data = 'Demographics.1.player.dont_use_data'))

# For calculating payoffs:
	# Note: Make sure to exclude the ones who are already payed!
	# dat %>%
	# 	transmute(
	# 		prolific_id = Tutorial_Investment_Task.1.player.prolific_id,
	# 		payoff = round(participant.payoff * .015, 2)
	# 	) %>% print(n = 40)

# Excluding participants -----------------------------------
	# TODO: (9) Write code to add an exclusion flag and filter for it here!

# Saving  --------------------------------------------------
master_list$dat <- dat_all_wide
write_delim(dat_all_wide, file.path(processed_data_path, 'dat_all_wide.csv'),
	delim = ';')
write_delim(dat_main_task, file.path(processed_data_path,
	'main_task_data.csv'), delim = ';')
