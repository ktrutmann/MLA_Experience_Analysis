library(tidyverse)

# Setup and load data
raw_file_path <- file.path('..', 'data', 'raw', 'bot-test',
	'all_apps_wide.csv')
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
	   levels = c('full_control', 'blocked_full_info', 'blocked_blocked_info'),
	   labels = c('Baseline', 'Blocked Trades', 'Blocked Info'))) %>%
	rename(condition = condition_name,
		participant = participant.code)


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
