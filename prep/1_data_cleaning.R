library(tidyverse)

master_list <- list() # See Readme

# Setup and load data
raw_file_path <- file.path('..', 'data', 'raw',
	'full_data_exp.csv')
processed_data_path <- file.path('..', 'data', 'processed')

dat <- read_csv(raw_file_path) %>%
	filter(participant._current_page_name == 'payoff_page')

# TODO: (4) Fix the "children in household" bug in the first session manually!

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
	    soep_general = 'Individual_Measures.1.player.soep_general',
	    soep_finance = 'Individual_Measures.1.player.soep_finance',
	    soep_ambiguity = 'Individual_Measures.1.player.soep_ambiguity',
	    soep_loss_aversion = 'Individual_Measures.1.player.soep_loss_aversion',
	    falk_patience_question = 'Individual_Measures.1.player.falk_patience_question',
	    chapman_impulse_question = 'Individual_Measures.1.player.chapman_impulse_question',
	    prob_no_dept_payment = 'Individual_Measures.1.player.prob_no_dept_payment',
		ravens_score = 'Ravens_Matrices.1.player.cogn_rpm_total_points',
		ravens_overestimation_ans = 'Ravens_Matrices.1.player.pers_rpm_overestimation_answer',
		ravens_overestimation_score = 'Ravens_Matrices.1.player.pers_rpm_overestimation_score',
		ravens_overplacement_ans = 'Ravens_Matrices.1.player.pers_rpm_overplacement_answer',
		age = 'Demographics.1.player.age',
		gender = 'Demographics.1.player.gender',
	    income_bracket = 'Demographics.1.player.income_bracket',
	    household_size = 'Demographics.1.player.household_size',
	    household_children = 'Demographics.1.player.household_children',
	    ethnicity = 'Demographics.1.player.ethnicity',
	    education = 'Demographics.1.player.education',
	    math_abilities_self_report = 'Demographics.1.player.math_abilities_self_report',
	    games_of_chance = 'Demographics.1.player.games_of_chance',
	    traded_stocks = 'Demographics.1.player.traded_stocks',
	    noticed_repetitions = 'Demographics.1.player.noticed_repetitions',
		investment_experience = 'Demographics.1.player.investment_experience',
		purpose = 'Demographics.1.player.purpose',
		attentiveness = 'Demographics.1.player.attentiveness',
		attention_check = 'Demographics.1.player.attention_check',
		engagement = 'Demographics.1.player.engagement',
		recognised_pattern =  'Demographics.1.player.pattern',
		participant_comments = 'Demographics.1.player.general_comments',
		dont_use_data = 'Demographics.1.player.dont_use_data',
		dont_use_data_reason = 'Demographics.1.player.dont_use_data_reason'))


# Excluding participants -----------------------------------
dat_all_wide <- mutate(dat_all_wide,
	excluded_reason = '',
	excluded_reason = if_else(attention_check != 1, 'attention_check/', ''),
	excluded_reason = if_else(attentiveness < 2,
		str_c(excluded_reason, 'attentiveness_question/'), excluded_reason),
	excluded_reason = if_else(dont_use_data == 1,
		str_c(excluded_reason, 'participant_reason'), excluded_reason))

dat_all_wide <- filter(dat_all_wide, excluded_reason == '')
dat_main_task <- filter(dat_main_task, participant %in% dat_all_wide$participant)


# Saving  --------------------------------------------------
master_list$dat <- dat_all_wide
write_delim(dat_all_wide, file.path(processed_data_path, 'dat_all_wide.csv'),
	delim = ';')
write_delim(dat_main_task, file.path(processed_data_path,
	'main_task_data.csv'), delim = ';')
