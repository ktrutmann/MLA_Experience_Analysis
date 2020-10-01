library(tidyverse)
source("helper_functions.R")


# Setup -------------------------------------------------------------------

raw_file_path <- file.path('..', '..', 'data', 'raw', 'bot-test',
	'all_apps_wide.csv')
processed_data_path <- file.path('..', '..', 'data', 'processed')

# Loading and splitting Data ---------------------------------------------------

dat <- read_csv(raw_file_path)
# Remove all non-finishers:
dat <- filter(dat, participant._current_page_name == 'payoff_page')

task_dat <- dplyr::select(dat, "participant.code",
    starts_with("Investment_Task."))
names(task_dat) <- str_remove_all(names(task_dat), "Investment_Task.")


# Reshaping --------------------------------------------------
dat_main_long <- otree_reshape(task_dat,
    save_path = processed_data_path,
    filename_str = "task_data",
    drop_prefix = TRUE,
    vars_to_drop = c('id_in_group', 'id_in_subsession')
)

# Manual fixes ----------------------------------------------
dat_main_long <- mutate(dat_main_long,
						investable = investable == 1,
						condition = factor(condition_name,
										   levels = c('full_control',
										   		      'full_control_with_MLA',
										   		      'blocked_full_info',
										   		      'blocked_blocked_info'),
										   labels = c('Baseline',
										   		      'MLA',
										   		      'Blocked Trades',
										   		      'Blocked Info')))
dat_main_long <- dplyr::select(dat_main_long, -condition_name)

# Only for the first pre-pilot:
if (str_detect(raw_file_path, '2020-05-03')) {
	# The order of the "global path id" is mixed up together with the conditions. Fixing that here.
	new_global_path_ids <- c()
	for (this_subj in unique(dat_main_long$participant)) {
		old_ids <- filter(dat_main_long, participant == this_subj) %>%
			pull(global_path_id)
		n_paths <- length(unique(old_ids))
		path_lengths <- rle(old_ids)$lengths
		for (i in seq_len(n_paths)) {
			new_global_path_ids <- c(new_global_path_ids, rep(i, each = path_lengths[i]))
		}
	}
	dat_main_long$global_path_id <- new_global_path_ids
	rm(new_global_path_ids)

	# Cut the last path, because it is incomplete!
	dat_main_long <- filter(dat_main_long, global_path_id != max(global_path_id))

	# Last rounds in 'full_control_with_MLA' blocks are wrongly marked as investable!
	dat_main_long$investable[dat_main_long$i_round_in_path == 12] <- FALSE
}


# Creating the wide table --------------------------------------------------
# A dictionarry with the names in dat and the new names in dat_all_wide:
vars_for_wide_table <- list(participant.code = 'participant',
	participant.payoff = 'payoff',
	Get_Payment_Info.1.player.matr_nr = 'matr_nr',
	Demographics.1.player.strategy = 'strategy',
	Demographics.1.player.strategy_random = 'strategy_random',
	Demographics.1.player.strategy_feeling = 'strategy_feeling',
	Demographics.1.player.strategy_rational = 'strategy_rational',
	Demographics.1.player.strategy_short_rational = 'strategy_short_rational',
	Demographics.1.player.strategy_risk_averse = 'strategy_risk_averse',
	Demographics.1.player.strategy_short_risk_averse = 'strategy_short_risk_averse',
	Demographics.1.player.strategy_inertia = 'strategy_inertia',
	Demographics.1.player.strategy_DE = 'strategy_DE',
	Demographics.1.player.strategy_anti_DE = 'strategy_anti_DE',
	Demographics.1.player.age = 'age',
	Demographics.1.player.gender = 'gender',
	Demographics.1.player.is_student = 'is_student',
	Demographics.1.player.study_field = 'study_field',
	Demographics.1.player.investment_experience = 'investment_experience',
	Demographics.1.player.purpose = 'purpose',
	Demographics.1.player.engagement = 'engagement',
	Demographics.1.player.interest = 'interest',
	Demographics.1.player.general_comments = 'participant_comments')

dat_all_wide <- dat %>%
	dplyr::select(names(vars_for_wide_table))
names(dat_all_wide) <- unlist(vars_for_wide_table)

# Saving  --------------------------------------------------
write_delim(dat_all_wide, file.path(processed_data_path, 'dat_all_wide.csv'),
	delim = ';')
write_delim(dat_main_long, file.path(processed_data_path,
	'all_participants_long_task_data.csv'), delim = ';')
