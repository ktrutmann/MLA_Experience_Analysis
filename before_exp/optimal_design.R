# See readme.md for notes about this file

library(tidyverse)
library(vroom)

theme_set(theme_minimal())

# Load data and reshape ----
processed_data_path <- file.path('..', 'data', 'processed')
raw_data_path <- file.path('..', 'data', 'raw', 'optimal_design')
dat_4_rpp_lr1 <- vroom(file.path(raw_data_path, 'data_4_rpp_lr1.csv'))
dat_4_rpp_lr125 <- vroom(file.path(raw_data_path, 'data_4_rpp_lr125.csv'))
dat_5_rpp_lr1 <- vroom(file.path(raw_data_path, 'data_5_rpp_lr1.csv'))
dat_5_rpp_lr125 <- vroom(file.path(raw_data_path, 'data_5_rpp_lr125.csv'))
dat_3_5_rpp_lr1 <- vroom(file.path(raw_data_path, 'data_3_5_rpp_lr1.csv'))
dat_3_5_rpp_lr125 <- vroom(file.path(raw_data_path, 'data_3_5_rpp_lr125.csv'))

dat_4_rpp_lr1$rounds_per_phase <- 4
dat_4_rpp_lr1$lr_multiplier <- 1
dat_4_rpp_lr125$rounds_per_phase <- 4
dat_4_rpp_lr125$lr_multiplier <- 1.25
dat_5_rpp_lr1$rounds_per_phase <- 5
dat_5_rpp_lr1$lr_multiplier <- 1
dat_5_rpp_lr125$rounds_per_phase <- 5
dat_5_rpp_lr125$lr_multiplier <- 1.25
dat_3_5_rpp_lr1$rounds_per_phase <- 3.5
dat_3_5_rpp_lr1$lr_multiplier <- 1
dat_3_5_rpp_lr125$rounds_per_phase <- 3.5
dat_3_5_rpp_lr125$lr_multiplier <- 1.25

names_to_join_by <- names(select(dat_4_rpp_lr1, -contains('learning_rate')))

oed_data <- full_join(dat_4_rpp_lr1, dat_5_rpp_lr1, by = names(dat_4_rpp_lr1)) %>%
	full_join(dat_3_5_rpp_lr1, by = names_to_join_by) %>%
	full_join(dat_4_rpp_lr125, by = names_to_join_by) %>%
	full_join(dat_5_rpp_lr125) %>%
	full_join(dat_3_5_rpp_lr125, by = names_to_join_by) %>%
	dplyr::select('participant.id_in_session', 'participant.label',
		'rounds_per_phase', 'lr_multiplier', starts_with('Investment_Task.'),
		-contains('subsession.round_number')) %>%
	rename_with(pattern = 'Investment_Task.|player.|group.',
		.fn = str_remove_all) %>%
	pivot_longer(cols = -c('participant.id_in_session',
		'participant.label', 'rounds_per_phase', 'lr_multiplier'),
	names_to = c('round_number', '.value'),
	names_sep = '\\.') %>%
	select(case_id = participant.label,
		participant = participant.id_in_session, i_round_in_path, drift,
		price, distinct_path_id, belief, hold, transaction, returns,
		cash, final_cash, investable, rounds_per_phase, lr_multiplier) %>%
	mutate(case_id = as.numeric(str_extract(case_id, '\\d+')))

# The caselist:
caselist <- jsonlite::fromJSON(file.path(raw_data_path, 'case_list.txt'),
	flatten = TRUE) %>%
	as_tibble() %>%
	transmute(model = model,
		up_prob_good = sapply(up_probs, pluck, 1),
		case_id = case_id)

oed_data <- left_join(oed_data, caselist, by = 'case_id')

# Add further variables ----
oed_data <- mutate(oed_data,
	round_label = case_when(
		i_round_in_path < rounds_per_phase ~ 'p1',
		i_round_in_path == rounds_per_phase ~ 'end_p1',
		i_round_in_path == rounds_per_phase * 2 ~ 'end_p2',
		i_round_in_path == rounds_per_phase * 2 + 1 ~ 'extra_round',
		i_round_in_path > rounds_per_phase ~ 'p2')) %>%
	mutate(round_label = case_when( # Handle 3/5 phase lengths # nolint
		rounds_per_phase == 3.5 & i_round_in_path < 3 ~ 'p1',
		rounds_per_phase == 3.5 & i_round_in_path == 3 ~ 'end_p1',
		rounds_per_phase == 3.5 & i_round_in_path == 8 ~ 'end_p2',
		rounds_per_phase == 3.5 & i_round_in_path == 9 ~ 'extra_round',
		rounds_per_phase == 3.5 & i_round_in_path > 3 ~ 'p2',
		TRUE ~ round_label
	))


# Target difference ----
oed_data %>%
	mutate(belief_before = lag(belief)) %>%
	filter(round_label == 'extra_round') %>%
	pivot_wider(id_cols = c(participant, distinct_path_id, up_prob_good,
		rounds_per_phase, lr_multiplier), values_from = c(hold, belief_before),
		names_from = model) %>%
	mutate(abs_hold_diff_between_models = abs(hold_CSRL - hold_RL_single),
		abs_belief_diff_between_models = abs(belief_before_CSRL -
			belief_before_RL_single),
		up_prob_good = as.factor(up_prob_good),
		rounds_per_phase = factor(rounds_per_phase, labels = c('3/5', '4/4', '5/5')),
		lr_multiplier = factor(lr_multiplier, labels = c('LR * 1', 'LR * 1.25'))) %>%
		group_by(up_prob_good, rounds_per_phase, lr_multiplier) %>%
	summarize(avg_abs_hold_diff = mean(abs_hold_diff_between_models),
		avg_abs_belief_diff = mean(abs_belief_diff_between_models)) %>%
	ggplot(aes(x = up_prob_good, y = avg_abs_belief_diff,
		fill = rounds_per_phase)) +
		geom_col(position = 'dodge') +
		facet_grid(rows = vars(lr_multiplier)) +
		labs(x = 'Drift Rate', y = 'Belief Difference', fill = 'Phase Lengths') +
		scale_fill_grey()

ggsave(file.path('output', 'figures', 'OED_plot.eps'), device = 'eps', width = 15,
	height = 10, units = 'cm')