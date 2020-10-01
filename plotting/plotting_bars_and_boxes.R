require(tidyverse)
library(ggplot2)
source("helper_functions.R")


# Setup and load data ----------------------------------------------------
# TODO: Put all this in a seperate setup file that is sourced in each other .R file.
data_path <- '..//..//data//processed'
data_file_name_long <- 'all_participants_long_task_data.csv'
data_file_name_wide <- 'dat_all_wide.csv'

dat_main_long <- read_delim(file.path(data_path, data_file_name_long), delim = ';')
dat_all_wide <- read_delim(file.path(data_path, data_file_name_wide), delim = ';')


# Holding at T2 by Condition ---------------------------------------------

ggplot(filter(dat_main_long, i_round_in_path == rounds_per_phase * 2 + 1),
  aes(x = ifelse(drift > .5, 'Drift Up', 'Drift Down'), y = hold)) +
    facet_grid(cols = vars(condition)) + 
    geom_jitter(width = .2, height = .1, alpha = .5) +
    geom_boxplot(aes(fill = condition), alpha = .75) +
    stat_summary(fun.y = mean, geom = 'point', shape = 23,
      size = 5, fill = '#dd0000', color = '#dd0000') +
    theme_minimal() +
    theme(legend.position = 'none')


# Beliefs at T2 by Condition ---------------------------------------------
ggplot(filter(dat_main_long, i_round_in_path == rounds_per_phase * 2),
  aes(x = ifelse(drift > .5, 'Drift Up', 'Drift Down'), y = belief)) +
    facet_grid(cols = vars(condition)) + 
    geom_jitter(width = .2, height = .1, alpha = .5) +
    geom_boxplot(aes(fill = condition), alpha = .75) +
    stat_summary(fun.y = mean, geom = 'point', shape = 23,
      size = 5, fill = '#dd0000', color = '#dd0000') +
    theme_minimal() +
    theme(legend.position = 'none')


# DE Numbers ----------------------------------------------------

# DE Boxplot
ggplot(dat_all_wide, aes(x = 0, y = de)) +
  geom_boxplot(fill = "skyblue4", outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  geom_hline(yintercept = 0, color = 'darkgrey', size = 1) +
  labs(title = 'Disposition Effect', x = '') +
  theme_minimal() +
  theme(axis.text.x = element_blank())
	 
# calculate PLR & PGR
ggplot(dat_all_wide, aes(0, plr)) +
  geom_boxplot(fill = "skyblue4", outlier.shape = NA) +
  geom_jitter(width = 0.1, height = 0) +
  geom_hline(yintercept = 0, color = 'darkgrey', size = 1) +
  labs(title = 'Propensity to sell Losses', x = '') +
  theme_minimal() +
  theme(axis.text.x = element_blank())

ggplot(dat_all_wide, aes(0, pgr)) +
  geom_boxplot(fill = "skyblue4", outlier.shape = NA) +
  geom_jitter(width = 0.1, height = 0) +
  geom_hline(yintercept = 0, color = 'darkgrey', size = 1) +
  labs(title = 'Propensity to sell Gains', x = '') +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# WC Style DE:
  ggplot(dat_all_wide, aes(x = 0, y = de_last_period)) +
  geom_boxplot(fill = "skyblue4", outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  geom_hline(yintercept = 0, color = 'darkgrey', size = 1) +
  labs(title = 'Disposition Effect', x = '') +
  theme_minimal() +
  theme(axis.text.x = element_blank())


# Time Spent on Exp ----------------------------------------------------
ggplot(dat_all_wide,
	aes(x = participant, y = experiment_time_in_minutes, group = 1)) +
	geom_bar(fill = 'skyblue4', stat = 'identity') +
	geom_hline(yintercept = mean(dat_all_wide$experiment_time_in_minutes),
		color = 'darkred', size = 1) +
	labs(title = 'Time spent on Experiment', x = 'Participant IDs', y = 'Minutes') +
	theme_minimal()


# Payoffs ----------------------------------------------------
ggplot(dat_all_wide,
	aes(x = participant, y = payoff, group = 1)) +
	geom_bar(fill = 'skyblue4', stat = 'identity') +
	geom_hline(yintercept = mean(dat_all_wide$payoff),
		color = 'darkred', size = 1) +
	labs(title = 'Payoff in Points', x = 'Participant IDs', y = 'Points') +
	theme_minimal()
