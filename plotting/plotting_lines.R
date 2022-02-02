require(tidyverse)
library(patchwork)


# Setup and load data ----------------------------------------------------
# TODO: Put all this in a seperate setup file that is sourced in each other .R file.
data_path <- file.path('..', '..', 'data', 'processed')
data_file_name_long <- 'all_participants_long_task_data.csv'
data_file_name_wide <- 'dat_all_wide.csv'

dat_main_long <- read_delim(file.path(data_path, data_file_name_long), delim = ';')
dat_all_wide <- read_delim(file.path(data_path, data_file_name_wide), delim = ';')

theme_set(theme_minimal())

# DE Numbers ----------------------------------------------------

line_df <- dat_all_wide %>%
	select(participant, plr, pgr, n_losses, n_gains) %>%
	gather('propensity', 'value', -participant, -n_losses, -n_gains) %>%
	mutate(n_loss_or_gains = ifelse(propensity == 'PLR', n_losses, n_gains))

ggplot(line_df,
	aes(x = propensity, y = value, group = participant)) +
	geom_line(size = .75, color = 'skyblue4') +
	geom_point(aes(size = n_loss_or_gains, alpha = n_loss_or_gains), color = 'skyblue4') +
	labs(title = 'PLR and PGR resulting in DE', x = '', y = 'Value',
		 size = 'Amount', alpha = 'Amount') +
	theme_minimal()


# Individual price paths ----------------------------------------------------
for (this_subj in unique(dat_main_long$participant)) {
	dat_main_long  %>%
		filter(participant == this_subj) %>%
		ggplot(aes(x = i_round_in_path + 1, y = price, group = global_path_id,
				color = condition)) +
			facet_grid(rows = vars(as.factor(distinct_path_id + 1))) +
			geom_hline(aes(yintercept = dat_main_long$price[1]),
					   color = 'grey', size = .5, linetype = 'dotted') +
			geom_line(size = 1, alpha = .5) +
			geom_vline(aes(xintercept = 5), size = 1,
				alpha = .75, linetype = 'dashed') +
			geom_vline(aes(xintercept = 9), size = 1,
				alpha = .75, linetype = 'dashed') +
			scale_color_manual(values = c('darkgreen', 'darkred', 'black', 'cyan3'),
				labels = c('Blocked\nInfo\n', 'Blocked\nTrade\n',
						   'Full\nControl\n', 'MLA')) +
			labs(title = str_c('Participant ', this_subj), x = 'Round', y = 'Price',
				 color = 'Condition') +
			theme_minimal()

	save_kevplot(file.path('..', 'figures', 'per_subject_plots',
		str_c('Lines_subj_', this_subj)))
}

# Beliefs at end_p2 per condition ---------------------------------------------

# Did the interventions have the expected effect in each of the combinations of
# holding/shorting and gain/loss?
these_filters <- c('Holding Gain', 'Shorting Gain', 'Holding Loss', 'Shorting Loss')
make_plot <- function(dat, this_filter) {
  this_dat <- dat_main_task %>%
    filter(majority_updates_p2 == this_filter,
      round_label == 'end_p2',
  	  condition != 'Blocked Info') %>%
    mutate(condition = factor(condition, levels = c(
      'Baseline', 'Blocked Trades', 'Delayed Info')))

  this_model <- lm(belief ~ condition, data = this_dat) %>%
    coeftest(vcov = vcovCL, cluster = ~participant)

  this_dat <- this_dat %>%
    group_by(condition) %>%
    summarise(avg_belief = mean(belief),
      n = n()) %>%
    add_column(std_err = this_model[, 'Std. Error']) %>%
    mutate(low_95ci = avg_belief - qt(.975, df = n - 1) * std_err,
      high_95ci = avg_belief + qt(.975, df = n - 1) * std_err)

  ggplot(this_dat, aes(x = condition, y = avg_belief)) +
    geom_point() +
    geom_line(group = 1) +
    geom_errorbar(aes(ymin = low_95ci, ymax = high_95ci),
      width = .1) +
    labs(x = 'Condition', y = 'Average Belief at end of Phase Two',
      title = str_c('Majority Updates from ', this_filter)) +
    geom_hline(yintercept = 50, alpha = .5) +
    ylim(10, 90)
}
plotlist <- lapply(these_filters, make_plot, dat = dat_main_task)
# Generating the plots via patchwork:
(plotlist[[1]] | plotlist[[2]]) /
(plotlist[[3]] | plotlist[[4]])
