# Note: Overleaf can't handle anything but base and ggplot figures
# Therefore plots using patchwork and the like are exported to pdf.

# DE Numbers ----------------------------------------------------
line_df <- de_table %>%
	select(participant, plr, pgr, n_loss_shares, n_gain_shares) %>%
	pivot_longer(cols = c('plr', 'pgr'), names_to = 'propensity') %>%
	mutate(n_loss_or_gains = ifelse(propensity == 'PLR', n_loss_shares, n_gain_shares))

ggplot(line_df,
	aes(x = propensity, y = value, group = participant)) +
	geom_line(size = .75, color = 'skyblue4') +
	geom_point(aes(size = n_loss_or_gains, alpha = n_loss_or_gains), color = 'skyblue4') +
	labs(title = 'PLR and PGR resulting in DE', x = '', y = 'Value',
		 size = 'Amount', alpha = 'Amount')


# Individual price paths ----------------------------------------------------
if (FALSE) {  # Doesn't need to be sourced every time!
for (this_subj in unique(dat_main_task$participant)) {
	cat('Creating individual plots for participant ', this_subj)
	dat_main_task %>%
		filter(participant == this_subj) %>%
		ggplot(aes(x = i_round_in_path + 1, y = price, group = global_path_id)) +
			facet_grid(rows = vars(as.factor(distinct_path_id + 1))) +
			geom_hline(aes(yintercept = dat_main_task$price[1]),
					   color = 'darkgrey', size = .75, linetype = 'dotted') +
			geom_line(size = 1, alpha = .5) +
			geom_vline(aes(xintercept = 4), size = 1,
				alpha = .75, linetype = 'dashed') +
			geom_vline(aes(xintercept = 9), size = 1,
				alpha = .75, linetype = 'dashed') +
			scale_color_manual(values = c('darkgreen', 'darkred', 'black', 'cyan3'),
				labels = c('Blocked\nInfo\n', 'Blocked\nTrade\n',
						   'Full\nControl\n', 'MLA')) +
			labs(title = str_c('Participant ', this_subj), x = 'Round', y = 'Price')

	ggsave(file.path('output', 'figures', 'per_subject_plots',
		str_c('Price_paths_subj_', this_subj, '.pdf')), dev = 'pdf')

# Individual Plots of price and belief:
	for (i_path in seq(0, 8)) {
	p1 <- dat_main_task %>%
	  filter(participant == this_subj,
	    distinct_path_id == i_path,
	    condition != 'Blocked Info') %>%
	  ggplot(aes(i_round_in_path, price)) +
	    geom_hline(yintercept = 1000, alpha = .5) +
	    geom_vline(xintercept = 3, color = 'red', alpha = .5) +
	    geom_vline(xintercept = 8, color = 'red', alpha = .5) +
	    geom_line(position = position_dodge(.1), size = 1) +
	    geom_point(aes(shape = hold_type_after_trade, color = condition),
	      size = 5, position = position_dodge(.1)) +
	    scale_color_brewer(palette = 'Paired') +
	    scale_x_continuous(breaks = c(0, 4, 8)) +
	    scale_shape_manual(values = c(2, 1, 6)) +
	    theme(axis.text.x = element_blank(),
	          axis.ticks.x = element_blank(),
	          axis.title.x = element_blank())

	p2 <- dat_main_task %>%
	  filter(participant == unique(participant)[5],
	    distinct_path_id == i_path,
	    condition != 'Blocked Info') %>%
	  mutate(belief = belief / 100) %>%
	  ggplot(aes(i_round_in_path, belief, color = condition)) +
	    geom_hline(yintercept = .5, alpha = .5) +
	    geom_vline(xintercept = 3, color = 'red', alpha = .5) +
	    geom_vline(xintercept = 8, color = 'red', alpha = .5) +
	    geom_line(size = 1, show.legend = FALSE) +
	    geom_point(aes(shape = return_type_after_trade),
	      size = 5, position = position_dodge(.1)) +
	    scale_color_brewer(palette = 'Paired') +
	    scale_x_continuous(breaks = c(0, 4, 8)) +
	    scale_shape_manual(values = c(2, 6, 1)) +
	    ylim(c(0, 1))

	p1 +
	p2 +
	plot_layout(ncol = 1)

	ggsave(file.path('output', 'figures', 'per_subject_plots',
		str_c('Hold_and_belief_subj_', this_subj, '.pdf')), dev = 'pdf')
	}
}
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

ggsave(file.path('output', 'figures',
  'belief_end_p2_by_cond_and_hold_lines.pdf'), dev = 'pdf')

# How does the condition influence the belief in a down/up drift at end_p2?
master_list$plots$beliefs_end_p2_lines <- dat_main_task %>%
	filter(round_label == 'end_p2') %>%
	mutate(drift = as.factor(drift)) %>%
	ggplot(
		aes(x = condition, y = belief, color = drift)) +
		stat_summary(
			fun.data = "mean_se", fun.args = list(mult = 1.96)) +
		geom_line(aes(group = drift), stat = 'summary', fun = 'mean') +
		scale_color_manual(values = color_set[1:2]) +
		geom_hline(yintercept = 50, alpha = .75) +
		labs(x = 'Condition', y = 'Average Belief at end_p2',
			color = 'Drift')
master_list$plots$beliefs_end_p2_lines 


# How does the condition influence the change in belief between
#	beginning and end of p2?
master_list$plots$beliefs_diff_p1_p2_cond_lines <- dat_main_task %>%
	filter(round_label %in% c('end_p1', 'end_p2')) %>%
		pivot_wider(id_cols = c('participant', 'distinct_path_id', 'condition',
			'drift'), values_from = 'belief', names_from = 'round_label',
			names_prefix = 'belief_') %>%
	mutate(drift = as.factor(if_else(drift == .65, 'Up', 'Down')),
		belief_diff = belief_end_p2 - belief_end_p1) %>%
	ggplot(
		aes(x = condition, y = belief_diff, color = drift)) +
		stat_summary(
			fun.data = "mean_se", fun.args = list(mult = 1.96),
			position = position_dodge(width = .02)) +
		geom_line(aes(group = drift), stat = 'summary', fun = 'mean') +
		geom_hline(yintercept = 0, alpha = .75) +
		scale_color_manual(values = color_set[c(1, 3)]) +
		labs(x = 'Condition',
			y = 'Average belief difference between end_p1 and end_p2',
			color = 'Drift')
master_list$plots$beliefs_diff_p1_p2_cond_lines