library(tidyverse)
library(lmtest)
library(sandwich)


# Target measures are improving the drift hit-rate of the last decision.
# We're going to go off a minimal effect of interest of an average improvement
# of "half a share", i.e. .5.


# Setup ------------------------------------------------
sample_sd <- 2.93
expected_effect <- sample_sd * .2 # Small coens d

# Notes:
# sd drift hit rates: 2.93

sample_sizes <- c(seq(240, 340, 20))
conditions <- c('Baseline', 'Condition_A', 'Condition_B')
n_repetitions <- 5000
alpha_level <- .1 # .1 for one-sided test!


# Simulation ------------------------------------------------
results <- array(dim = c(
	length(conditions),
	length(sample_sizes),
	n_repetitions))

for (sample_size_ix in seq_along(sample_sizes)) {
	for (repetition_ix in seq_len(n_repetitions)) {
		this_n_per_condition <- sample_sizes[sample_size_ix]
		this_dat <- tibble(condition = rep(conditions,
				each = this_n_per_condition),
			outcome = rnorm(n = length(conditions) * this_n_per_condition,
				mean = ifelse(condition == 'Baseline', 0, expected_effect),
				sd = sample_sd))

		# TODO: (4) We don't need clusting, right?
		results[, sample_size_ix, repetition_ix] <-
			lm(outcome ~ condition, data = this_dat) %>%
			summary() %>%
			pluck('coefficients') %>%
			`[`(, 'Pr(>|t|)')
	}
	cat('Done simulating sample size ', sample_sizes[sample_size_ix], '\n')
}


# Analysis ------------------------------------------------

results_bool <- results <= alpha_level
power_table <- apply(results_bool, c(1, 2), mean)

power_data <- as_tibble(power_table) %>%
	setNames(paste0('Sample_', sample_sizes)) %>%
	add_column(condition = conditions) %>%
	pivot_longer(cols = -'condition', names_to = c(NA, 'sample_size'),
		names_sep = '_', values_to = 'power')

ggplot(filter(power_data, condition == 'Condition_A'),
	aes(x = sample_size, y = power)) +
	geom_point() +
	geom_line(aes(group = condition)) +
	geom_hline(yintercept = .9, color = 'red', alpha = .7) +
	geom_text(label = 'Power = .9', x = 1, y = .92, color = 'red', alpha = .7) +
	geom_hline(yintercept = .8, color = 'orange', alpha = .7) +
	geom_text(label = 'Power = .8', x = 1, y = .82, color = 'orange', alpha = .7) +
	ylim(0, 1) +
	labs(title = 'Improving Hit Rate of last Decision on Drift',
		subtitle = str_c(n_repetitions, ' Simulations'),
		x = 'Sample Size',
		y = 'Power')

ggsave(file.path('output', 'figures', 'power_analysis',
	'drift_hit_rate_small_effect.pdf'), device = 'pdf',
	height = 20, width = 30, units = 'cm')