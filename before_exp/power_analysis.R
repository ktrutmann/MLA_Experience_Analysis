library(tidyverse)
library(lmtest)
library(sandwich)


# Target measures are improving the drift hit-rate of the last decision.
# We're going to go off a minimal effect of interest of an average improvement
# of "half a share", i.e. .5.


# Setup ------------------------------------------------
sample_sd <- 2.93
expected_effect <- sample_sd * .2 # Small coens d
n_paths_per_condition <- 6

# Notes:
# sd drift hit rates: 2.93

sample_sizes <- c(seq(100, 300, 50))
conditions <- c('Baseline', 'Condition_A', 'Condition_B')
n_repetitions <- 10000
alpha_level <- .1 # .1 for one-sided test!


# Simulation ------------------------------------------------
set.seed(220519)
results <- array(dim = c(
	length(conditions),
	length(sample_sizes),
	n_repetitions))

for (sample_size_ix in seq_along(sample_sizes)) {
	for (repetition_ix in seq_len(n_repetitions)) {
		try_again <- TRUE  # Some simulations generate NA standard errors. Repeat those.
		while (try_again) {
			try_again <- FALSE
			this_n_per_condition <- sample_sizes[sample_size_ix]
			this_dat <- tibble(condition = rep(conditions,
					each = this_n_per_condition * n_paths_per_condition),
				price_path = rep(1:n_paths_per_condition,
					this_n_per_condition * length(conditions)),
				participant = rep(rep(1:this_n_per_condition,
					each = n_paths_per_condition), length(conditions)),
				outcome = rnorm(n = length(conditions) *
					this_n_per_condition * n_paths_per_condition,
					mean = ifelse(condition == 'Baseline', 0, expected_effect),
					sd = sample_sd)) %>%
				# Add some "clustering"
				mutate(outcome = outcome + ((participant / this_n_per_condition) - .5)
						* sample_sd,
					outcome = outcome + ((price_path / n_paths_per_condition) - .5)
						* sample_sd)

			results[, sample_size_ix, repetition_ix] <-
				lm(outcome ~ condition, data = this_dat) %>%
				coeftest(vcov = vcovCL,
					cluster = ~participant + price_path) %>%
				`[`(, 'Pr(>|t|)')

			if (any(is.na(results[, sample_size_ix, repetition_ix]))) {
				try_again <- TRUE
			}
		}
		if (repetition_ix %in% seq(0, n_repetitions, as.integer(n_repetitions / 10))) {
			cat('Repetition ', repetition_ix, '\n')
		}
	}
	cat('Done simulating sample size ', sample_sizes[sample_size_ix], '\n')
}


# Analysis ------------------------------------------------

results_bool <- results <= alpha_level

# Power for one condition:
power_table <- apply(results_bool, c(1, 2), mean)

power_data <- as_tibble(power_table) %>%
	setNames(paste0('Sample_', sample_sizes)) %>%
	add_column(condition = conditions) %>%
	pivot_longer(cols = -'condition', names_to = c(NA, 'sample_size'),
		names_sep = '_', values_to = 'power') %>%
	mutate(sample_size = as.numeric(sample_size))

ggplot(filter(power_data, condition == 'Condition_A'),
	aes(x = sample_size, y = power)) +
	geom_point() +
	geom_line(aes(group = condition)) +
	geom_hline(yintercept = .9, color = 'red', alpha = .7) +
	geom_text(label = 'Power = .9', x = min(sample_sizes), hjust = 'left',
		y = .92, color = 'red', alpha = .7) +
	geom_hline(yintercept = .8, color = 'orange', alpha = .7) +
	geom_text(label = 'Power = .8', x = min(sample_sizes), hjust = 'left',
		y = .82, color = 'orange', alpha = .7) +
	ylim(0, 1) +
	labs(title = 'Improving Hit Rate of last Decision on Drift',
		subtitle = str_c(n_repetitions, ' Simulations'),
		x = 'Sample Size',
		y = 'Power') +
	theme_minimal()

ggsave(file.path('output', 'figures', 'power_analysis',
	'drift_hit_rate_small_effect.pdf'), device = 'pdf',
	height = 20, width = 30, units = 'cm')


# Power to detect effect in both conditions:
power_table <- apply(results_bool[2:3, , ], c(3, 2), all) %>%
	apply(2, mean)

power_data <- as_tibble(power_table) %>%
	rename(power = value) %>%
	add_column(sample_size = sample_sizes)

ggplot(power_data, aes(x = sample_size, y = power)) +
	geom_point() +
	geom_line(aes(group = 1)) +
	geom_hline(yintercept = .9, color = 'red', alpha = .7) +
	geom_text(label = 'Power = .9', x = min(sample_sizes), hjust = 'left',
		y = .92, color = 'red', alpha = .7) +
	geom_hline(yintercept = .8, color = 'orange', alpha = .7) +
	geom_text(label = 'Power = .8', x = min(sample_sizes), hjust = 'left',
		y = .82, color = 'orange', alpha = .7) +
	ylim(0, 1) +
	labs(title = 'Improving Hit Rate of last Decision on Drift in both Conditions',
		subtitle = str_c(n_repetitions, ' Simulations'),
		x = 'Sample Size',
		y = 'Power') +
	theme_minimal()

ggsave(file.path('output', 'figures', 'power_analysis',
	'drift_hit_rate_both_conditions_small_effect.pdf'), device = 'pdf',
	height = 20, width = 30, units = 'cm')