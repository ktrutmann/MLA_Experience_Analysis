library(tidyverse)
library(sandwich)
library(lmtest)
library(patchwork)

# TODO: (3) Also check updating between end p1 and end p2 for each condition!


# Q: How do the conditions influence the "belief error" at end_p2?
this_model <- dat_main_task %>%
  mutate(belief_error = abs(rational_belief - belief)) %>%
  filter(round_label == 'end_p2') %>%
  {lm(belief_error ~ condition, data = .)} #nolint

this_model_clust <- coeftest(this_model, vcov = vcovCL,
  cluster = ~participant + distinct_path_id)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4]
master_list$cond_on_belief_err_end_p2 <- this_model


# Is there an effect of (and interaction between) position and favorability in
    # the second phase of the baseline condition?
this_dat <- dat_main_task %>%
  mutate(return_pos_end_last_round = lag(return_type_after_trade)) %>%
  filter(condition %in% c('Baseline', 'Blocked Trades'),
    round_label %in% c('p2', 'end_p2'),
    return_pos_end_last_round != 'None',
    favorable_move_since_last != 'None') %>%
  mutate(return_pos_end_last_round = if_else(
    return_pos_end_last_round == 'Gain', 1, -1),
    favorable_move_since_last = if_else(
      favorable_move_since_last == 'Favorable', 1, -1))

this_model <- lm(belief_updates_bayes_corrected ~ return_pos_end_last_round *
  favorable_move_since_last, data = filter(this_dat, condition == 'Baseline'))
this_model_clust <- coeftest(this_model,
  vcov = vcovCL, cluster = ~participant)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4]
master_list$updating_p2_baseline <- this_model


# Is there an effect of (and interaction between) position and favorability in
    # the second phase of the blocked trades condition?
this_model <- lm(belief_updates_bayes_corrected ~ return_pos_end_last_round *
  favorable_move_since_last, data = filter(this_dat, condition == 'Blocked Trades'))
this_model_clust <- coeftest(this_model,
  vcov = vcovCL, cluster = ~participant)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4]
master_list$updating_p2_blocked_trades <- this_model


# Is there a three way interaction with the condition?
this_model <- lm(belief_updates_bayes_corrected ~ return_pos_end_last_round *
  favorable_move_since_last * condition, data = this_dat)
this_model_clust <- coeftest(this_model,
  vcov = vcovCL, cluster = ~participant)
# Add the cluster robust standard errors and p-values
this_model$clust_str_err <- this_model_clust[, 2]
this_model$p_val_clust <- this_model_clust[, 4]
master_list$updating_p2_by_condition <- this_model


# Did the interventions have the expected effect in each of the combinations of
# holding/shorting and gain/loss?
these_filters <- c('Holding Gain', 'Shorting Gain', 'Holding Loss', 'Shorting Loss')
make_plot <- function(dat, this_filter) {
  this_dat <- dat_main_task %>%
    filter(majority_updates_p2 == this_filter,
      round_label == 'end_p2') %>%
    mutate(condition = factor(condition, levels = c(
      'Baseline', 'Blocked Trades', 'Delayed Info', 'Blocked Info')))

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

# TODO: (4) Also check for effects of the IMs.
