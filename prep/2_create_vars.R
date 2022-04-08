# The purpose of this script is to generate additional variables
# which will be used in multiple analyses so that they don't
# have to be recreated each time.

require(tidyverse)

# Setup and load data ----------------------------------------------------
data_path <- file.path('..', 'data', 'processed')
data_file_name_long <- 'main_task_data.csv'
data_file_name_wide <- 'dat_all_wide.csv'
data_file_time <- file.path('..', '..', 'data', 'raw',
  'PageTimes-2022-03-17.csv')

dat_main_task <- read_delim(
  file.path(data_path, data_file_name_long), delim = ';')
dat_all_wide <- read_delim(
  file.path(data_path, data_file_name_wide), delim = ';')

# Constants
rounds_p1 <- 3  # Make sure this matches the python one!
rounds_p2 <- 5  # Make sure this matches the python one!
up_probs <- c(.65, .35) # The probs of an upward move in an up and down drift
max_hold <- 4

# Derivative Constants
n_blocks <- max(dat_main_task$global_path_id)
n_participants <- n_distinct(dat_main_task$participant)


# General additional variables: ----------------------------------------
dat_main_task <- mutate(dat_main_task,
  belief_diff_since_last = c(NA, diff(belief)),
	round_label = case_when(
		i_round_in_path < rounds_p1 ~ 'p1',
		i_round_in_path == rounds_p1 ~ 'end_p1',
		i_round_in_path == rounds_p1 + rounds_p2 ~ 'end_p2',
		i_round_in_path == rounds_p1 + rounds_p2 + 1 ~ 'extra_round',
		i_round_in_path > rounds_p1 ~ 'p2'),
	price_up_since_last = if_else(i_round_in_path == 0, NA,
		c(NA, diff(price) > 0)),
  inverse_update = if_else(belief_diff_since_last == 0,
    FALSE, price_up_since_last != (belief_diff_since_last > 0)),
  belief_diff_since_last_flipped = if_else(price_up_since_last,
    belief_diff_since_last, -belief_diff_since_last),
  hold_type = case_when(
    hold > 0 ~ 'Holding',
    hold < 0 ~ 'Shorting',
    hold == 0 ~ 'Not Inv.'),
	hold_after_trade = replace(lead(hold), round_label == 'extra_round', NA),
  hold_type_after_trade = case_when(
    lead(hold) > 0 ~ 'Holding',
    lead(hold) < 0 ~ 'Shorting',
    TRUE ~ 'None'),
  hold_type_after_trade = replace(hold_type_after_trade,
    round_label == 'extra_round', NA),
  return_type = case_when(
    returns > 0 ~ 'Gain',
    returns < 0 ~ 'Loss',
    returns == 0 ~ 'None'),
  return_type_after_trade = case_when(
    hold_after_trade == 0 ~ 'None',
    sign(hold_after_trade) != sign(hold) ~ 'None',
    round_label == 'extra_round' ~ 'None',
    TRUE ~ return_type),
  favorable_move_since_last = case_when(
    hold == 0 ~ 'None',
    (hold > 0) == price_up_since_last ~ 'Favorable',
    (hold > 0) != price_up_since_last ~ 'Unfavorable',
    TRUE ~ 'None'),
  updated_from = if_else(lag(return_type_after_trade) == 'None', 'None',
    str_c(lag(hold_type_after_trade), lag(return_type_after_trade), sep = ' ')))

# How many times has this path been shown before?
dat_main_task$path_position <- NA
cat('Creating the "path_position" variable. This takes a while, sorry.\n')
for (this_participant in unique(dat_main_task$participant)) {
  cat('\rWorking on participant', this_participant)
  for (i in 0:7) {
    dat_main_task$path_position[
      dat_main_task$distinct_path_id == i &
      dat_main_task$participant == this_participant] <- 1:3
  }
}
cat('\n')

# Determine whether someone was continualy invested and / or
# in a gain/loss position:
dat_main_task <- dat_main_task %>%
  filter(round_label %in% c('p2', 'end_p2')) %>%
  group_by(participant, global_path_id) %>%
  summarise(n_hold_types_in_p2 = n_distinct(hold_type),
    n_return_types_after_trade_in_p2 = n_distinct(return_type_after_trade),
    n_update_types_in_p2 = n_distinct(updated_from)) %>%
  ungroup() %>%
  right_join(dat_main_task, by = c('participant', 'global_path_id'))

# Determine what the main position was from which the updating in p2 happened
# In case of a tie, invested updates overrule non-invested
determine_majority <- function(dat) {
  temp <- tibble(updated_from = dat) %>%
    count(updated_from) %>%
    filter(n == max(n))

  if (nrow(temp) == 1) return(temp$updated_from) # nolint
  temp <- filter(temp, updated_from != 'None') # nolint
  if (nrow(temp) == 1) return(temp$updated_from) # nolint
  return('Tie')
}

cat('\nGetting majority situation in p2\n')
dat_main_task <- dat_main_task %>%
  filter(round_label %in% c('p2', 'end_p2')) %>%
  group_by(participant, global_path_id) %>%
  summarize(majority_updates_p2 = determine_majority(updated_from)) %>%
  ungroup() %>%
  full_join(dat_main_task,
    by = c('participant', 'global_path_id', 'majority_updates_p2'))


# Rational actor: ---------------------------------------------
dat_main_task$rational_belief <- 999
dat_main_task$rational_belief_state <- 999
cat('Caclculating rational beliefs and trades\n')
for (i in seq_len(nrow(dat_main_task))) {
	if (dat_main_task$i_round_in_path[i] == 0) {
		dat_main_task$rational_belief[i] <- .5
		dat_main_task$rational_belief_state[i] <- .5
		next
	}

	dat_main_task$rational_belief_state[i] <- with(dat_main_task,
		(rational_belief_state[i - 1] * up_probs[2 - price_up_since_last[i]]) /
			(rational_belief_state[i - 1] * up_probs[2 - price_up_since_last[i]] +
			((1 - rational_belief_state[i - 1]) * up_probs[price_up_since_last[i] + 1])))

	dat_main_task$rational_belief[i] <- dat_main_task$rational_belief_state[i] *
    up_probs[1] + (1 - dat_main_task$rational_belief_state[i]) * up_probs[2]
}

dat_main_task <- mutate(dat_main_task,
  rational_belief = ifelse((!condition %in% c('Baseline', 'Blocked Trades') &
    round_label == 'p2') | round_label == 'extra_round',
    NA, rational_belief * 100),
  rational_belief_state = rational_belief_state * 100,
	rational_hold_after_trade = case_when(
		rational_belief > 50 ~ max_hold,
		rational_belief < 50 ~ -max_hold,
    rational_belief == 50 ~ 0,
		TRUE ~ 0),
  rational_hold_after_trade = ifelse(round_label == 'extra_round',
    NA, rational_hold_after_trade))

# Rational holds / trades should also not change if trading is blocked:
for (i in seq_len(nrow(dat_main_task))) {
  if (dat_main_task$condition[i] == 'Baseline') next
  if (dat_main_task$round_label[i] == 'end_p1') {
    curr_hold <- dat_main_task$rational_hold_after_trade[i]
  } else if (dat_main_task$round_label[i] == 'p2') {
    dat_main_task$rational_hold_after_trade[i] <- curr_hold
  }
}

dat_main_task <- mutate(dat_main_task,
  rational_hold = lag(rational_hold_after_trade),
  rational_trade = c(diff(rational_hold), NA))

# Rational Return:
cat('Calculating rational returns\n')
dat_main_task$rational_returns <- 0
for (i in seq_len(nrow(dat_main_task) - 1) + 1) {
  if (dat_main_task$round_label[i] == 'extra_round' ||
    dat_main_task$i_round_in_path[i] == 0)
    next

  previous_return <- dat_main_task$rational_returns[i - 1]

  if (sign(dat_main_task$rational_hold[i]) != sign(dat_main_task$rational_hold[i - 1]) ||
      dat_main_task$rational_hold[i] == 0) {
    previous_return <- 0
  }

  dat_main_task$rational_returns[i] <- previous_return + dat_main_task$rational_hold[i] *
    (dat_main_task$price[i] - dat_main_task$price[i - 1])
}


# Correcting belief updates by the rational updates:
dat_main_task <- mutate(dat_main_task,
  belief_updates_bayes_corrected = belief_diff_since_last_flipped -
    abs(c(NA, diff(rational_belief))))


# DE Measure --------------------------------------------------------------
# This tibble contains one row per price path
# Note: We calculate the DE by counting the number of shares (Similar to Odean).
# Sales can not go over the zero line (i.e. can be maximally abs(hold)).
de_table <- tibble(
  participant = rep(unique(dat_main_task$participant), each = n_blocks),
  block = rep(seq_len(n_blocks), n_participants),
  condition = dat_main_task$condition[dat_main_task$i_round_in_path == 0],
  n_loss_shares = vector(mode = 'numeric', length = n_blocks * n_participants),
  loss_last_period = vector(mode = 'numeric', length = n_blocks * n_participants),
  n_gain_shares = vector(mode = 'numeric', length = n_blocks * n_participants),
  gain_last_period = vector(mode = 'numeric', length = n_blocks * n_participants),
  n_sold_shares = vector(mode = 'numeric', length = n_blocks * n_participants),
  sale_last_period = vector(mode = 'numeric', length = n_blocks * n_participants),
  n_sold_loss_shares = vector(mode = 'numeric', length = n_blocks * n_participants),
  sold_loss_last_period = vector(mode = 'numeric', length = n_blocks * n_participants),
  n_sold_gain_shares = vector(mode = 'numeric', length = n_blocks * n_participants),
  sold_gain_last_period = vector(mode = 'numeric', length = n_blocks * n_participants),
  rational_n_loss_shares = vector(mode = 'numeric', length = n_blocks * n_participants),
  rational_loss_last_period = vector(mode = 'numeric', length = n_blocks * n_participants),
  rational_n_gain_shares = vector(mode = 'numeric', length = n_blocks * n_participants),
  rational_gain_last_period = vector(mode = 'numeric', length = n_blocks * n_participants),
  rational_n_sold_shares = vector(mode = 'numeric', length = n_blocks * n_participants),
  rational_sale_last_period = vector(mode = 'numeric', length = n_blocks * n_participants),
  rational_n_sold_loss_shares = vector(mode = 'numeric', length = n_blocks * n_participants),
  rational_sold_loss_last_period = vector(mode = 'numeric', length = n_blocks * n_participants),
  rational_n_sold_gain_shares = vector(mode = 'numeric', length = n_blocks * n_participants),
  rational_sold_gain_last_period = vector(mode = 'numeric', length = n_blocks * n_participants)
  )

for (subj in sort(dat_all_wide$participant)) {
  cat('\rCalculating DE values for participant', subj)
  for (i_path in seq_len(n_blocks)) {

    # This only considers the periods in which they could invest
    this_dat <- filter(dat_main_task,
      participant == subj, global_path_id == i_path, investable)

    path_selector <- de_table$participant == subj & de_table$block == i_path
    sale_flag <- this_dat$hold > 0 & this_dat$transaction < 0 |
          this_dat$hold < 0 & this_dat$transaction > 0
    rational_sale_flag <- this_dat$rational_hold > 0 & this_dat$rational_trade < 0 |
          this_dat$rational_hold < 0 & this_dat$rational_trade > 0

    de_table[path_selector, 'n_gain_shares'] <- sum(abs(this_dat$hold[this_dat$returns > 0]))
    de_table[path_selector, 'n_loss_shares'] <- sum(abs(this_dat$hold[this_dat$returns < 0]))
    de_table[path_selector, 'n_sold_shares'] <- sum(pmin(abs(this_dat$hold[sale_flag]),
        abs(this_dat$transaction[sale_flag])))
    de_table[path_selector, 'n_sold_loss_shares'] <- sum(pmin(abs(this_dat$hold[
      sale_flag & this_dat$returns < 0]), abs(this_dat$transaction[
      sale_flag & this_dat$returns < 0])))
    de_table[path_selector, 'n_sold_gain_shares'] <- sum(pmin(abs(this_dat$hold[
      sale_flag & this_dat$returns > 0]), abs(this_dat$transaction[
      sale_flag & this_dat$returns > 0])))

    # Rational DE:
    de_table[path_selector, 'rational_n_gain_shares'] <- sum(
      abs(this_dat$rational_hold[this_dat$rational_returns > 0]))
    de_table[path_selector, 'rational_n_loss_shares'] <- sum(
      abs(this_dat$rational_hold[this_dat$rational_returns < 0]))
    de_table[path_selector, 'rational_n_sold_shares'] <- sum(
      pmin(abs(this_dat$rational_hold[rational_sale_flag]),
        abs(this_dat$transaction[rational_sale_flag])))
    de_table[path_selector, 'rational_n_sold_loss_shares'] <- sum(
      pmin(abs(this_dat$rational_hold[
      rational_sale_flag & this_dat$rational_returns < 0]), abs(this_dat$rational_trade[
      rational_sale_flag & this_dat$rational_returns < 0])))
    de_table[path_selector, 'rational_n_sold_gain_shares'] <- sum(
      pmin(abs(this_dat$rational_hold[
      rational_sale_flag & this_dat$rational_returns > 0]), abs(this_dat$rational_trade[
      rational_sale_flag & this_dat$rational_returns > 0])))

    # Filter it further down to get the WC style DE measure only counting the last decision:
    this_dat <- filter(this_dat, round_label == 'end_p2')
    sale_flag <- this_dat$hold > 0 & this_dat$transaction < 0 |
                        this_dat$hold < 0 & this_dat$transaction > 0
    rational_sale_flag <- this_dat$rational_hold > 0 & this_dat$rational_trade < 0 |
          this_dat$rational_hold < 0 & this_dat$rational_trade > 0

    de_table[path_selector, 'gain_last_period'] <- (this_dat$returns > 0) * abs(this_dat$hold)
    de_table[path_selector, 'loss_last_period'] <- (this_dat$returns < 0) * abs(this_dat$hold)
    de_table[path_selector, 'sales_last_period'] <- min(abs(this_dat$hold),
      abs(this_dat$transaction)) * sale_flag
    de_table[path_selector, 'sold_loss_last_period'] <- de_table[path_selector,
      'sales_last_period'] * (this_dat$returns < 0)
    de_table[path_selector, 'sold_gain_last_period'] <- de_table[path_selector,
      'sales_last_period'] * (this_dat$returns > 0)

    # Rational DE:
    de_table[path_selector, 'rational_gain_last_period'] <-
      (this_dat$rational_returns > 0) * abs(this_dat$rational_hold)
    de_table[path_selector, 'rational_loss_last_period'] <-
      (this_dat$rational_returns < 0) * abs(this_dat$rational_hold)
    de_table[path_selector, 'rational_sales_last_period'] <-
      min(abs(this_dat$rational_hold), abs(this_dat$rational_trade)) * rational_sale_flag
    de_table[path_selector, 'sold_loss_last_period'] <- de_table[path_selector,
      'rational_sales_last_period'] * (this_dat$rational_returns < 0)
    de_table[path_selector, 'sold_gain_last_period'] <- de_table[path_selector,
      'rational_sales_last_period'] * (this_dat$rational_returns > 0)
  }
}
cat('\n')

de_table <- de_table %>%
  group_by(participant, condition) %>%
  select(-block) %>%
  summarise_at(vars(everything()), sum, na.rm = TRUE)

# Calculate all the different DE measures
  de_table$plr <- de_table$n_sold_loss_shares / de_table$n_loss_shares
  de_table$pgr <- de_table$n_sold_gain_shares / de_table$n_gain_shares
  de_table$de  <- de_table$pgr - de_table$plr

  de_table$plr_last_period <-
    de_table$sold_loss_last_period / de_table$loss_last_period
  de_table$pgr_last_period <-
    de_table$sold_gain_last_period / de_table$gain_last_period
  de_table$de_last_period  <-
    de_table$pgr_last_period - de_table$plr_last_period

  # Rational DE measures:
  de_table$rational_plr <- de_table$rational_n_sold_loss_shares / de_table$rational_n_loss_shares
  de_table$rational_pgr <- de_table$rational_n_sold_gain_shares / de_table$rational_n_gain_shares
  de_table$rational_de  <- de_table$rational_pgr - de_table$rational_plr

  de_table$rational_plr_last_period <-
    de_table$rational_sold_loss_last_period / de_table$rational_loss_last_period
  de_table$rational_pgr_last_period <-
    de_table$rational_sold_gain_last_period / de_table$rational_gain_last_period
  de_table$rational_de_last_period  <-
    de_table$rational_pgr_last_period - de_table$rational_plr_last_period

# Saving the updated dataframe --------------------------------------------
write_delim(dat_main_task, file.path(data_path, data_file_name_long), delim = ';')
write_delim(dat_all_wide, file.path(data_path, data_file_name_wide), delim = ';')
write_delim(de_table, file.path(data_path, 'de_table.csv'), delim = ';')

# dat_main_task <- read_delim(file.path(data_path, data_file_name_long), delim = ';')
# dat_all_wide <- read_delim(file.path(data_path, data_file_name_wide), delim = ';')
# de_table <- read_delim(file.path(data_path, 'de_table.csv'), delim = ';')
