# TODO: Define whether we are talking about #shares sold or just #sale periods for the DE
# -> Odean counts the #shares sold!

require(tidyverse)

# Setup and load data ----------------------------------------------------
data_path <- file.path('..', '..', 'data', 'processed')
data_file_name_long <- 'all_participants_long_task_data.csv'
data_file_name_wide <- 'dat_all_wide.csv'
data_file_time <- file.path('..', '..', 'data', 'raw', 'testing',
  'TimeSpent (accessed 2020-05-03).csv')

dat_main_long <- read_delim(str_c(data_path, '//', data_file_name_long), delim = ';')
dat_all_wide <- read_delim(str_c(data_path, '//', data_file_name_wide), delim = ';')

# Constants
rounds_per_phase <- 4  # Make sure this matches the python one!

# Derivative Constants
n_blocks <- max(dat_main_long$global_path_id)
n_participants <- nrow(dat_all_wide)


# DE Measure --------------------------------------------------------------
# This tibble contains one row per price path
de_table <- tibble(participant = rep(unique(dat_main_long$participant),
                      each = n_blocks),
                  block = rep(seq_len(n_blocks), n_participants),
                  condition = dat_main_long$condition[
                              dat_main_long$i_round_in_path == 0],
                  n_losses = vector(mode = 'numeric',
                      length = n_blocks * n_participants),
                  loss_last_period = vector(mode = 'numeric',
                      length = n_blocks * n_participants),
                  
                  n_gains = vector(mode = 'numeric',
                      length = n_blocks * n_participants),
                  gain_last_period = vector(mode = 'numeric',
                      length = n_blocks * n_participants),
                  
                  n_sales = vector(mode = 'numeric',
                      length = n_blocks * n_participants),
                  sale_last_period = vector(mode = 'numeric',
                      length = n_blocks * n_participants),
                  
                  n_sold_losses = vector(mode = 'numeric',
                      length = n_blocks * n_participants),
                  sold_loss_last_period = vector(mode = 'numeric',
                      length = n_blocks * n_participants),
                  
                  n_sold_gains = vector(mode = 'numeric',
                      length = n_blocks * n_participants),
                  sold_gain_last_period = vector(mode = 'numeric',
                      length = n_blocks * n_participants)
                  )

for (subj in dat_all_wide$participant) {
  for (i_path in seq_len(n_blocks)) {

    # This only considers the periods in which they could invest
    this_dat <- filter(dat_main_long, participant == subj,
                                      global_path_id == i_path,
                                      investable)

    de_table[de_table$participant == subj & de_table$block == i_path,
      'n_gains'] <- sum(this_dat$returns > 0)

    de_table[de_table$participant == subj & de_table$block == i_path,
      'n_losses'] <- sum(this_dat$returns < 0)

    de_table[de_table$participant == subj & de_table$block == i_path,
      'n_sales'] <- sum(this_dat$hold > 0 & this_dat$transaction < 0 |
                        this_dat$hold < 0 & this_dat$transaction > 0)

    de_table[de_table$participant == subj & de_table$block == i_path,
      'n_sold_losses'] <- sum((this_dat$hold > 0 & this_dat$transaction < 0 |
                              this_dat$hold < 0 & this_dat$transaction > 0) &
                              this_dat$returns < 0)

    de_table[de_table$participant == subj & de_table$block == i_path,
      'n_sold_gains'] <- sum((this_dat$hold > 0 & this_dat$transaction < 0 |
                              this_dat$hold < 0 & this_dat$transaction > 0) &
                              this_dat$returns > 0)

    # Filter it further down to get the WC style DE measure only counting the last decision:
    this_dat <- filter(this_dat, i_round_in_path == rounds_per_phase * 2)

    de_table[de_table$participant == subj & de_table$block == i_path,
      'gain_last_period'] <- this_dat$returns > 0

    de_table[de_table$participant == subj & de_table$block == i_path,
      'loss_last_period'] <- this_dat$returns < 0

    de_table[de_table$participant == subj & de_table$block == i_path,
      'sales_last_period'] <- (this_dat$hold > 0 & this_dat$transaction < 0 |
                        this_dat$hold < 0 & this_dat$transaction > 0)

    de_table[de_table$participant == subj & de_table$block == i_path,
      'sold_loss_last_period'] <- ((this_dat$hold > 0 & this_dat$transaction < 0 |
                              this_dat$hold < 0 & this_dat$transaction > 0) &
                              this_dat$returns < 0)

    de_table[de_table$participant == subj & de_table$block == i_path,
      'sold_gain_last_period'] <- ((this_dat$hold > 0 & this_dat$transaction < 0 |
                              this_dat$hold < 0 & this_dat$transaction > 0) &
                              this_dat$returns > 0)
  }
}

de_table <- de_table %>%
  group_by(participant) %>%
  summarise_at(vars(-condition), sum, na.rm = TRUE)

# Calculate all the different DE measures
  de_table$plr <- de_table$n_sold_losses / de_table$n_losses
  de_table$pgr <- de_table$n_sold_gains / de_table$n_gains
  de_table$de  <- de_table$pgr - de_table$plr

  de_table$plr_last_period <- de_table$sold_loss_last_period /
    de_table$loss_last_period
  de_table$pgr_last_period <- de_table$sold_gain_last_period /
    de_table$gain_last_period
  de_table$de_last_period  <- de_table$pgr_last_period -
    de_table$plr_last_period

dat_all_wide <- left_join(dat_all_wide, select(de_table, - block), by = 'participant')


# Holding and beliefs at the "critical points": ----------------------------------------
# Holding after first decision:
# This filters for + 1 because we want the hold value of the next period
first_decision <- dat_main_long %>%
  filter(i_round_in_path == rounds_per_phase + 1) %>%
  group_by(participant, condition) %>%
  summarise(hold = mean(hold)) %>%
  ungroup()

first_decision <- dat_main_long %>%
  filter(i_round_in_path == rounds_per_phase) %>%
  group_by(participant, condition) %>%
  summarise(belief = mean(belief)) %>%
  ungroup() %>%
  left_join(first_decision, by = c('participant', 'condition'))

last_decision <- dat_main_long %>%
  filter(i_round_in_path == rounds_per_phase * 2 + 1) %>%
  group_by(participant, condition) %>%
  summarise(hold = mean(hold)) %>%
  ungroup()

last_decision <- dat_main_long %>%
  filter(i_round_in_path == rounds_per_phase * 2) %>%
  group_by(participant, condition) %>%
  summarise(belief = mean(belief)) %>%
  ungroup() %>%
  left_join(last_decision, by = c('participant', 'condition'))

for (i in seq_len(nrow(first_decision))) {
  first_temp  <- slice(first_decision, i)
  last_temp <- slice(last_decision, i)

  dat_all_wide[dat_all_wide$participant == first_temp$participant,
               str_c('first_hold_',
               str_replace(first_temp$condition, ' ', '_'))] <-
               first_temp$hold

  dat_all_wide[dat_all_wide$participant == first_temp$participant,
               str_c('first_belief_',
               str_replace(first_temp$condition, ' ', '_'))] <-
               first_temp$belief

  dat_all_wide[dat_all_wide$participant == last_temp$participant,
               str_c('last_hold_',
               str_replace(first_temp$condition, ' ', '_'))] <-
               last_temp$hold
               
  dat_all_wide[dat_all_wide$participant == last_temp$participant,
               str_c('last_belief_',
               str_replace(first_temp$condition, ' ', '_'))] <-
               last_temp$belief
}
rm(first_decision)
rm(last_decision)


# Mean beliefs correlation with drift: -----------------------------------------
mean_belief_table <- dat_main_long %>%
  filter(investable) %>%
  group_by(participant, global_path_id) %>%
  summarise(drift = mean(drift),
            mean_belief = mean(belief, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(participant) %>%
  summarise(cor_belief_drift = cor(drift, mean_belief),
            cor_belief_drift_p = cor.test(drift, mean_belief)$p.value)

dat_all_wide <- left_join(dat_all_wide, mean_belief_table, by = 'participant')
rm(mean_belief_table)

# Time spent on the experiment --------------------------------------------
# dat_time <- read_csv(data_file_time)
# dat_time <- dat_time %>%
#   group_by(participant__code) %>%
#   summarise(experiment_time_in_minutes = sum(seconds_on_page) / 60) %>%
#   rename(participant = participant__code)

# dat_all_wide <- left_join(dat_all_wide, dat_time, by = 'participant')
# rm(dat_time)


# Saving the updated dataframe --------------------------------------------
write_delim(dat_all_wide, file.path(data_path, data_file_name_wide), delim = ';')