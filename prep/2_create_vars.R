# The purpose of this script is to generate additional variables
# which will be used in multiple analyses so that they don't
# have to be recreated each time.

require(tidyverse)

# Setup and load data ----------------------------------------------------
data_path <- file.path('..', 'data', 'processed')
data_file_name_long <- 'main_task_data.csv'
data_file_name_wide <- 'dat_all_wide.csv'
data_file_time <- file.path('..', '..', 'data', 'raw', 'testing',
  'TimeSpent (accessed 2020-05-03).csv')

dat_main_task <- read_delim(
  file.path(data_path, data_file_name_long), delim = ';')
dat_all_wide <- read_delim(
  file.path(data_path, data_file_name_wide), delim = ';')

# Constants
rounds_per_phase <- 4  # Make sure this matches the python one!

# Derivative Constants
n_blocks <- max(dat_main_task$global_path_id)
n_participants <- nrow(dat_all_wide)


# General additional variables:
dat_main_long <- mutate(dat_main_long,
	hold_drift_flipped = ifelse(drift > .5, hold, -hold),
	belief_drift_flipped = ifelse(drift > .5, belief, 100 - belief))

# TODO: (3) Create a variable with some key indicators for a rational trader


# DE Measure --------------------------------------------------------------
# This tibble contains one row per price path
# TODO: (2) Define whether we are talking about #shares sold or just #sale periods for the DE
# -> Odean counts the #shares sold!
de_table <- tibble(
  participant = rep(unique(dat_main_task$participant), each = n_blocks),
  block = rep(seq_len(n_blocks), n_participants),
  condition = dat_main_task$condition[dat_main_task$i_round_in_path == 0],
  n_losses = vector(mode = 'numeric', length = n_blocks * n_participants),
  loss_last_period = vector(mode = 'numeric', length = n_blocks * n_participants),
  n_gains = vector(mode = 'numeric', length = n_blocks * n_participants),
  gain_last_period = vector(mode = 'numeric', length = n_blocks * n_participants),
  n_sales = vector(mode = 'numeric', length = n_blocks * n_participants),
  sale_last_period = vector(mode = 'numeric', length = n_blocks * n_participants),
  n_sold_losses = vector(mode = 'numeric', length = n_blocks * n_participants),
  sold_loss_last_period = vector(mode = 'numeric', length = n_blocks * n_participants),
  n_sold_gains = vector(mode = 'numeric', length = n_blocks * n_participants),
  sold_gain_last_period = vector(mode = 'numeric', length = n_blocks * n_participants)
  )

for (subj in dat_all_wide$participant) {
  for (i_path in seq_len(n_blocks)) {

    # This only considers the periods in which they could invest
    this_dat <- filter(dat_main_task,
      participant == subj, global_path_id == i_path, investable)

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

  de_table$plr_last_period <-
    de_table$sold_loss_last_period / de_table$loss_last_period
  de_table$pgr_last_period <-
    de_table$sold_gain_last_period / de_table$gain_last_period
  de_table$de_last_period  <-
    de_table$pgr_last_period - de_table$plr_last_period

dat_all_wide <- left_join(dat_all_wide,
  select(de_table, - block), by = 'participant')


# Saving the updated dataframe --------------------------------------------
write_delim(dat_all_wide, file.path(data_path, data_file_name_wide), delim = ';')