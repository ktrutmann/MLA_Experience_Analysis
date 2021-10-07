library(tidyverse)

theme_set(theme_minimal())

# Average belief by gain/loss and condition:
dat_main_task %>%
  filter(round_label == 'end_p2') %>%
  mutate(returns = case_when(
    returns > 0 ~ 'Gain',
    returns < 0 ~ 'Loss',
    returns == 0 ~ 'None')) %>%
  group_by(returns, condition) %>%
  summarise(mean_belief = mean(belief)) %>%
  ggplot(aes(condition, mean_belief, fill = returns)) +
    geom_bar(stat = 'identity', position = 'dodge')


# Average investment by gain/loss and condition:
# TODO: (1) Needs to be adjusted for shorting!
dat_main_task %>%
  filter(round_label == 'end_p2') %>%
  mutate(returns = case_when(
    returns > 0 ~ 'Gain',
    returns < 0 ~ 'Loss',
    returns == 0 ~ 'None')) %>%
  group_by(returns, condition) %>%
  summarise(mean_invest = mean(hold_after_trade)) %>%
  ggplot(aes(condition, mean_invest, fill = returns)) +
    geom_bar(stat = 'identity', position = 'dodge')
