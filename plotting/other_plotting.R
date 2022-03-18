# Movement of the majority_updated_in_p2 as Sankey plot:
plot_prep <- dat_main_task %>%
  filter(i_round_in_path == 0, condition != 'Blocked Info') %>%
  mutate(condition = recode(condition, `Blocked Trades` = 'Blocked_Trades')) %>%
  select(participant, majority_updates_p2, condition, distinct_path_id) %>%
  pivot_wider(id_cols = c('participant', 'distinct_path_id'),
    names_from = condition, values_from = majority_updates_p2) %>%
  mutate(update_pos_movement = str_c(Baseline, Blocked_Trades, sep = ' / ')) %>%
  count(update_pos_movement) %>%
  mutate(source = str_c(unlist(lapply(str_split(update_pos_movement, pattern = ' / '), function(x) x[1])), ' BL'),
    target = str_c(unlist(lapply(str_split(update_pos_movement, pattern = ' / '), function(x) x[2])), ' BT'),
    id_source = match(source, unique(c(source, target))) - 1,
    id_target = match(target, unique(c(source, target))) - 1) %>%
  as.data.frame()

plot_prep2 <- data.frame(name = unique(c(plot_prep$source, plot_prep$target)))

sankeyNetwork(Links = plot_prep, Nodes = plot_prep2,
  Source = 'id_source', Target = 'id_target',
  Value = 'n', NodeID = 'name',
  fontSize = 14)
