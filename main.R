# source(file.path('prep', '1_data_cleaning.R'))
# source(file.path('prep', '2_create_vars.R'))
source(file.path('prep', '3_codebook.R'))

source(file.path('stats', '0_stats_setup.R'))
source(file.path('stats', 'descriptives.R'))
source(file.path('stats', 'inference_beliefs.R'))
source(file.path('stats', 'inference_trades.R'))

saveRDS(master_list, file.path('output', 'master_list.RDS'))