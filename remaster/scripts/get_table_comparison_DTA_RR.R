
## Load true migration rates used to run the simulations
## and compute the daily movement probability
true_df_migration <- readRDS('../input_files/migration_matrix_5_demes.rds') %>% 
  as_tibble() %>% 
  mutate(subgroup_1 = 0:4) %>% 
  pivot_longer(cols = - 'subgroup_1', values_to = 'migration_rate', names_to = 'subgroup_2', names_prefix = 'V') %>% 
  mutate(subgroup_2 = as.integer(as.numeric(subgroup_2) - 1)) %>% 
  mutate(daily_proba_migration = ifelse(subgroup_1 == subgroup_2, 0., 1. - exp(-migration_rate * 1))) %>% 
  group_by(subgroup_1) %>% 
  mutate(daily_proba_migration = ifelse(daily_proba_migration == 0., 1. - sum(daily_proba_migration), daily_proba_migration)) %>% 
  ungroup()

get_DTA_statistics <- function(file_path_biased_DTA, file_path_unbiased_DTA){
  df_DTA_biased <- read_csv(file_path_biased_DTA) %>% select(- `...1`) %>% 
    left_join(true_df_migration, by = c('origin' = 'subgroup_1', 'destination' = 'subgroup_2'))
  df_DTA_unbiased <- read_csv(file_path_unbiased_DTA) %>% select(- `...1`) %>% 
    left_join(true_df_migration, by = c('origin' = 'subgroup_1', 'destination' = 'subgroup_2'))
  df_combined_DTA <- bind_rows(df_DTA_biased %>% mutate(type = 'Biased'), df_DTA_unbiased %>% mutate(type = 'Unbiased'))
  
}

## Define file paths
file_path_biased_DTA_rep_1 <- '../results/migration_rate_tables/sym_dta/migration_rates_beast_rep-1-seq-density-01-biased.csv'
file_path_unbiased_DTA_rep_1 <- '../results/migration_rate_tables/sym_dta/migration_rates_beast_rep-1-seq-density-01-unbiased.csv'
file_path_biased_DTA_rep_2 <- '../results/migration_rate_tables/sym_dta/migration_rates_beast_rep-2-seq-density-01-biased.csv'
file_path_unbiased_DTA_rep_2 <- '../results/migration_rate_tables/sym_dta/migration_rates_beast_rep-2-seq-density-01-unbiased.csv'
file_path_biased_DTA_rep_1_larger_dataset <- '../results/migration_rate_tables/sym_dta/migration_rates_beast_rep-1-seq-density-05-biased.csv'
file_path_unbiased_DTA_rep_1_larger_dataset <- '../results/migration_rate_tables/sym_dta/migration_rates_beast_rep-1-seq-density-05-unbiased.csv'
file_path_biased_DTA_rep_2_larger_dataset <- '../results/migration_rate_tables/sym_dta/migration_rates_beast_rep-2-seq-density-05-biased.csv'
file_path_unbiased_DTA_rep_2_larger_dataset <- '../results/migration_rate_tables/sym_dta/migration_rates_beast_rep-2-seq-density-05-unbiased.csv'

## 
file_path_biased_RR_rep_1 <- '../results_remaster/5-demes-rep-1-seq-density-2-biased-pop-100k-higher-beta/'
file_path_unbiased_RR_rep_1 <- '../results_remaster/5-demes-rep-1-seq-density-2-unbiased-pop-100k-higher-beta/'
file_path_biased_RR_rep_2 <- '../results_remaster/5-demes-rep-1-seq-density-2-biased-pop-100k-higher-beta/'
file_path_unbiased_RR_rep_2 <- '../results_remaster/5-demes-rep-1-seq-density-2-unbiased-pop-100k-higher-beta/'
file_path_biased_RR_rep_1_smaller_dataset <- '../results_remaster/5-demes-rep-1-seq-density-0.5-biased-pop-100k-higher-beta/'
file_path_unbiased_RR_rep_1_smaller_dataset <- '../results_remaster/5-demes-rep-1-seq-density-0.5-unbiased-pop-100k-higher-beta/'
file_path_biased_RR_rep_2_smaller_dataset <- '../results_remaster/5-demes-rep-1-seq-density-0.5-biased-pop-100k-higher-beta/'
file_path_unbiased_RR_rep_2_smaller_dataset <- '../results_remaster/5-demes-rep-1-seq-density-0.5-unbiased-pop-100k-higher-beta/'

compare