library(tidyverse)
library(xtable)

## Load mean sequencing probability
p_seq_per_group <- readRDS('../input_files/p_sample_per_group_5_demes.rds')
average_sequencing_proba <- mean(p_seq_per_group)

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

## Define file paths for DTA
file_path_biased_DTA_rep_1 <- '../results/migration_rate_tables/sym_dta/migration_rates_beast_rep-1-seq-density-01-biased.csv'
file_path_unbiased_DTA_rep_1 <- '../results/migration_rate_tables/sym_dta/migration_rates_beast_rep-1-seq-density-01-unbiased.csv'
file_path_metadata_biased_DTA_rep_1 <- '../results_remaster/5-demes-rep-1-seq-density-0.1-biased-pop-100k-higher-beta/metadata.csv'
file_path_metadata_unbiased_DTA_rep_1 <- '../results_remaster/5-demes-rep-1-seq-density-0.1-unbiased-pop-100k-higher-beta/metadata.csv'

file_path_biased_DTA_rep_2 <- '../results/migration_rate_tables/sym_dta/migration_rates_beast_rep-2-seq-density-01-biased.csv'
file_path_unbiased_DTA_rep_2 <- '../results/migration_rate_tables/sym_dta/migration_rates_beast_rep-2-seq-density-01-unbiased.csv'
file_path_metadata_biased_DTA_rep_2 <- '../results_remaster/5-demes-rep-2-seq-density-0.1-biased-pop-100k-higher-beta/metadata.csv'
file_path_metadata_unbiased_DTA_rep_2 <- '../results_remaster/5-demes-rep-2-seq-density-0.1-unbiased-pop-100k-higher-beta/metadata.csv'

file_path_biased_DTA_rep_1_larger_dataset <- '../results/migration_rate_tables/sym_dta/migration_rates_beast_rep-1-seq-density-05-biased.csv'
file_path_unbiased_DTA_rep_1_larger_dataset <- '../results/migration_rate_tables/sym_dta/migration_rates_beast_rep-1-seq-density-05-unbiased.csv'
file_path_metadata_biased_DTA_rep_1_larger_dataset <- '../results_remaster/5-demes-rep-1-seq-density-0.5-biased-pop-100k-higher-beta/metadata.csv'
file_path_metadata_unbiased_DTA_rep_1_larger_dataset <- '../results_remaster/5-demes-rep-1-seq-density-0.5-unbiased-pop-100k-higher-beta/metadata.csv'

file_path_biased_DTA_rep_2_larger_dataset <- '../results/migration_rate_tables/sym_dta/migration_rates_beast_rep-2-seq-density-05-biased.csv'
file_path_unbiased_DTA_rep_2_larger_dataset <- '../results/migration_rate_tables/sym_dta/migration_rates_beast_rep-2-seq-density-05-unbiased.csv'
file_path_metadata_biased_DTA_rep_2_larger_dataset <- '../results_remaster/5-demes-rep-2-seq-density-0.5-biased-pop-100k-higher-beta/metadata.csv'
file_path_metadata_unbiased_DTA_rep_2_larger_dataset <- '../results_remaster/5-demes-rep-2-seq-density-0.5-unbiased-pop-100k-higher-beta/metadata.csv'


## Define file paths for RR
folder_path_biased_RR_rep_1 <- '../results_remaster/5-demes-rep-1-seq-density-2-biased-pop-100k-higher-beta/'
folder_path_unbiased_RR_rep_1 <- '../results_remaster/5-demes-rep-1-seq-density-2-unbiased-pop-100k-higher-beta/'
folder_path_biased_RR_rep_2 <- '../results_remaster/5-demes-rep-2-seq-density-2-biased-pop-100k-higher-beta/'
folder_path_unbiased_RR_rep_2 <- '../results_remaster/5-demes-rep-2-seq-density-2-unbiased-pop-100k-higher-beta/'
folder_path_biased_RR_rep_1_smaller_dataset <- '../results_remaster/5-demes-rep-1-seq-density-0.5-biased-pop-100k-higher-beta/'
folder_path_unbiased_RR_rep_1_smaller_dataset <- '../results_remaster/5-demes-rep-1-seq-density-0.5-unbiased-pop-100k-higher-beta/'
folder_path_biased_RR_rep_2_smaller_dataset <- '../results_remaster/5-demes-rep-2-seq-density-0.5-biased-pop-100k-higher-beta/'
folder_path_unbiased_RR_rep_2_smaller_dataset <- '../results_remaster/5-demes-rep-2-seq-density-0.5-unbiased-pop-100k-higher-beta/'


## Get summary statistics tables
get_DTA_statistics <- function(file_path_biased_DTA, file_path_unbiased_DTA, 
                               file_path_metadata_biased, file_path_metadata_unbiased, 
                               sequencing_density, replicate_id){
  
  # Load dataframes of DTA results
  df_DTA_biased <- read_csv(file_path_biased_DTA) %>% select(- `...1`) %>% 
    left_join(true_df_migration, by = c('origin' = 'subgroup_1', 'destination' = 'subgroup_2')) %>% 
    arrange(origin, destination)
  df_DTA_unbiased <- read_csv(file_path_unbiased_DTA) %>% select(- `...1`) %>% 
    left_join(true_df_migration, by = c('origin' = 'subgroup_1', 'destination' = 'subgroup_2')) %>% 
    arrange(origin, destination)
  df_combined_DTA <- bind_rows(df_DTA_biased %>% mutate(type = 'Biased'), df_DTA_unbiased %>% mutate(type = 'Unbiased'))
  
  # Load metadata files to get the dataset sizes
  n_seq_biased <- read_csv(file_path_metadata_biased) %>% select(- ...1) %>% nrow()
  n_seq_unbiased <- read_csv(file_path_metadata_unbiased) %>% select(- ...1) %>% nrow()
  
  cor_DTA_migration_rates_biased <- cor(df_DTA_biased$instant_rates, df_DTA_biased$migration_rate, method = 'spearman')
  cor_DTA_migration_rates_unbiased <- cor(df_DTA_unbiased$instant_rates, df_DTA_unbiased$migration_rate, method = 'spearman')
  cor_DTA_biased_unbiased <- df_DTA_biased %>% select(origin, destination, instant_rates) %>% rename(instant_rates_biased = instant_rates) %>% 
    left_join(df_DTA_unbiased %>% select(origin, destination, instant_rates) %>% rename(instant_rates_unbiased = instant_rates)) %>% 
    summarise(cor = cor(instant_rates_biased, instant_rates_unbiased, method = 'spearman')) %>% unlist() %>% as.numeric()
  
  tibble(`Method` = 'DTA (Fix tree)',
         `Average sequencing probability` = paste0(round(average_sequencing_proba *sequencing_density * 100, 2), ' %'),
         `Replicate` = replicate_id,
         `Sample size unbiased` = n_seq_unbiased,
         `Sample size biased` = n_seq_biased,
         `Corr. with sim (unbiased)` = round(cor_DTA_migration_rates_unbiased, 2),
         `Corr. with sim (biased)` = round(cor_DTA_migration_rates_biased, 2),
         `Corr. biased / unbiased` = round(cor_DTA_biased_unbiased, 2)) 
}

get_RR_statistics <- function(folder_path_biased_RR, folder_path_unbiased_RR, sequencing_density, replicate_id){
  df_RR_biased <- readRDS(paste0(folder_path_biased_RR,  'df_RR.rds')) %>% 
    left_join(true_df_migration) %>% 
    filter(subgroup_1 >= subgroup_2)
  df_RR_unbiased <- readRDS(paste0(folder_path_unbiased_RR, 'df_RR.rds')) %>% 
    left_join(true_df_migration) %>% 
    filter(subgroup_1 >= subgroup_2)
  
  n_seq_biased <- read_csv(paste0(folder_path_biased_RR, 'metadata.csv')) %>% select(- ...1) %>% nrow()
  n_seq_unbiased <- read_csv(paste0(folder_path_unbiased_RR, 'metadata.csv')) %>% select(- ...1) %>% nrow()
  
  cor_RR_proba_migration_biased <- cor(df_RR_biased$log_RR, df_RR_biased$daily_proba_migration, method = 'spearman')
  cor_RR_proba_migration_unbiased <- cor(df_RR_unbiased$log_RR, df_RR_unbiased$daily_proba_migration, method = 'spearman')
  cor_RR_biased_unbiased <- df_RR_biased %>% select(subgroup_1, subgroup_2, log_RR) %>% rename(log_RR_biased = log_RR) %>% 
    left_join(df_RR_unbiased %>% select(subgroup_1, subgroup_2, log_RR) %>% rename(log_RR_unbiased = log_RR)) %>% 
    summarise(cor = cor(log_RR_biased, log_RR_unbiased, method = 'spearman')) %>% unlist() %>% as.numeric()
  
  tibble(`Method` = 'RR',
         `Average sequencing probability` = paste0(round(average_sequencing_proba * sequencing_density * 100, 2), ' %'),
         `Replicate` = replicate_id,
         `Sample size unbiased` = n_seq_unbiased,
         `Sample size biased` = n_seq_biased,
         `Corr. with sim (unbiased)` = round(cor_RR_proba_migration_unbiased, 2),
         `Corr. with sim (biased)` = round(cor_RR_proba_migration_biased, 2),
         `Corr. biased / unbiased` = round(cor_RR_biased_unbiased, 2)) 
}


df_for_results <- bind_rows(get_DTA_statistics(file_path_biased_DTA_rep_1, file_path_unbiased_DTA_rep_1, 
                                               file_path_metadata_biased_DTA_rep_1, file_path_metadata_unbiased_DTA_rep_1, 
                                               0.1, 1),
                            get_DTA_statistics(file_path_biased_DTA_rep_2, file_path_unbiased_DTA_rep_2, 
                                               file_path_metadata_biased_DTA_rep_2, file_path_metadata_unbiased_DTA_rep_2, 
                                               0.1, 2),
                            get_DTA_statistics(file_path_biased_DTA_rep_1_larger_dataset, file_path_unbiased_DTA_rep_1_larger_dataset, 
                                               file_path_metadata_biased_DTA_rep_1_larger_dataset, file_path_metadata_unbiased_DTA_rep_1_larger_dataset, 
                                               0.5, 1),
                            get_DTA_statistics(file_path_biased_DTA_rep_2_larger_dataset, file_path_unbiased_DTA_rep_2_larger_dataset, 
                                               file_path_metadata_biased_DTA_rep_2_larger_dataset, file_path_metadata_unbiased_DTA_rep_2_larger_dataset, 
                                               0.5, 2),
                            get_RR_statistics(folder_path_biased_RR_rep_1, 
                                              folder_path_unbiased_RR_rep_1, 
                                              2, 1),
                            get_RR_statistics(folder_path_biased_RR_rep_2, 
                                              folder_path_unbiased_RR_rep_2, 
                                              2, 2),
                            get_RR_statistics(folder_path_biased_RR_rep_1_smaller_dataset, 
                                              folder_path_unbiased_RR_rep_1_smaller_dataset, 
                                              0.5, 1),
                            get_RR_statistics(folder_path_biased_RR_rep_2_smaller_dataset, 
                                              folder_path_unbiased_RR_rep_2_smaller_dataset, 
                                              0.5, 2))



df_for_results %>% 
  xtable() %>% 
  print(include.rownames = FALSE)


