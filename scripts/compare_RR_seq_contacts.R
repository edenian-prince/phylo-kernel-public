library(tidyverse)
library(vegan)
source('utils_comp_RR.R')

## Load relative risk of observing identical sequences between ahe groups
df_RR_age <- readRDS('../results/RR_age/df_RR_age_0_mut_away.rds') %>% rename(RR_seq = RR) %>% ungroup()

## Load results from subsampling to compute uncertainty intervals
df_uncertainty_age <- readRDS('../results/RR_age/df_RR_uncertainty_age_0_mut_away.rds') %>% 
  group_by(n_mutations, group_1, group_2) %>% 
  summarise(median_RR = median(RR), 
            lower_RR = quantile(RR, 0.025), 
            upper_RR = quantile(RR, 0.975)) %>% 
  ungroup()

## Load relative risk of contacts between two age groups
df_RR_contacts <- readRDS( '../results/RR_contacts/df_RR_overall.rds') %>% 
  select(age_decade_i, age_decade_j, RR_contacts)

df_RR_for_comparison <- df_RR_age %>% 
  left_join(df_RR_contacts, by = c('group_1' = 'age_decade_i', 'group_2' = 'age_decade_j')) %>% 
  left_join(df_uncertainty_age %>% select(- n_mutations), by = c('group_1', 'group_2'))

## Mantel test for comparison (this does not account for the diagonal)
mat_RR_seq <- get_mat_RR_from_df_RR(df_RR_for_comparison, 'RR_seq', 'group')
mat_RR_contacts <- get_mat_RR_from_df_RR(df_RR_for_comparison, 'RR_contacts', 'group')

get_mantel_comparison_from_mat_RR(mat_RR_seq, mat_RR_contacts, n_permut = 1e5 - 1, 'spearman')
get_mantel_comparison_from_mat_RR(mat_RR_seq, mat_RR_contacts, n_permut = 1e5 - 1, 'pearson')

## Correlation coefficients
df_RR_for_comparison %>% filter(group_1 >= group_2) %>% 
  summarise(cor_pearson = cor(RR_seq, RR_contacts, method = 'pearson'),
            pval_pearson = cor.test(RR_seq, RR_contacts, method = 'pearson')$p.value,
            cor_pearson_log = cor(log(RR_seq), log(RR_contacts), method = 'pearson'),
            pval_pearson_log = cor.test(log(RR_seq), log(RR_contacts), method = 'pearson')$p.value,
            cor_spearman = cor(RR_seq, RR_contacts, method = 'spearman'),
            pval_spearman = cor.test(RR_seq, RR_contacts, method = 'spearman')$p.value)

cor.test(as.numeric(unlist(df_RR_for_comparison %>% filter(group_1 >= group_2) %>% select(RR_seq))),
         as.numeric(unlist(df_RR_for_comparison %>% filter(group_1 >= group_2) %>% select(RR_contacts))),
         method = 'spearman')
