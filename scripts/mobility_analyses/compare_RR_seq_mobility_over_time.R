## This script commpute the correlation between the RR of observing identical sequences between regions 
## and the relative risk of movement (from mobile phone and commuting data) stratifying by pandemic waves in WA

library(mgcv)
library(tidyverse)
library(vegan)
library(broom)
source('../utils_comp_RR.R')

## Load relative risk of observing identical sequences between regions and counties
vec_periods <- 1:4
df_RR_regions_by_period <- Reduce('bind_rows', lapply(vec_periods, FUN = function(i_period){
  readRDS(paste0('../results/RR_region_by_period/df_RR_region_0_mut_away_period_', i_period, '.rds')) %>% 
    rename(RR_seq = RR) %>% ungroup() %>% 
    mutate(i_period = i_period)
}))
  
## Load relative risk of movements between regions and counties
df_RR_mobility_commute_region <- readRDS('../results/RR_mobility/RR_workflow_region_WA.rds') %>% rename(RR_workflow = RR)
df_RR_mobility_mobile_phone_region_by_period <- Reduce('bind_rows', lapply(vec_periods, FUN = function(i_period){
  readRDS(paste0('../results/RR_mobility/RR_mobile_phone_region_WA_period_', i_period, '.rds')) %>% 
    rename(RR_mobile_phone = RR) %>% ungroup() %>% 
    mutate(i_period = i_period)
}))
df_distance_region <- readRDS('../data/maps/df_dist_region.rds')

df_RR_for_comparison_regions <- df_RR_regions_by_period %>% select(group_1, group_2, RR_seq, i_period) %>% 
  left_join(df_RR_mobility_commute_region %>% select(region_1, region_2, RR_workflow),
            by = c('group_1' = 'region_1', 'group_2' = 'region_2')) %>% 
  left_join(df_RR_mobility_mobile_phone_region_by_period %>% select(region_1, region_2, RR_mobile_phone, i_period),
            by = c('group_1' = 'region_1', 'group_2' = 'region_2', 'i_period')) %>% 
  left_join(df_distance_region, by = c('group_1' = 'region_1', 'group_2' = 'region_2')) %>% 
  mutate(log_RR_seq = log(RR_seq), log_RR_workflow = log(RR_workflow), log_RR_mobile_phone = log(RR_mobile_phone))



## Correlation coefficients
df_RR_for_comparison_regions %>% filter(group_1 >= group_2) %>% 
  group_by(i_period) %>% 
  summarise(#cor_pearson_workflow = cor(RR_seq, RR_workflow, method = 'pearson'),
            #pval_pearson_workflow = cor.test(RR_seq, RR_workflow, method = 'pearson')$p.value,
            cor_spearman_workflow = cor(RR_seq, RR_workflow, method = 'spearman'),
            pval_spearman_workflow = cor.test(RR_seq, RR_workflow, method = 'spearman')$p.value,
            #cor_pearson_mobile_phone = cor(RR_seq, RR_mobile_phone, method = 'pearson'),
            #pval_pearson_mobile_phone = cor.test(RR_seq, RR_mobile_phone, method = 'pearson')$p.value,
            cor_spearman_mobile_phone = cor(RR_seq, RR_mobile_phone, method = 'spearman'),
            pval_spearman_mobile_phone = cor.test(RR_seq, RR_mobile_phone, method = 'spearman')$p.value,
            #cor_pearson_distance = cor(RR_seq, distance_km, method = 'pearson'),
            #pval_pearson_distance = cor.test(RR_seq, distance_km, method = 'pearson')$p.value,
            cor_spearman_distance = cor(RR_seq, distance_km, method = 'spearman'),
            pval_spearman_distance = cor.test(RR_seq, distance_km, method = 'spearman')$p.value
            ) %>% 
  View()


df_RR_for_comparison_regions %>% 
  ggplot(aes(x = RR_mobile_phone, y = RR_seq)) +
  geom_point() +
  scale_y_continuous(trans = 'log') +
  scale_x_continuous(trans = 'log') +
  facet_wrap(. ~ i_period, scales = 'free_y')
