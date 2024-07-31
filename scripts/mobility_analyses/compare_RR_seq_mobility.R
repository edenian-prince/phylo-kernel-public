library(mgcv)
library(tidyverse)
library(vegan)
library(broom)
source('utils_comp_RR.R')

## Load relative risk of observing identical sequences between regions and counties
df_RR_counties <- readRDS('../results/RR_county/df_RR_county_0_mut_away.rds') %>% rename(RR_seq = RR) %>% ungroup()
df_RR_regions <- readRDS('../results/RR_region/df_RR_region_0_mut_away.rds') %>% rename(RR_seq = RR) %>% ungroup()

## Load relative risk of movements between regions and counties
df_RR_mobility_commute <- readRDS('../results/RR_mobility/RR_workflow_county_WA.rds') %>% rename(RR_workflow = RR)
df_RR_mobility_commute_region <- readRDS('../results/RR_mobility/RR_workflow_region_WA.rds') %>% rename(RR_workflow = RR)
df_RR_mobility_mobile_phone <- readRDS('../results/RR_mobility/RR_mobile_phone_county_WA.rds') %>% rename(RR_mobile_phone = RR)
df_RR_mobility_mobile_phone_region <- readRDS('../results/RR_mobility/RR_mobile_phone_region_WA.rds') %>% rename(RR_mobile_phone = RR)
df_distance <- readRDS('../data/maps/df_dist_county.rds')
df_distance_region <- readRDS('../data/maps/df_dist_region.rds')

df_RR_for_comparison_counties <- df_RR_counties %>% select(group_1, group_2, RR_seq) %>% 
  left_join(df_RR_mobility_commute %>% select(county_1, county_2, RR_workflow),
            by = c('group_1' = 'county_1', 'group_2' = 'county_2')) %>% 
  left_join(df_RR_mobility_mobile_phone %>% select(county_1, county_2, RR_mobile_phone),
            by = c('group_1' = 'county_1', 'group_2' = 'county_2')) %>% 
  left_join(df_distance, by = c('group_1' = 'county_1', 'group_2' = 'county_2')) %>% 
  mutate(log_RR_seq = log(RR_seq), log_RR_workflow = log(RR_workflow), log_RR_mobile_phone = log(RR_mobile_phone))

df_RR_for_comparison_regions <- df_RR_regions %>% select(group_1, group_2, RR_seq) %>% 
  left_join(df_RR_mobility_commute_region %>% select(region_1, region_2, RR_workflow),
            by = c('group_1' = 'region_1', 'group_2' = 'region_2')) %>% 
  left_join(df_RR_mobility_mobile_phone_region %>% select(region_1, region_2, RR_mobile_phone),
            by = c('group_1' = 'region_1', 'group_2' = 'region_2')) %>% 
  left_join(df_distance_region, by = c('group_1' = 'region_1', 'group_2' = 'region_2')) %>% 
  mutate(log_RR_seq = log(RR_seq), log_RR_workflow = log(RR_workflow), log_RR_mobile_phone = log(RR_mobile_phone))

## Correlation coefficients
df_RR_for_comparison_regions %>% filter(group_1 >= group_2, ) %>% 
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

df_RR_for_comparison_counties %>% filter(group_1 >= group_2) %>% 
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
            pval_spearman_distance = cor.test(RR_seq, distance_km, method = 'spearman')$p.value) %>% 
  View()

df_RR_for_comparison_counties %>% filter(group_1 >= group_2, RR_seq > 0.) %>% 
  summarise(cor_spearman_distance = cor(RR_seq, distance_km, method = 'spearman'),
            pval_spearman_distance = cor.test(RR_seq, distance_km, method = 'spearman')$p.value) %>% 
  View()


df_RR_for_comparison_counties %>% filter(group_1 >= group_2, RR_seq > 0., RR_workflow > 0.) %>% 
  summarise(cor_spearman_workflow = cor(RR_seq, RR_workflow, method = 'spearman'),
    pval_spearman_workflow = cor.test(RR_seq, RR_workflow, method = 'spearman')$p.value) %>% 
  View()

df_RR_for_comparison_counties %>% filter(group_1 >= group_2, RR_seq > 0., RR_mobile_phone > 0.) %>% 
  summarise(cor_spearman_mobile_phone = cor(RR_seq, RR_mobile_phone, method = 'spearman'),
            pval_spearman_mobile_phone = cor.test(RR_seq, RR_mobile_phone, method = 'spearman')$p.value) %>% 
  View()
