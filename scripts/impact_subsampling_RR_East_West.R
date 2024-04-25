library(tidyverse)
library(ggpubr)

col_west <- 'dodgerblue3'
col_east <- 'coral1'
col_east_west <- 'darkslateblue'

## Load adjacency matrix
df_adj_county <- readRDS('../data/maps/df_adj_county.rds')
df_adj_zcta <- readRDS('../data/maps/df_adj_zcta.rds') %>% 
  as_tibble() %>% 
  mutate(zcta_1 = as.character(zcta_1), zcta_2 = as.character(zcta_2))

## Load characteristics of WA counties
df_char_counties <- read.csv('../data/maps/county_wa.csv') %>% as_tibble()
df_char_zctas <- read_csv('../data/maps/relationship_zcta_county_WA.csv', col_types = 'cc') %>% 
  left_join(df_char_counties %>% select(county, is_west), by = 'county')

## Load the relative risk of observing identical sequences between two counties
df_RR_counties <- readRDS('../results/RR_county/df_RR_county_0_mut_away.rds') %>% 
  left_join(df_adj_county, by = c('group_1' = 'county_1', 'group_2' = 'county_2')) %>% 
  mutate(is_same_county = (group_1 == group_2)) %>% 
  left_join(df_char_counties %>% select(county, is_west), by = c('group_1' = 'county')) %>% 
  rename(is_west_1 = is_west) %>% 
  left_join(df_char_counties %>% select(county, is_west), by = c('group_2' = 'county')) %>% 
  rename(is_west_2 = is_west) %>% 
  mutate(region = case_when(is_west_1 + is_west_2 == 2 ~ 'West - West',
                            is_west_1 + is_west_2 == 1 ~ 'West - East',
                            is_west_1 + is_west_2 == 0 ~ 'East - East'),
         region = factor(region, levels = c('West - West', 'West - East', 'East - East'))) %>% 
  mutate(is_adjacent_same_county = case_when(is_same_county == T ~ 'Same county',
                                             is_adjacent == T ~ 'Adjacent counties',
                                             TRUE ~ 'Non adjacent counties'),
         is_adjacent_same_county = factor(is_adjacent_same_county, levels = c('Non adjacent counties', 'Adjacent counties', 'Same county')))

df_RR_zcta <- readRDS('../results/RR_zcta/df_RR_zcta_0_mut_away.rds') %>%
  rename(group_1 = zcta_1, group_2 = zcta_2) %>% as_tibble() %>% 
  mutate(is_same_zcta = (group_1 == group_2)) %>% 
  left_join(df_adj_zcta, by = c('group_1' = 'zcta_1', 'group_2' = 'zcta_2')) %>% 
  left_join(df_char_zctas %>% select(county, zcta, is_west), by = c('group_1' = 'zcta')) %>% 
  rename(county_1 = county, is_west_1 = is_west) %>% 
  left_join(df_char_zctas %>% select(county, zcta, is_west), by = c('group_2' = 'zcta')) %>% 
  rename(county_2 = county, is_west_2 = is_west) %>% 
  mutate(is_west_1 = as.numeric(is_west_1), is_west_2 = as.numeric(is_west_2)) %>% 
  mutate(region = case_when(is_west_1 + is_west_2 == 2 ~ 'West - West',
                            is_west_1 + is_west_2 == 1 ~ 'West - East',
                            is_west_1 + is_west_2 == 0 ~ 'East - East'),
         region = factor(region, levels = c('West - West', 'West - East', 'East - East')),
         is_adjacent_same_zcta = case_when(is_same_zcta == T ~ 'Same ZCTA',
                                           is_adjacent == T ~ 'Adjacent ZCTAs',
                                           TRUE ~ 'Non adjacent ZCTAs'),
         is_adjacent_same_zcta = factor(is_adjacent_same_zcta, levels = c('Non adjacent ZCTAs', 'Adjacent ZCTAs', 'Same ZCTA'))) 


########## County-level analysis
## Evaluate whether the fact that we do not observe any difference in the RR between 
## adjacent and non adjacent counties in Western and Eastern WA can be explained by 
## the fact that we are comparing a different number of groups.

## Procedure: Subsample a fraction of pairs of adjacent and non-adjacent counties
## for W/W, W/E and E/E to systematically compare the same number of pairs.
## Then, we perform a Wilcoxon test and report p-values.

## Dataframe with the number of pairs available in adjacent and non-adjacent counties
## by Eastern / Western WA status
df_pairs_by_counties <- df_RR_counties %>% 
  filter(group_1 > group_2) %>% 
  group_by(region, is_adjacent_same_county) %>% 
  summarise(n_pairs = n()) %>% ungroup()

## Lowest number of pairs of adjacent counties across regions
min_n_pairs_adjacent <- min(df_pairs_by_counties$n_pairs[df_pairs_by_counties$is_adjacent_same_county == 'Adjacent counties'])
## Lowest number of pairs of non adjacent counties across regions
min_n_pairs_non_adjacent <- min(df_pairs_by_counties$n_pairs[df_pairs_by_counties$is_adjacent_same_county == 'Non adjacent counties'])

## Vector from which to subsample
vec_RR_counties_WE_adj <- df_RR_counties[df_RR_counties$region == 'West - East' & df_RR_counties$is_adjacent_same_county == 'Adjacent counties' & (df_RR_counties$group_1 > df_RR_counties$group_2), ]$RR
vec_RR_counties_WE_non_adj <- df_RR_counties[df_RR_counties$region == 'West - East' & df_RR_counties$is_adjacent_same_county == 'Non adjacent counties' & (df_RR_counties$group_1 > df_RR_counties$group_2), ]$RR
vec_RR_counties_west_adj <- df_RR_counties[df_RR_counties$region == 'West - West' & df_RR_counties$is_adjacent_same_county == 'Adjacent counties' & (df_RR_counties$group_1 > df_RR_counties$group_2), ]$RR
vec_RR_counties_east_adj <- df_RR_counties[df_RR_counties$region == 'East - East' & df_RR_counties$is_adjacent_same_county == 'Adjacent counties' & (df_RR_counties$group_1 > df_RR_counties$group_2), ]$RR
vec_RR_counties_west_non_adj <- df_RR_counties[df_RR_counties$region == 'West - West' & df_RR_counties$is_adjacent_same_county == 'Non adjacent counties' & (df_RR_counties$group_1 > df_RR_counties$group_2), ]$RR
vec_RR_counties_east_non_adj <- df_RR_counties[df_RR_counties$region == 'East - East' & df_RR_counties$is_adjacent_same_county == 'Non adjacent counties' & (df_RR_counties$group_1 > df_RR_counties$group_2), ]$RR

## Perform subsampling
n_rep <- 1000
set.seed(2000)
df_sim <- Reduce('bind_rows', lapply(1:n_rep, FUN = function(i_rep){
  subsample_RR_west_adj <- sample(vec_RR_counties_west_adj, size = min_n_pairs_adjacent, replace = F)
  subsample_RR_east_adj <- sample(vec_RR_counties_east_adj, size = min_n_pairs_adjacent, replace = F)
  subsample_RR_WE_adj <-  sample(vec_RR_counties_WE_adj, size = min_n_pairs_adjacent, replace = F)
  
  subsample_RR_west_non_adj <-sample(vec_RR_counties_west_non_adj, size = min_n_pairs_non_adjacent, replace = F)
  subsample_RR_east_non_adj <- sample(vec_RR_counties_east_non_adj, size = min_n_pairs_non_adjacent, replace = F)
  subsample_RR_WE_non_adj <- sample(vec_RR_counties_WE_non_adj, size = min_n_pairs_non_adjacent, replace = F)
  
  c('p_val_west' = wilcox.test(x = subsample_RR_west_adj, y = subsample_RR_west_non_adj)$p.value,
    'p_val_east' = wilcox.test(x = subsample_RR_east_adj, y = subsample_RR_east_non_adj)$p.value,
    'p_val_WE' = wilcox.test(x = subsample_RR_WE_adj, y = subsample_RR_WE_non_adj)$p.value,
    'rep' = i_rep)
})) 

df_for_boxplot_p_val <-  df_sim %>% 
  summarise(y05_west = quantile(p_val_west, 0.05), y25_west = quantile(p_val_west, 0.25), y50_west = quantile(p_val_west, 0.5), y75_west = quantile(p_val_west, 0.75), y95_west = quantile(p_val_west, 0.95),
            y05_east = quantile(p_val_east, 0.05), y25_east = quantile(p_val_east, 0.25), y50_east = quantile(p_val_east, 0.5), y75_east = quantile(p_val_east, 0.75), y95_east = quantile(p_val_east, 0.95),
            y05_WE = quantile(p_val_WE, 0.05), y25_WE = quantile(p_val_WE, 0.25), y50_WE = quantile(p_val_WE, 0.5), y75_WE = quantile(p_val_WE, 0.75), y95_WE = quantile(p_val_WE, 0.95))

plt_impact_subsampling_wilcoxon <- df_sim %>% ggplot() +
  geom_jitter(aes(x = 'West - West', y = p_val_west), alpha = 0.08, col = 'darkgrey',
              width = 0.3, height = 0.) +
  geom_boxplot(data = df_for_boxplot_p_val, 
               aes(x = 'West - West', ymin = y05_west, lower = y25_west, middle = y50_west, upper = y75_west, ymax = y95_west),
               stat = "identity", fill = NA, width = 0.7, color = col_west) +
  geom_jitter(aes(x = 'West - East', y = p_val_WE), alpha = 0.08, col = 'darkgrey',
              width = 0.3, height = 0.) +
  geom_boxplot(data = df_for_boxplot_p_val, 
               aes(x = 'West - East',  ymin = y05_WE, lower = y25_WE, middle = y50_WE, upper = y75_WE, ymax = y95_WE),
               stat = "identity", fill = NA, width = 0.7, color = col_east_west) +
  geom_jitter(aes(x = 'East - East', y = p_val_east), alpha = 0.08, col = 'darkgrey',
              height = 0., width = 0.3) +
  geom_boxplot(data = df_for_boxplot_p_val, 
               aes(x = 'East - East',  ymin = y05_east, lower = y25_east, middle = y50_east, upper = y75_east, ymax = y95_east),
               stat = "identity", fill = NA, width = 0.7, color = col_east) +
  scale_x_discrete(name = '', limits = rev) +
  scale_y_continuous(trans = 'log10', 
                     name = 'p-value (Wilcoxon test for difference in RR\nbetween adjacent and non-adjacent counties)',
                     breaks = c(1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1.),
                     labels = c(expression(10^{-5}), 
                                expression(10^{-4}), expression(10^{-3}), 
                                expression(10^{-2}), expression(10^{-1}),
                                expression(10^{0}))) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1.),
        axis.title = element_text(size = 12))


########## ZCTA analysis
## Dataframe with the number of pairs available in adjacent and non-adjacent ZCTAs
## by Eastern / Western WA status
df_pairs_by_zcta <- df_RR_zcta %>% 
  filter(group_1 > group_2) %>% 
  group_by(region, is_adjacent_same_zcta) %>% 
  summarise(n_pairs = n()) %>% ungroup()

## Lowest number of pairs of adjacent ZCTAs across regions
min_n_pairs_adjacent_zctas <- min(df_pairs_by_zcta$n_pairs[df_pairs_by_zcta$is_adjacent_same_zcta == 'Adjacent ZCTAs'])
## Lowest number of pairs of non adjacent ZCTAs across regions
min_n_pairs_non_adjacent_zctas <- min(df_pairs_by_zcta$n_pairs[df_pairs_by_zcta$is_adjacent_same_zcta == 'Non adjacent ZCTAs'])

## Vector from which to subsample
vec_RR_zctas_WE_adj <- df_RR_zcta[df_RR_zcta$region == 'West - East' & df_RR_zcta$is_adjacent_same_zcta == 'Adjacent ZCTAs' & (df_RR_zcta$group_1 > df_RR_zcta$group_2), ]$RR
vec_RR_zctas_WE_non_adj <- df_RR_zcta[df_RR_zcta$region == 'West - East' & df_RR_zcta$is_adjacent_same_zcta == 'Non adjacent ZCTAs' & (df_RR_zcta$group_1 > df_RR_zcta$group_2), ]$RR
vec_RR_zctas_west_adj <- df_RR_zcta[df_RR_zcta$region == 'West - West' & df_RR_zcta$is_adjacent_same_zcta == 'Adjacent ZCTAs' & (df_RR_zcta$group_1 > df_RR_zcta$group_2), ]$RR
vec_RR_zctas_east_adj <- df_RR_zcta[df_RR_zcta$region == 'East - East' & df_RR_zcta$is_adjacent_same_zcta == 'Adjacent ZCTAs' & (df_RR_zcta$group_1 > df_RR_zcta$group_2), ]$RR
vec_RR_zctas_west_non_adj <- df_RR_zcta[df_RR_zcta$region == 'West - West' & df_RR_zcta$is_adjacent_same_zcta == 'Non adjacent ZCTAs' & (df_RR_zcta$group_1 > df_RR_zcta$group_2), ]$RR
vec_RR_zctas_east_non_adj <- df_RR_zcta[df_RR_zcta$region == 'East - East' & df_RR_zcta$is_adjacent_same_zcta == 'Non adjacent ZCTAs' & (df_RR_zcta$group_1 > df_RR_zcta$group_2), ]$RR

## Perform subsampling
n_rep <- 1000
df_sim_zcta <- Reduce('bind_rows', lapply(1:n_rep, FUN = function(i_rep){
  subsample_RR_west_adj <- sample(vec_RR_zctas_west_adj, size = min_n_pairs_adjacent_zctas, replace = F)
  subsample_RR_east_adj <- sample(vec_RR_zctas_east_adj, size = min_n_pairs_adjacent_zctas, replace = F)
  subsample_RR_WE_adj <-  sample(vec_RR_zctas_WE_adj, size = min_n_pairs_adjacent_zctas, replace = F)
  
  subsample_RR_west_non_adj <-sample(vec_RR_zctas_west_non_adj, size = min_n_pairs_non_adjacent_zctas, replace = F)
  subsample_RR_east_non_adj <- sample(vec_RR_zctas_east_non_adj, size = min_n_pairs_non_adjacent_zctas, replace = F)
  subsample_RR_WE_non_adj <- sample(vec_RR_zctas_WE_non_adj, size = min_n_pairs_non_adjacent_zctas, replace = F)
  
  c('p_val_west' = wilcox.test(x = subsample_RR_west_adj, y = subsample_RR_west_non_adj)$p.value,
    'p_val_east' = wilcox.test(x = subsample_RR_east_adj, y = subsample_RR_east_non_adj)$p.value,
    'p_val_WE' = wilcox.test(x = subsample_RR_WE_adj, y = subsample_RR_WE_non_adj)$p.value,
    'rep' = i_rep)
})) 

df_for_boxplot_p_val_zctas <-  df_sim_zcta %>% 
  summarise(y05_west = quantile(p_val_west, 0.05), y25_west = quantile(p_val_west, 0.25), y50_west = quantile(p_val_west, 0.5), y75_west = quantile(p_val_west, 0.75), y95_west = quantile(p_val_west, 0.95),
            y05_east = quantile(p_val_east, 0.05), y25_east = quantile(p_val_east, 0.25), y50_east = quantile(p_val_east, 0.5), y75_east = quantile(p_val_east, 0.75), y95_east = quantile(p_val_east, 0.95),
            y05_WE = quantile(p_val_WE, 0.05), y25_WE = quantile(p_val_WE, 0.25), y50_WE = quantile(p_val_WE, 0.5), y75_WE = quantile(p_val_WE, 0.75), y95_WE = quantile(p_val_WE, 0.95))

plt_impact_subsampling_wilcoxon_zcta <- df_sim_zcta %>% ggplot() +
  geom_jitter(aes(x = 'West - West', y = p_val_west), alpha = 0.08, col = 'darkgrey',
              width = 0.3, height = 0.) +
  geom_boxplot(data = df_for_boxplot_p_val_zctas, 
               aes(x = 'West - West', ymin = y05_west, lower = y25_west, middle = y50_west, upper = y75_west, ymax = y95_west),
               stat = "identity", fill = NA, width = 0.7, color = col_west) +
  geom_jitter(aes(x = 'West - East', y = p_val_WE), alpha = 0.08, col = 'darkgrey',
              width = 0.3, height = 0.) +
  geom_boxplot(data = df_for_boxplot_p_val_zctas, 
               aes(x = 'West - East',  ymin = y05_WE, lower = y25_WE, middle = y50_WE, upper = y75_WE, ymax = y95_WE),
               stat = "identity", fill = NA, width = 0.7, color = col_east_west) +
  geom_jitter(aes(x = 'East - East', y = p_val_east), alpha = 0.08, col = 'darkgrey',
              height = 0., width = 0.3) +
  geom_boxplot(data = df_for_boxplot_p_val_zctas, 
               aes(x = 'East - East',  ymin = y05_east, lower = y25_east, middle = y50_east, upper = y75_east, ymax = y95_east),
               stat = "identity", fill = NA, width = 0.7, color = col_east) +
  scale_x_discrete(name = '', limits = rev) +
  scale_y_continuous(trans = 'log10', 
                     name = 'p-value (Wilcoxon test for difference in RR\nbetween adjacent and non-adjacent ZCTAs)',
                     breaks = c(1e-12, 1e-11, 1e-10, 1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1.),
                     labels = c(expression(10^{-12}), expression(10^{-11}), 
                                expression(10^{-10}), expression(10^{-9}), 
                                expression(10^{-8}), expression(10^{-7}),
                                expression(10^{-6}), expression(10^{-5}), 
                                expression(10^{-4}), expression(10^{-3}), 
                                expression(10^{-2}), expression(10^{-1}),
                                expression(10^{0}))) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1.),
        axis.title = element_text(size = 12))

plot(plt_impact_subsampling_wilcoxon)
plot(plt_impact_subsampling_wilcoxon_zcta)

panel_subsampling_wilcoxon <- ggarrange(plt_impact_subsampling_wilcoxon + ggtitle('County level'),
                                        plt_impact_subsampling_wilcoxon_zcta + ggtitle('ZCTA level'))

# pdf('../plots/figure_space/impact_subsampling_wilcoxon.pdf',
#     height = 6, width = 7)
plot(panel_subsampling_wilcoxon)
#dev.off()
