## This script compares the relative risk of movement among adjacent counties 
## across the Eastern / Western WA border.

library(tidyverse)
library(ggpubr)

## Load adjacency matrix
df_adj_county <- readRDS('../../data/maps/df_adj_county.rds')

## Load characteristics of WA counties
df_char_counties <- read.csv('../../data/maps/county_wa.csv') %>% as_tibble()

## Load the relative risk of observing identical sequences in two counties
## and add information regarding counties Eastern / Western WA membership
df_RR_counties <- read_csv('../../results/RR_county/df_RR_county_0_mut_away.csv') %>% 
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


## Load the relative risk of movement between two counties from workflow
## and add information regarding counties Eastern / Western WA membership
df_RR_workflow <- readRDS('../../results/RR_mobility/RR_workflow_county_WA.rds') %>% 
  left_join(df_adj_county) %>% 
  mutate(is_same_county = (county_1 == county_2)) %>% 
  left_join(df_char_counties %>% select(county, is_west), by = c('county_1' = 'county')) %>% 
  rename(is_west_1 = is_west) %>% 
  left_join(df_char_counties %>% select(county, is_west), by = c('county_2' = 'county')) %>% 
  rename(is_west_2 = is_west) %>% 
  mutate(region = case_when(is_west_1 + is_west_2 == 2 ~ 'West - West',
                            is_west_1 + is_west_2 == 1 ~ 'West - East',
                            is_west_1 + is_west_2 == 0 ~ 'East - East'),
         region = factor(region, levels = c('West - West', 'West - East', 'East - East'))) %>% 
  mutate(is_adjacent_same_county = case_when(is_same_county == T ~ 'Same county',
                                             is_adjacent == T ~ 'Adjacent counties',
                                             TRUE ~ 'Non adjacent counties'),
         is_adjacent_same_county = factor(is_adjacent_same_county, levels = c('Non adjacent counties', 'Adjacent counties', 'Same county'))) %>% 
  rename(group_1 = county_1, group_2 = county_2)

## Load the relative risk of movement between two counties from mobile phone mobility data
## and add information regarding counties Eastern / Western WA membership
df_RR_mobile_phone <- readRDS('../../results/RR_mobility/RR_mobile_phone_county_WA.rds') %>% 
  left_join(df_adj_county) %>% 
  mutate(is_same_county = (county_1 == county_2)) %>% 
  left_join(df_char_counties %>% select(county, is_west), by = c('county_1' = 'county')) %>% 
  rename(is_west_1 = is_west) %>% 
  left_join(df_char_counties %>% select(county, is_west), by = c('county_2' = 'county')) %>% 
  rename(is_west_2 = is_west) %>% 
  mutate(region = case_when(is_west_1 + is_west_2 == 2 ~ 'West - West',
                            is_west_1 + is_west_2 == 1 ~ 'West - East',
                            is_west_1 + is_west_2 == 0 ~ 'East - East'),
         region = factor(region, levels = c('West - West', 'West - East', 'East - East'))) %>% 
  mutate(is_adjacent_same_county = case_when(is_same_county == T ~ 'Same county',
                                             is_adjacent == T ~ 'Adjacent counties',
                                             TRUE ~ 'Non adjacent counties'),
         is_adjacent_same_county = factor(is_adjacent_same_county, levels = c('Non adjacent counties', 'Adjacent counties', 'Same county'))) %>% 
  rename(group_1 = county_1, group_2 = county_2)


## Get the list of counties that are situated on the Eastern / Western WA border
vec_counties_border <- df_RR_workflow %>% 
  mutate(is_different_region = (is_west_1 != is_west_2)) %>% 
  mutate(is_adjacent_across_border = (is_different_region & is_adjacent)) %>%
  filter(is_adjacent_across_border == T) %>% 
  group_by(group_1) %>% slice(1) %>% ungroup() %>% 
  select(group_1) %>% unlist() %>% as.vector()

## Build a dataframe to compare, for counties on the border, the RR (either from mobility or ID seq)
## across the border or within the region focusing on adjacent counties
df_for_comp <- df_RR_workflow %>% 
  select(group_1, group_2, RR, is_west_1, is_west_2, is_adjacent) %>% mutate(type = 'From workflow mobility data') %>% 
  bind_rows(df_RR_mobile_phone %>% 
              select(group_1, group_2, RR, is_west_1, is_west_2, is_adjacent) %>% mutate(type = 'From mobile phone mobility data')) %>% 
  bind_rows(df_RR_counties %>% 
              select(group_1, group_2, RR, is_west_1, is_west_2, is_adjacent) %>% mutate(type = 'From sequence data')) %>% 
  mutate(is_border_county_1 = group_1 %in% vec_counties_border) %>% 
  filter(is_border_county_1, is_adjacent == T, group_1 > group_2) %>% 
  mutate(is_across_border = (is_west_1 != is_west_2)) %>% 
  mutate(type = factor(type, levels = c('From mobile phone mobility data', 'From workflow mobility data', 'From sequence data')))

## Dataframe that will be used to display boxplots
df_for_boxplot <- df_for_comp %>% 
  group_by(type, is_across_border) %>% 
  summarise(y05 = quantile(RR, 0.05),
            y25 = quantile(RR, 0.25),
            y50 = quantile(RR, 0.5),
            y75 = quantile(RR, 0.75),
            y95 = quantile(RR, 0.95))

## Value for zero on the y-axis scale
yaxis_zero_value <- min(df_for_comp$RR[df_for_comp$RR > 0.]) * 0.1

# Boxplot of RR of movement from mobile phone data across the border and within region
plt_mobile_phone <- df_for_comp %>% 
  filter(type == 'From mobile phone mobility data') %>% 
  mutate(n_pairs = ifelse(RR == 0., 0., 890),
         RR_bis = ifelse(RR == 0., yaxis_zero_value, RR)) %>% 
  ggplot(aes(x = as.factor(is_across_border))) +
  geom_jitter(aes(y = RR_bis), width = 0.2) +
  geom_boxplot(data = df_for_boxplot %>% 
                 filter(type == 'From mobile phone mobility data') %>% 
                 mutate(RR = 1, y05 = ifelse(y05 == 0., yaxis_zero_value, y05)),
               aes(ymin = y05, lower = y25, middle = y50, upper = y75, ymax = y95),
               stat = 'identity', fill = NA) +
  scale_x_discrete(name = '', breaks = c(T, F), 
                   labels = c('Across the\nW/E border', 'Within\nE/W WA')) +
  scale_y_continuous(trans = 'log', name = expression(RR['mobile phone']),
                     breaks = c(yaxis_zero_value, 0.01, 0.1, 1., 10.),
                     labels = c(0, expression(10^{-2}), expression(10^{-1}), expression(10^{0}), expression(10^{1})),
                     expand = expansion(add = c(0.05, 0.1))) +
  facet_grid((! RR == 0.) ~ type,  scales = 'free', space = "free") +
  theme_classic() +
  theme(strip.background = element_rect(fill = 'gray22'),
        strip.text = element_text(size = 12, colour = 'white'),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1.),
        axis.text.y = element_text(size = 13),
        strip.background.y = element_blank(),
        strip.text.y = element_blank())

# Boxplot of RR of movement from commue data across the border and within region
plt_workflow <- df_for_comp %>% 
  filter(type == 'From workflow mobility data') %>% 
  mutate(n_pairs = ifelse(RR == 0., 0., 890),
         RR_bis = ifelse(RR == 0., yaxis_zero_value, RR)) %>% 
  ggplot(aes(x = as.factor(is_across_border))) +
  geom_jitter(aes(y = RR_bis), width = 0.2) +
  geom_boxplot(data = df_for_boxplot %>% 
                 filter(type == 'From workflow mobility data') %>% 
                 mutate(RR = 1, y05 = ifelse(y05 == 0., yaxis_zero_value * 2, y05)),
               aes(ymin = y05, lower = y25, middle = y50, upper = y75, ymax = y95),
               stat = 'identity', fill = NA) +
  scale_x_discrete(name = '', breaks = c(T, F), 
                   labels = c('Across the\nW/E border', 'Within\nE/W WA')) +
  scale_y_continuous(trans = 'log', name = expression(RR['workflow']),
                     breaks = c(yaxis_zero_value, 0.01, 0.1, 1., 10.),
                     labels = c(0, expression(10^{-2}), expression(10^{-1}), expression(10^{0}), expression(10^{1})),
                     expand = expansion(add = c(0.05, 0.1))) +
  facet_grid((RR == 0.) ~ type,  scales = 'free', space = "free") +
  theme_classic() +
  theme(strip.background = element_rect(fill = 'gray22'),
        strip.text = element_text(size = 12, colour = 'white'),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1.),
        axis.text.y = element_text(size = 13),
        strip.background.y = element_blank(),
        strip.text.y = element_blank())

# Boxplot of RR of movement from identical sequences across the border and within region
plt_sequence <- df_for_comp %>% 
  filter(type == 'From sequence data') %>% 
  mutate(n_pairs = ifelse(RR == 0., 0., 890),
         RR_bis = ifelse(RR == 0., yaxis_zero_value, RR)) %>% 
  ggplot(aes(x = as.factor(is_across_border))) +
  geom_jitter(aes(y = RR_bis), width = 0.2) +
  geom_boxplot(data = df_for_boxplot %>% 
                 filter(type == 'From sequence data') %>% 
                 mutate(RR = 1, y05 = ifelse(y05 == 0., yaxis_zero_value * 2, y05)),
               aes(ymin = y05, lower = y25, middle = y50, upper = y75, ymax = y95),
               stat = 'identity', fill = NA) +
  scale_x_discrete(name = '', breaks = c(T, F), 
                   labels = c('Across the\nW/E border', 'Within\nE/W WA')) +
  scale_y_continuous(trans = 'log', name = expression(RR['identical sequences']),
                     breaks = c(yaxis_zero_value, 0.01, 0.1, 1., 10.),
                     labels = c(0, expression(10^{-2}), expression(10^{-1}), expression(10^{0}), expression(10^{1})),
                     expand = expansion(add = c(0.05, 0.1))) +
  facet_grid((RR == 0.) ~ type,  scales = 'free', space = "free") +
  theme_classic() +
  theme(strip.background = element_rect(fill = 'gray22'),
        strip.text = element_text(size = 12, colour = 'white'),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1.),
        axis.text.y = element_text(size = 13),
        strip.background.y = element_blank(),
        strip.text.y = element_blank())

# Create a panels woth the 3 plots (mobile phone, commute, identical sequences)
panel_adjacency_across_border <- 
  ggarrange(plt_mobile_phone, plt_workflow, plt_sequence, ncol = 3,
            labels = 'AUTO')

#pdf('../plots/figure_space/panel_adjacency_across_border_mobility.pdf', height = 5., width = 10.)
plot(panel_adjacency_across_border)
#dev.off()


# Run Wilcoxon test to test whether the appearant differences are significant
wilcox.test(x = df_for_comp$RR[df_for_comp$type == 'From mobile phone mobility data' & df_for_comp$is_across_border == T],
            y = df_for_comp$RR[df_for_comp$type == 'From mobile phone mobility data' & df_for_comp$is_across_border == F])

wilcox.test(x = df_for_comp$RR[df_for_comp$type == 'From workflow mobility data' & df_for_comp$is_across_border == T],
            y = df_for_comp$RR[df_for_comp$type == 'From workflow mobility data' & df_for_comp$is_across_border == F])
# Check that we get the same conclusion when removing ties
wilcox.test(x = df_for_comp$RR[df_for_comp$type == 'From workflow mobility data' & df_for_comp$is_across_border == T & df_for_comp$RR > 0.],
            y = df_for_comp$RR[df_for_comp$type == 'From workflow mobility data' & df_for_comp$is_across_border == F & df_for_comp$RR > 0.]) 

wilcox.test(x = df_for_comp$RR[df_for_comp$type == 'From sequence data' & df_for_comp$is_across_border == T],
            y = df_for_comp$RR[df_for_comp$type == 'From sequence data' & df_for_comp$is_across_border == F])



