library(tidyverse)
library(ggpubr)

## Load adjacency matrix
df_adj_county <- readRDS('../data/maps/df_adj_county.rds')

## Load characteristics of WA counties
df_char_counties <- read.csv('../data/maps/county_wa.csv') %>% as_tibble()

## Load the relative risk of observing identical sequences in two counties
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


## Load the relative risk of movement between two counties
df_RR_workflow <- readRDS('../results/RR_mobility/RR_workflow_county_WA.rds') %>% 
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

df_RR_mobile_phone <- readRDS('../results/RR_mobility/RR_mobile_phone_county_WA.rds') %>% 
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

## Wilcoxon test - workflow data
wilcox.test(x = df_RR_workflow[df_RR_workflow$region == 'West - East' & df_RR_workflow$is_adjacent_same_county == 'Adjacent counties'& (df_RR_workflow$group_1 > df_RR_workflow$group_2), ]$RR,
            y = df_RR_workflow[df_RR_workflow$region == 'West - East' & df_RR_workflow$is_adjacent_same_county == 'Non adjacent counties'& (df_RR_workflow$group_1 > df_RR_workflow$group_2), ]$RR)

wilcox.test(x = df_RR_workflow[df_RR_workflow$region == 'West - West' & df_RR_workflow$is_adjacent_same_county == 'Adjacent counties'& (df_RR_workflow$group_1 > df_RR_workflow$group_2), ]$RR,
            y = df_RR_workflow[df_RR_workflow$region == 'West - West' & df_RR_workflow$is_adjacent_same_county == 'Non adjacent counties'& (df_RR_workflow$group_1 > df_RR_workflow$group_2), ]$RR)

wilcox.test(x = df_RR_workflow[df_RR_workflow$region == 'West - West' & df_RR_workflow$is_adjacent_same_county == 'Adjacent counties'& (df_RR_workflow$group_1 > df_RR_workflow$group_2), ]$RR,
            y = df_RR_workflow[df_RR_workflow$region == 'West - West' & df_RR_workflow$is_adjacent_same_county == 'Same county', ]$RR)

wilcox.test(x = df_RR_workflow[df_RR_workflow$region == 'East - East' & df_RR_workflow$is_adjacent_same_county == 'Adjacent counties'& (df_RR_workflow$group_1 > df_RR_workflow$group_2), ]$RR,
            y = df_RR_workflow[df_RR_workflow$region == 'East - East' & df_RR_workflow$is_adjacent_same_county == 'Non adjacent counties'& (df_RR_workflow$group_1 > df_RR_workflow$group_2), ]$RR)

wilcox.test(x = df_RR_workflow[df_RR_workflow$region == 'East - East' & df_RR_workflow$is_adjacent_same_county == 'Adjacent counties'& (df_RR_workflow$group_1 > df_RR_workflow$group_2), ]$RR,
            y = df_RR_workflow[df_RR_workflow$region == 'East - East' & df_RR_workflow$is_adjacent_same_county == 'Same county', ]$RR)


## Wilcoxon test - mobile phone data
wilcox.test(x = df_RR_mobile_phone[df_RR_mobile_phone$region == 'West - East' & df_RR_mobile_phone$is_adjacent_same_county == 'Adjacent counties'& (df_RR_mobile_phone$group_1 > df_RR_mobile_phone$group_2), ]$RR,
                                   y = df_RR_mobile_phone[df_RR_mobile_phone$region == 'West - East' & df_RR_mobile_phone$is_adjacent_same_county == 'Non adjacent counties'& (df_RR_mobile_phone$group_1 > df_RR_mobile_phone$group_2), ]$RR,
                                   conf.int = T)

wilcox.test(x = df_RR_mobile_phone[df_RR_mobile_phone$region == 'West - West' & df_RR_mobile_phone$is_adjacent_same_county == 'Adjacent counties'& (df_RR_mobile_phone$group_1 > df_RR_mobile_phone$group_2), ]$RR,
            y = df_RR_mobile_phone[df_RR_mobile_phone$region == 'West - West' & df_RR_mobile_phone$is_adjacent_same_county == 'Non adjacent counties'& (df_RR_mobile_phone$group_1 > df_RR_mobile_phone$group_2), ]$RR,
            conf.int = T)['conf.int']

wilcox.test(y = df_RR_mobile_phone[df_RR_mobile_phone$region == 'West - West' & df_RR_mobile_phone$is_adjacent_same_county == 'Adjacent counties'& (df_RR_mobile_phone$group_1 > df_RR_mobile_phone$group_2), ]$RR,
            x = df_RR_mobile_phone[df_RR_mobile_phone$region == 'West - West' & df_RR_mobile_phone$is_adjacent_same_county == 'Same county', ]$RR)

wilcox.test(x = df_RR_mobile_phone[df_RR_mobile_phone$region == 'East - East' & df_RR_mobile_phone$is_adjacent_same_county == 'Adjacent counties'& (df_RR_mobile_phone$group_1 > df_RR_mobile_phone$group_2), ]$RR,
            y = df_RR_mobile_phone[df_RR_mobile_phone$region == 'East - East' & df_RR_mobile_phone$is_adjacent_same_county == 'Non adjacent counties'& (df_RR_mobile_phone$group_1 > df_RR_mobile_phone$group_2), ]$RR)

wilcox.test(x = df_RR_mobile_phone[df_RR_mobile_phone$region == 'East - East' & df_RR_mobile_phone$is_adjacent_same_county == 'Adjacent counties'& (df_RR_mobile_phone$group_1 > df_RR_mobile_phone$group_2), ]$RR,
            y = df_RR_mobile_phone[df_RR_mobile_phone$region == 'East - East' & df_RR_mobile_phone$is_adjacent_same_county == 'Same county', ]$RR)


yaxis_zero_value_workflow <- min(df_RR_workflow$RR[df_RR_workflow$RR > 0.]) * 0.4
df_for_boxplot_workflow <- df_RR_workflow %>% 
  group_by(is_adjacent_same_county, region) %>% 
  summarise(y05 = quantile(RR, 0.05),
            y25 = quantile(RR, 0.25),
            y50 = quantile(RR, 0.5),
            y75 = quantile(RR, 0.75),
            y95 = quantile(RR, 0.95)) %>% 
  mutate(y05_plot = ifelse(y05 == 0., yaxis_zero_value_workflow, y05),
         y25_plot = ifelse(y25 == 0., yaxis_zero_value_workflow, y25),
         y50_plot = ifelse(y50 == 0., yaxis_zero_value_workflow, y50),
         y75_plot = ifelse(y75 == 0., yaxis_zero_value_workflow, y75),
         y95_plot = ifelse(y95 == 0., yaxis_zero_value_workflow, y95))

yaxis_zero_value_mobile_phone <- min(df_RR_mobile_phone$RR[df_RR_mobile_phone$RR > 0.]) * 0.4
df_for_boxplot_mobile_phone <- df_RR_mobile_phone %>% 
  group_by(is_adjacent_same_county, region) %>% 
  summarise(y05 = quantile(RR, 0.05),
            y25 = quantile(RR, 0.25),
            y50 = quantile(RR, 0.5),
            y75 = quantile(RR, 0.75),
            y95 = quantile(RR, 0.95)) %>% 
  mutate(y05_plot = ifelse(y05 == 0., yaxis_zero_value_mobile_phone, y05),
         y25_plot = ifelse(y25 == 0., yaxis_zero_value_mobile_phone, y25),
         y50_plot = ifelse(y50 == 0., yaxis_zero_value_mobile_phone, y50),
         y75_plot = ifelse(y75 == 0., yaxis_zero_value_mobile_phone, y75),
         y95_plot = ifelse(y95 == 0., yaxis_zero_value_mobile_phone, y95))

col_west <- 'dodgerblue3'
col_east <- 'coral1'
col_east_west <- 'darkslateblue'

plt_RR_adj_workflow <- df_RR_workflow  %>%
  mutate(RR = ifelse(RR == 0., yaxis_zero_value_workflow, RR)) %>% 
  ggplot(aes(x = is_adjacent_same_county,
             group = region, fill = region, color = region)) +
  geom_jitter(aes(y = RR), alpha = 0.25, height = 0., width = 0.3, color = 'darkgrey')  +
  geom_boxplot(data = df_for_boxplot_workflow %>% mutate(n_tot_workflows = 1.), 
               aes(ymin = y05, lower = y25, middle = y50, upper = y75, ymax = y95, 
                   group = interaction(is_adjacent_same_county, region)),
               stat = "identity", fill = NA, width = 0.7) +
  scale_x_discrete(name = '', 
                   breaks = c('Non adjacent counties', 'Adjacent counties', 'Same county'),
                   labels = c('Non adjacent', 'Adjacent', 'Within county')) +
  scale_y_continuous(trans = 'log', name = expression(RR["workflow"]),
                     expand = expansion(mult = c(0.05, 0.), add = c(0.1, 0.1)),
                     breaks = c(yaxis_zero_value_workflow, 0.1, 1, 10, 100, 1e3, 1e4),
                     labels = c(0, expression(10^{-1}), 
                                expression(10^{0}), expression(10^{1}), expression(10^{2}),
                                expression(10^{3}), expression(10^{4}))) +
  scale_colour_manual(name = '', 
                      values = c(col_west, col_east_west, col_east)) +
  scale_fill_manual(name = '', 
                    values = c(col_west, col_east_west, col_east)) +
  coord_flip() +
  facet_grid(region ~ (n_tot_workflows != 0),  scales = 'free', space = "free") +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        axis.text = element_text(size = 12),
        strip.background.x = element_blank(),
        strip.text.x = element_blank(),
        strip.background.y = element_rect(fill = 'gray22'),
        strip.text.y = element_text(colour = 'white', size = 11),
        legend.position = 'none') 


plt_RR_adj_mobile_phone <- df_RR_mobile_phone  %>%
  mutate(RR = ifelse(RR == 0., yaxis_zero_value_mobile_phone, RR)) %>% 
  ggplot(aes(x = is_adjacent_same_county,
             group = region, fill = region, color = region)) +
  geom_jitter(aes(y = RR), alpha = 0.25, height = 0., width = 0.3, color = 'darkgrey')  +
  geom_boxplot(data = df_for_boxplot_mobile_phone %>% mutate(n_tot_visits = 1.), 
               aes(ymin = y05, lower = y25, middle = y50, upper = y75, ymax = y95, 
                   group = interaction(is_adjacent_same_county, region)),
               stat = "identity", fill = NA, width = 0.7) +
  scale_x_discrete(name = '', 
                   breaks = c('Non adjacent counties', 'Adjacent counties', 'Same county'),
                   labels = c('Non adjacent', 'Adjacent', 'Within county')) +
  scale_y_continuous(trans = 'log', name = expression(RR["mobile phone"]),
                     expand = expansion(mult = c(0.05, 0.), add = c(0.1, 0.1)),
                     breaks = c(yaxis_zero_value_mobile_phone, 0.1, 1, 10, 100, 1e3, 1e4),
                     labels = c(0, expression(10^{-1}), 
                                expression(10^{0}), expression(10^{1}), expression(10^{2}),
                                expression(10^{3}), expression(10^{4}))) +
  scale_colour_manual(name = '', 
                      values = c(col_west, col_east_west, col_east)) +
  scale_fill_manual(name = '', 
                    values = c(col_west, col_east_west, col_east)) +
  coord_flip() +
  facet_grid(region ~ (n_tot_visits != 0),  scales = 'free', space = "free") +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        axis.text = element_text(size = 12),
        strip.background.x = element_blank(),
        strip.text.x = element_blank(),
        strip.background.y = element_rect(fill = 'gray22'),
        strip.text.y = element_text(colour = 'white', size = 11),
        legend.position = 'none') 

plt_RR_adj_mobile_phone


vec_counties_border <- df_RR_workflow %>% 
  mutate(is_different_region = (is_west_1 != is_west_2)) %>% 
  mutate(is_adjacent_across_border = (is_different_region & is_adjacent)) %>%
  filter(is_adjacent_across_border == T) %>% 
  group_by(group_1) %>% slice(1) %>% ungroup() %>% 
  select(group_1) %>% unlist() %>% as.vector()


df_RR_mobile_phone %>% 
  mutate(is_different_region = (is_west_1 != is_west_2)) %>% 
  mutate(is_adjacent_across_border = (is_different_region & is_adjacent)) %>% 
  mutate(is_border_county_1 = group_1 %in% vec_counties_border) %>% 
  filter(is_border_county_1) %>% 
  ggplot(aes(x = as.factor(is_adjacent), y = RR, group = is_different_region, colour = is_different_region)) +
  geom_point(position = position_dodge(0.8)) +
  facet_wrap(. ~ group_1) +
  scale_y_continuous(trans = 'log')

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

df_for_boxplot <- df_for_comp %>% 
  group_by(type, is_across_border) %>% 
  summarise(y05 = quantile(RR, 0.05),
            y25 = quantile(RR, 0.25),
            y50 = quantile(RR, 0.5),
            y75 = quantile(RR, 0.75),
            y95 = quantile(RR, 0.95))

yaxis_zero_value = min(df_for_comp$RR[df_for_comp$RR > 0.]) * 0.1

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

panel_adjacency_across_border <- 
  ggarrange(plt_mobile_phone, plt_workflow, plt_sequence, ncol = 3,
            labels = 'AUTO')



wilcox.test(x = df_for_comp$RR[df_for_comp$type == 'From mobile phone mobility data' & df_for_comp$is_across_border == T],
            y = df_for_comp$RR[df_for_comp$type == 'From mobile phone mobility data' & df_for_comp$is_across_border == F])

wilcox.test(x = df_for_comp$RR[df_for_comp$type == 'From workflow mobility data' & df_for_comp$is_across_border == T],
            y = df_for_comp$RR[df_for_comp$type == 'From workflow mobility data' & df_for_comp$is_across_border == F])
# Check that we get the same conclusion when removing ties
wilcox.test(x = df_for_comp$RR[df_for_comp$type == 'From workflow mobility data' & df_for_comp$is_across_border == T & df_for_comp$RR > 0.],
            y = df_for_comp$RR[df_for_comp$type == 'From workflow mobility data' & df_for_comp$is_across_border == F & df_for_comp$RR > 0.]) 

wilcox.test(x = df_for_comp$RR[df_for_comp$type == 'From sequence data' & df_for_comp$is_across_border == T],
            y = df_for_comp$RR[df_for_comp$type == 'From sequence data' & df_for_comp$is_across_border == F])


pdf('../plots/figure_space/RR_adjacent_west_east_workflow.pdf', height = 2.8, width = 5.5)
plot(plt_RR_adj_workflow)
dev.off()
pdf('../plots/figure_space/RR_adjacent_west_east_mobile_phone.pdf', height = 2.8, width = 5.5)
plot(plt_RR_adj_mobile_phone)
dev.off()
pdf('../plots/figure_space/panel_adjacency_across_border_mobility.pdf', height = 5., width = 10.)
plot(panel_adjacency_across_border)
dev.off()
png('../plots/figure_space/panel_adjacency_across_border_mobility.png', height = 5., width = 10., res = 350, units = 'in')
plot(panel_adjacency_across_border)
dev.off()
