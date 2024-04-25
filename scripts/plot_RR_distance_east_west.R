library(tidyverse)

## Load distance matrix
df_dist_county <- readRDS('../data/maps/df_dist_county.rds')
df_dist_zcta <- readRDS('../data/maps/df_dist_zcta.rds')

## Load characteristics of WA counties
df_char_counties <- read.csv('../data/maps/county_wa.csv') %>% as_tibble()
df_char_zctas <- read_csv('../data/maps/relationship_zcta_county_WA.csv', col_types = 'cc') %>% 
  left_join(df_char_counties %>% select(county, is_west), by = 'county')

## Load the relative risk of observing identical sequences between two counties
df_RR_counties <- readRDS('../results/RR_county/df_RR_county_0_mut_away.rds') %>% 
  left_join(df_dist_county, by = c('group_1' = 'county_1', 'group_2' = 'county_2')) %>% 
  mutate(is_same_county = (group_1 == group_2)) %>% 
  left_join(df_char_counties %>% select(county, is_west), by = c('group_1' = 'county')) %>% 
  rename(is_west_1 = is_west) %>% 
  left_join(df_char_counties %>% select(county, is_west), by = c('group_2' = 'county')) %>% 
  rename(is_west_2 = is_west) %>% 
  mutate(region = case_when(is_west_1 + is_west_2 == 2 ~ 'West - West',
                            is_west_1 + is_west_2 == 1 ~ 'West - East',
                            is_west_1 + is_west_2 == 0 ~ 'East - East'),
         region = factor(region, levels = c('West - West', 'West - East', 'East - East')))

df_RR_zcta <- readRDS('../results/RR_zcta/df_RR_zcta_0_mut_away.rds') %>%
  rename(group_1 = zcta_1, group_2 = zcta_2) %>% as_tibble() %>% 
  left_join(df_dist_zcta, by = c('group_1' = 'zcta_1', 'group_2' = 'zcta_2')) %>% 
  mutate(is_same_zcta = (group_1 == group_2)) %>% 
  left_join(df_char_zctas %>% select(county, zcta, is_west), by = c('group_1' = 'zcta')) %>% 
  rename(county_1 = county, is_west_1 = is_west) %>% 
  left_join(df_char_zctas %>% select(county, zcta, is_west), by = c('group_2' = 'zcta')) %>% 
  rename(county_2 = county, is_west_2 = is_west) %>% 
  mutate(is_west_1 = as.numeric(is_west_1), is_west_2 = as.numeric(is_west_2)) %>% 
  mutate(region = case_when(is_west_1 + is_west_2 == 2 ~ 'West - West',
                            is_west_1 + is_west_2 == 1 ~ 'West - East',
                            is_west_1 + is_west_2 == 0 ~ 'East - East'),
         region = factor(region, levels = c('West - West', 'West - East', 'East - East'))) 

## Plot RR as a function of distance by East / West region
col_west <- 'dodgerblue3'
col_east <- 'coral1'
col_east_west <- 'darkslateblue'

yaxis_zero_value_counties <- min(df_RR_counties$RR[df_RR_counties$RR > 0.]) * 0.4
yaxis_zero_value_zctas <- min(df_RR_zcta$RR[df_RR_zcta$RR > 0.]) * 0.4

#####
distance_max_to_plot <- 330
distance_max_to_plot_2 <- 250

df_for_plot_counties <- df_RR_counties %>% 
  filter(group_1 > group_2) %>% 
  mutate(modif_RR = ifelse(RR == 0., yaxis_zero_value_counties, RR))

df_for_plot_zctas <- df_RR_zcta %>% 
  filter(group_1 > group_2) %>% 
  mutate(modif_RR = ifelse(RR == 0., yaxis_zero_value_zctas, RR))

plt_RR_distance_W_E <- df_for_plot_counties %>% 
  ggplot(aes(x = distance_km, group = region,
             colour = as.factor(region), fill = as.factor(region))) +
  geom_point(aes(y = log(modif_RR)), alpha = 0.2) +
  geom_smooth(data = df_for_plot_counties %>% mutate(n_pairs = 1.) %>% filter(RR > 0.), 
              aes(y = log(RR)), method = 'loess') +
  scale_x_continuous(name = 'Distance between counties\ncentroids (in km)',
                     breaks = seq(50, 450, 50),
                     labels = c('', 100, '', 200, '', 300, '', 400, ''),
                     expand = expansion(mult = c(0.05, 0.01))) +
  scale_y_continuous(name = expression(RR["identical sequences"]),
                     breaks = log(c(yaxis_zero_value_counties, 0.1, 1., 10., 100.)),
                     labels = c(0, expression(10^{-1}), expression(10^{0}), expression(10^{1}), expression(10^{2})),
                     expand = expansion(mult = c(0.1, 0.1))) +
  theme_classic() +
  scale_colour_manual(name = '', 
                      values = c(col_west, col_east_west, col_east)) +
  scale_fill_manual(name = '', 
                      values = c(col_west, col_east_west, col_east)) +
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 12),
        legend.background = element_blank(),
        legend.position = c(0.8, 0.9),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  facet_grid((n_pairs == 0.) ~ ., scales = 'free', space = 'free_y') +
  coord_cartesian(xlim = c(NA, distance_max_to_plot))

plt_RR_distance_W_E_2 <- df_for_plot_counties %>% 
  ggplot(aes(x = distance_km, group = region,
             colour = as.factor(region), fill = as.factor(region))) +
  geom_point(aes(y = log(modif_RR)), alpha = 0.2) +
  geom_smooth(data = df_for_plot_counties %>% mutate(n_pairs = 1.) %>% filter(RR > 0.), 
              aes(y = log(RR)), method = 'loess') +
  scale_x_continuous(name = 'Distance between counties\ncentroids (in km)',
                     breaks = seq(50, 450, 50),
                     labels = c('', 100, '', 200, '', 300, '', 400, ''),
                     expand = expansion(mult = c(0.05, 0.01))) +
  scale_y_continuous(name = expression(RR["identical sequences"]),
                     breaks = log(c(yaxis_zero_value_counties, 0.1, 1., 10., 100.)),
                     labels = c(0, expression(10^{-1}), expression(10^{0}), expression(10^{1}), expression(10^{2})),
                     expand = expansion(add = c(0.1, 0.1))) +
  theme_classic() +
  scale_colour_manual(name = '', 
                      values = c(col_west, col_east_west, col_east)) +
  scale_fill_manual(name = '', 
                    values = c(col_west, col_east_west, col_east)) +
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 12),
        legend.background = element_blank(),
        legend.position = c(0.8, 0.9),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  facet_grid((n_pairs == 0.) ~ ., scales = 'free', space = 'free_y') +
  coord_cartesian(xlim = c(NA, distance_max_to_plot_2))

plot(plt_RR_distance_W_E_2)

# pdf('../plots/figure_space/RR_geog_distance_EW.pdf', height = 4.0, width = 3.)
# plot(plt_RR_distance_W_E)
# dev.off()
# png('../plots/figure_space/RR_geog_distance_EW.png', height = 4., width = 3.0,
#     res = 350, units = 'in')
# plot(plt_RR_distance_W_E)
# dev.off()
# 
# 
# pdf('../plots/figure_space/RR_geog_distance_EW_2.pdf', height = 3.0, width = 3.0)
# plot(plt_RR_distance_W_E_2)
# dev.off()
# png('../plots/figure_space/RR_geog_distance_EW_2.png', height = 3.0, width = 3.0,
#     res = 350, units = 'in')
# plot(plt_RR_distance_W_E_2)
# dev.off()
# 
