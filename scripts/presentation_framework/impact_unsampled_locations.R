library(tidyverse)

## Load characteristics of WA counties
df_char_counties <- read.csv('../data/maps/county_wa.csv') %>% as_tibble()
vec_counties_west <- df_char_counties$county[df_char_counties$is_west == T]

## Load the relative risk of observing identical sequences between two counties
df_RR_counties <- readRDS('../results/RR_county/df_RR_county_0_mut_away.rds')

df_RR_only_west <- df_RR_counties %>% 
  rename(RR_full = RR) %>% 
  filter(group_1 %in% vec_counties_west, group_2 %in% vec_counties_west) %>% 
  group_by(group_1) %>% 
  mutate(n_pairs_1_x = sum(n_pairs)) %>% 
  group_by(group_2) %>% 
  mutate(n_pairs_x_2 = sum(n_pairs)) %>% 
  ungroup() %>% 
  mutate(n_pairs_x_x = sum(n_pairs),
         RR_only_west = n_pairs / n_pairs_1_x / n_pairs_x_2 * n_pairs_x_x) %>% 
  filter(group_1 >= group_2)
  
df_RR_only_east <- df_RR_counties %>% 
  rename(RR_full = RR) %>% 
  filter(! group_1 %in% vec_counties_west, ! group_2 %in% vec_counties_west) %>% 
  group_by(group_1) %>% 
  mutate(n_pairs_1_x = sum(n_pairs)) %>% 
  group_by(group_2) %>% 
  mutate(n_pairs_x_2 = sum(n_pairs)) %>% 
  ungroup() %>% 
  mutate(n_pairs_x_x = sum(n_pairs),
         RR_only_east = n_pairs / n_pairs_1_x / n_pairs_x_2 * n_pairs_x_x) %>% 
  filter(group_1 >= group_2)

cor_only_east <- df_RR_only_east %>% summarise(cor = cor(RR_full, RR_only_east, method = 'spearman')) %>% round(digits = 2) %>% unlist() %>% as.numeric()
cor_only_west <- df_RR_only_west %>% summarise(cor = cor(RR_full, RR_only_west, method = 'spearman')) %>% round(digits = 2) %>% unlist() %>% as.numeric()

zero_value_east <- min(c(df_RR_only_east$RR_full[df_RR_only_east$RR_full > 0.],
                         df_RR_only_east$RR_only_east[df_RR_only_east$RR_only_east > 0.])) * 0.5
zero_value_west <- min(c(df_RR_only_west$RR_full[df_RR_only_west$RR_full > 0.],
                         df_RR_only_west$RR_only_west[df_RR_only_west$RR_only_west > 0.])) * 0.5

plt_cor_east <- df_RR_only_east %>% 
  mutate(RR_full_crop = ifelse(RR_full == 0., zero_value_east, RR_full),
         RR_only_east_crop = ifelse(RR_only_east == 0., zero_value_east, RR_only_east)) %>% 
  ggplot() +
  geom_text(data = tibble(RR_full = 1., RR_only_east = 1.),
            aes(x = 3., y = 2e3), label = paste0('Spearman r = ', cor_only_east)) +
  geom_point(aes(x = RR_full_crop, y = RR_only_east_crop)) +
  scale_x_continuous(name = expression(RR['full dataset']),
                     trans = 'log',
                     breaks = c(zero_value_east, 1e-1, 1e0, 1e1, 1e2, 1e3, 1e4),
                     labels = c(0, expression(10^{-1}), expression(10^{0}), expression(10^{1}),
                                expression(10^{2}), expression(10^{3}), expression(10^{4})),
                     expand = expansion(add = c(0.5, 0.5))) +
  scale_y_continuous(name = expression(RR['only Eastern WA counties']),
                     trans = 'log',
                     breaks = c(zero_value_east, 1e-1, 1e0, 1e1, 1e2, 1e3, 1e4),
                     labels = c(0, expression(10^{-1}), expression(10^{0}), expression(10^{1}),
                                expression(10^{2}), expression(10^{3}), expression(10^{4})),
                     expand = expansion(add = c(0.5, 0.5))) +
  facet_grid((RR_full == 0.) ~ (RR_only_east != 0.), 
             scales= 'free', space = 'free') +
  theme_classic() +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        strip.background = element_blank(), 
        strip.text = element_blank())
  
plt_cor_west <- df_RR_only_west %>% 
  mutate(RR_full_crop = ifelse(RR_full == 0., zero_value_west, RR_full),
         RR_only_west_crop = ifelse(RR_only_west == 0., zero_value_west, RR_only_west)) %>% 
  ggplot() +
  geom_text(data = tibble(RR_full = 1., RR_only_west = 1.),
            aes(x = 3., y = 2e3), label = paste0('Spearman r = ', cor_only_west)) +
  geom_point(aes(x = RR_full_crop, y = RR_only_west_crop)) +
  scale_x_continuous(name = expression(RR['full dataset']),
                     trans = 'log',
                     breaks = c(zero_value_west, 1e-1, 1e0, 1e1, 1e2, 1e3, 1e4),
                     labels = c(0, expression(10^{-1}), expression(10^{0}), expression(10^{1}),
                                expression(10^{2}), expression(10^{3}), expression(10^{4})),
                     expand = expansion(add = c(0.5, 0.5))) +
  scale_y_continuous(name = expression(RR['only Western WA counties']),
                     trans = 'log',
                     breaks = c(zero_value_west, 1e-1, 1e0, 1e1, 1e2, 1e3, 1e4),
                     labels = c(0, expression(10^{-1}), expression(10^{0}), expression(10^{1}),
                                expression(10^{2}), expression(10^{3}), expression(10^{4})),
                     expand = expansion(add = c(0.5, 0.5))) +
  facet_grid((RR_full == 0.) ~ (RR_only_west != 0.), 
             scales= 'free', space = 'free') +
  theme_classic() +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        strip.background = element_blank(), 
        strip.text = element_blank())

panel_impact_unsampled_locations <- ggarrange(plt_cor_west, plt_cor_east, nrow = 1, ncol = 2, labels = 'AUTO')

plot(panel_impact_unsampled_locations)

# pdf('../plots/figure_framework/impact_unsampled_locations.pdf', height = 3.5, width = 7.5)
# plot(panel_impact_unsampled_locations)
# dev.off()
# png('../plots/figure_framework/impact_unsampled_locations.png', height = 3.5, width = 7.5, res = 350, units = 'in')
# plot(panel_impact_unsampled_locations)
# dev.off()
