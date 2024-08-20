## This script (1) compares the relative risk of observing identical sequences within the same county,
## in adjacent counties and in non adjacent counties.
## It also (2) despicts how the RR of observing identical sequences varies as a function of the 
## geographic distances between counties.

library(tidyverse)
library(ggsignif)

## Load adjacency matrix
df_adj_county <- readRDS('../../data/maps/df_adj_county.rds')

## Load distance matrix
df_dist_county <- readRDS('../../data/maps/df_dist_county.rds')

## Load the relative risk of observing identical sequences between two counties
df_RR_counties <- read_csv('../../results/RR_county/df_RR_county_0_mut_away.csv') %>% 
  left_join(df_adj_county, by = c('group_1' = 'county_1', 'group_2' = 'county_2')) %>% 
  left_join(df_dist_county, by = c('group_1' = 'county_1', 'group_2' = 'county_2')) %>% 
  mutate(is_same_county = (group_1 == group_2))

## Compare RR of identical sequences by adjacency status
vec_RR_within_county <- df_RR_counties$RR[df_RR_counties$is_same_county == T]
vec_RR_adjacent_county <- df_RR_counties$RR[df_RR_counties$is_adjacent == T]
vec_RR_non_adjacent_county <- df_RR_counties$RR[df_RR_counties$is_adjacent == F & df_RR_counties$is_same_county == F]

wilcox.test(vec_RR_within_county, vec_RR_adjacent_county)
wilcox.test(vec_RR_non_adjacent_county, vec_RR_adjacent_county)
wilcox.test(vec_RR_non_adjacent_county, vec_RR_within_county)

## Plot RR by adjacency status
yaxis_zero_value <- min(df_RR_counties$RR[df_RR_counties$RR > 0.]) * 0.1


df_for_boxplot <- df_RR_counties %>% 
  group_by(is_adjacent, is_same_county) %>% 
  summarise(y05 = quantile(RR, 0.05),
            y25 = quantile(RR, 0.25),
            y50 = quantile(RR, 0.5),
            y75 = quantile(RR, 0.75),
            y95 = quantile(RR, 0.95)) %>% 
  mutate(y05_plot = ifelse(y05 == 0., yaxis_zero_value, y05),
         y25_plot = ifelse(y25 == 0., yaxis_zero_value, y25),
         y50_plot = ifelse(y50 == 0., yaxis_zero_value, y50),
         y75_plot = ifelse(y75 == 0., yaxis_zero_value, y75),
         y95_plot = ifelse(y95 == 0., yaxis_zero_value, y95))



plt_comp_RR_adjacent <- df_RR_counties %>%
  mutate(RR = ifelse(RR == 0., yaxis_zero_value, RR)) %>% 
  ggplot(aes(x = interaction(is_adjacent, is_same_county))) +
  geom_jitter(aes(y = RR),
              alpha = 0.25, height = 0., color = 'darkgrey', width = 0.3) +
  geom_boxplot(data = df_for_boxplot,
  aes(ymin = y05_plot, lower = y25_plot, middle = y50_plot,
      upper = y75_plot, ymax = y95_plot),
  stat = "identity", fill = NA, width = 0.7) +
  geom_signif(stat="identity",
              data=data.frame(x = c(0.875, 1.875), xend = c(2.125, 3.125),
                              y = c(1e2, 5e3),
                              annotation=c("***", " *** ")),
              aes(x = x, xend = xend, 
                  y = y, yend = y, annotation = annotation),
              color = 'firebrick') +
  scale_x_discrete(name = '', breaks = c('0.TRUE', '1.FALSE', '0.FALSE'),
                   labels = c('Within county', 'Adjacent county', 'Non-adjacent county')) +
  scale_y_continuous(trans = 'log', name = expression(RR["identical sequences"]),
                     breaks = c(yaxis_zero_value, 0.1, 1, 10, 1e2, 1e3, 1e4),
                     labels = c('0', expression(10^{-1}), expression(10^{0}), expression(10^{1}),
                                expression(10^{2}), expression(10^{3}), expression(10^{4}))) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 12)) +
  coord_flip()

plt_comp_RR_adjacent <- df_RR_counties %>%
  mutate(RR = ifelse(RR == 0., yaxis_zero_value, RR)) %>% 
  ggplot(aes(x = interaction(is_adjacent, is_same_county))) +
  geom_jitter(aes(y = RR),
              alpha = 0.25, height = 0., color = 'darkgrey', width = 0.3) +
  geom_boxplot(data = df_for_boxplot %>% mutate(n_pairs = 0.1),
               aes(ymin = y05, lower = y25, middle = y50,
                   upper = y75, ymax = y95),
               stat = "identity", fill = NA, width = 0.7) +
  geom_signif(stat="identity",
              data=data.frame(x = c(0.875, 1.875), xend = c(2.125, 3.125),
                              y = c(1e2, 5e3),
                              n_pairs = c(0.1, 0.1),
                              annotation=c("***", " *** ")),
              aes(x = x, xend = xend, 
                  y = y, yend = y, annotation = annotation),
              color = 'firebrick') +
  scale_x_discrete(name = '', breaks = c('0.TRUE', '1.FALSE', '0.FALSE'),
                   labels = c('Within county', 'Adjacent county', 'Non-adjacent county')) +
  scale_y_continuous(trans = 'log', name = expression(RR["identical sequences"]),
                     breaks = c(yaxis_zero_value, 0.1, 1, 10, 1e2, 1e3, 1e4),
                     labels = c('0', expression(10^{-1}), expression(10^{0}), expression(10^{1}),
                                expression(10^{2}), expression(10^{3}), expression(10^{4})),
                     expand = expansion(mult = c(0.18, 0.13))) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  coord_flip() +
  facet_grid(. ~ (n_pairs != 0.), scales = 'free',
             space='free')

plot(plt_comp_RR_adjacent)

# pdf('../plots/figure_space/comp_RR_adjacent.pdf', height = 2, width = 6)
# plot(plt_comp_RR_adjacent)
# dev.off()
# png('../plots/figure_space/comp_RR_adjacent.png', height = 2, width = 6, res = 350, units = 'in')
# plot(plt_comp_RR_adjacent)
# dev.off()

## Plot RR by distance between counties 
distance_max_to_plot <- 250

df_for_plot <- df_RR_counties %>% 
  filter(group_1 > group_2) %>% 
  mutate(modif_RR = ifelse(RR == 0., yaxis_zero_value, RR))

plt_RR_distance_2 <- df_for_plot %>% 
  ggplot(aes(x = distance_km)) +
  geom_point(aes(y = log(modif_RR)), alpha = 0.2) +
  geom_smooth(data = df_for_plot %>% mutate(n_pairs = 1.) %>% filter(RR > 0.), aes(y = log(RR)),
              color = 'firebrick', fill = 'firebrick', method = 'loess') +
  scale_x_continuous(name = 'Distance between counties\ncentroids (in km)',
                     breaks = seq(50, 450, 50),
                     expand = expansion(mult = c(0.05, 0.01))) +
  scale_y_continuous(name = expression(RR["identical sequences"]),
                     breaks = log(c(yaxis_zero_value, 0.1, 1., 10., 100.)),
                     labels = c(0, expression(10^{-1}), expression(10^{0}), expression(10^{1}), expression(10^{2})),
                     expand = expansion(add = c(0.1, 0.1))) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 12),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  facet_grid((n_pairs == 0.) ~ ., scales = 'free', space = 'free_y') +
  coord_cartesian(xlim = c(NA, distance_max_to_plot))

plot(plt_RR_distance)

# pdf('../plots/figure_space/RR_geog_distance.pdf', height = 3.0, width = 3.)
# plot(plt_RR_distance)
# dev.off()
# png('../plots/figure_space/RR_geog_distance.png', height = 3., width = 3.0,
#     res = 350, units = 'in')
# plot(plt_RR_distance)
# dev.off()


## Get the distance at which the association is no longer significant using a LOESS curve
mod_smooth <-  loess(data = df_for_plot %>% filter(RR > 0.), formula = log(RR) ~ distance_km)
tibble(distance_km = seq(0, 400, 1)) %>% 
  mutate(pred = predict(mod_smooth, newdata = tibble(distance_km = seq(0, 400, 1))),
         se = predict(mod_smooth, newdata = tibble(distance_km = seq(0, 400, 1)), se = T)$se.fit) %>% 
  mutate(pred_lower = pred - 1.96 * se, pred_upper = pred + 1.96 * se) %>% 
  filter(! is.na(pred)) %>% 
  summarise(distance_cross_0 = min(distance_km[pred < 0.]),
            lower_distance_cross_0 = min(distance_km[pred_lower < 0.]),
            upper_distance_cross_0 = min(distance_km[pred_upper < 0.]))
  
