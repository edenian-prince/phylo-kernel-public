## This script reproduces figures depicting the proportion of pairs first observed in Western WA
## among pairs observed in Eastern and Western WA. This was used in the manuscript to assess
## the typical transmission direction between groups.

library(tidyverse)

## Load timing of identical sequence collection information
df_timing_pairs <- readRDS('../results/direction_transmission/df_timing_pairs_county.rds')
df_timing_pairs_symptom_onset <- readRDS('../results/direction_transmission/df_timing_pairs_county_imput_symptom_onset.rds')

## Load characteristics of WA counties
df_char_counties <- read.csv('../data/maps/county_wa.csv') %>% as_tibble() %>% 
  select(county, is_west)

## Get the proportion of pairs first observed in Eastern or Western WA across the different waves
df_timing_pairs <- df_timing_pairs %>% 
  group_by(period) %>% 
  mutate(prop_west_before_east = n_west_before_east/n_pairs,
         lower_prop = prop.test(x = n_west_before_east, n = n_pairs, conf.level = 0.95)$conf.int[1],
         upper_prop = prop.test(x = n_west_before_east, n = n_pairs, conf.level = 0.95)$conf.int[2]) %>% 
  ungroup() %>% 
  select(- n_west_before_east, n_pairs)

df_timing_pairs_symptom_onset <- df_timing_pairs_symptom_onset %>% 
  group_by(period) %>% 
  summarise(prop_west_before_east = median(prop_west_before_east),
            lower_prop = min(lower_prop),
            upper_prop = max(upper_prop))


## Plot the proportion of pairs consistent with a transmission direction E -> W
col_west <- 'dodgerblue3'
col_east <- 'coral1'
col_border <- 'darkslateblue'

label_waves <- c('Wave 4\nMar-Jun 2021',
                 'Wave 5\nJul-Nov 2021',
                 'Wave 6\nDec 2021-Feb 2022',
                 'Wave 7\nMar 2022-Aug 2022')

plt_timing_sequences_with_symptom_onset <- df_timing_pairs %>% mutate(type = 'Using sequence collection date') %>% 
  bind_rows(df_timing_pairs_symptom_onset %>% mutate(type = 'Using symptom onset date')) %>% 
  mutate(id_wave = as.numeric(substr(period, start = 6, stop = nchar(period)))) %>% 
  ggplot(aes(x = -id_wave)) +
  geom_point(aes(y = prop_west_before_east, group = type, shape = type),
             position = position_dodge(0.4)) +
  geom_linerange(aes(ymin =  lower_prop, ymax =  upper_prop, group = type),
                 position = position_dodge(0.4)) +
  coord_flip(ylim = c(0.29, 0.71),
             xlim = c(-7.2, -3.8)) +
  scale_x_continuous(name = '', breaks = seq(-7, -4),
                     labels = rev(label_waves)
                     ) +
  scale_y_continuous(name = 'Proportion of pairs of identical sequences\nfirst collected in Western WA',
                     breaks = seq(0., 1., 0.1)) +
  scale_shape_manual(name = '', values = c(16, 17)) +
  geom_hline(linetype = 'dashed', color = col_border, yintercept = 0.5) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.grid.minor = element_blank())

plot(plt_timing_sequences_with_symptom_onset + theme(legend.position = 'top', legend.direction = 'vertical'))

# pdf('../plots/figure_space/timing_seq_east_west_with_symptom_onset.pdf', height = 3.5, width = 6.)
# plot(plt_timing_sequences_with_symptom_onset + theme(legend.position = 'top', legend.direction = 'vertical'))
# dev.off()
# png('../plots/figure_space/timing_seq_east_west_with_symptom_onset.png', height = 3., width = 5.,
#     res = 350, units = 'in')
# plot(plt_timing_sequences_with_symptom_onset + theme(legend.position = 'top', legend.direction = 'vertical'))
# dev.off()
