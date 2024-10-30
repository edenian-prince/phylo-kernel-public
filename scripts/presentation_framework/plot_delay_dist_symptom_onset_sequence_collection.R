## This script depicts the delay distribution between sequence collection 
## and symptoms onset by wave, age and Eastern / Western membership
## This is used to impute symptom onset dates when unavailable for the
## direction analysis

library(tidyverse)

## Load delay distribution
delay_dist <- read_csv('../../results/direction_transmission/delay_distribution_symptom_onset_sequence_collection.csv')

## Define plot colours
col_west <- 'dodgerblue3'
col_east <- 'coral1'

## Plot delay distribution
plt_delay_dist <- delay_dist %>% 
  ggplot(aes(x = delay_sequence_collection_symptom_onset, y = prop, group = is_west, fill = is_west)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(age_decade ~ period, scales = 'free') +
  scale_x_continuous(name = 'Delay between symptom onset and sequence collection (in days)') +
  scale_y_continuous(name = 'Proportion') +
  scale_fill_manual(name = '', breaks = c(T, F), 
                    labels = c('Western WA', 'Eastern WA'), 
                    values = c(col_west, col_east)) +
  my_theme_bw() +
  theme(legend.position = 'top')


# pdf('../../figures/figure_comparison_symptom_onset/delay_dist_by_region_age_period.pdf', 
#     height = 10, width = 8)
plot(plt_delay_dist)
#dev.off()