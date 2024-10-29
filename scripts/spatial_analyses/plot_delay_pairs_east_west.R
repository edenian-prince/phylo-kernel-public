## This scripts reproduces the figure depicting the distribution
## of the delay between the timing of sequences collection in Western and
## Eastern WA (time_east - time_west) within pairs of identical sequences
## observed in both Eastern amd Western WA.

library(tidyverse)

## Load dataframe with delay between pairs by wave
df_for_plot_delay <- read_csv('../../results/direction_transmission/delay_pairs_east_west.csv')

## Plot delay distribution 
plt_delay_east_west <- df_for_plot_delay %>%
  ggplot(aes(x = delay_east_west)) +
  geom_histogram(binwidth = 1., fill = 'darkslateblue') +
  scale_x_continuous(name = 'Delay between collection in Eastern and Western WA (in days)') +
  scale_y_continuous(name = 'Number of pairs') +
  scale_colour_manual() +
  facet_wrap(. ~ period, scales = 'free') +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12, colour = 'white'),
        strip.background = element_rect(fill = 'gray22'))

plot(plt_delay_east_west)

## Look at key summary statistics
df_for_plot_delay %>% 
  group_by(period) %>% 
  summarise(median_delay = median(delay_east_west),
            mean_delay = mean(delay_east_west),
            sd_delay = sd(delay_east_west))

# pdf('../../plots/figure_space/delay_east_west.pdf', height = 4.5, width = 7.5)
# plot(plt_delay_east_west)
# dev.off()