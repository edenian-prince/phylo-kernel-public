## This scripts enables to depict the median delay between the dates of sequence
## collection within pairs of identical sequences by pairs of age groups.
## This is done by considering all pairs of identical sequences or only pairs
## that are not observed on the same day.

library(tidyverse)

## Load dataframe with median delays between pairs of ID seq by pairs of age groups
df_timing_sequences_within_pairs <- read_csv('../../results/direction_transmission/delay_pairs_age.csv')
df_timing_sequences_within_pairs_remove_delay_of_0 <- read_csv('../../results/direction_transmission/delay_pairs_age_remove_0_delays.csv')

## Display these median delay on heatmaps
plt_timing_all_pairs <- df_timing_sequences_within_pairs %>% 
  ggplot(aes(x = age_decade_2, y = age_decade_1, fill = as.numeric(median_delay))) +
  geom_tile() +
  geom_text(aes(label =  round(as.numeric(median_delay), 1))) +
  scale_x_discrete(name = 'Age group A',
                   expand = expansion(mult = c(0., 0.))) +
  scale_y_discrete(name = 'Age group B',
                   expand = expansion(mult = c(0., 0.))) +
  scale_fill_gradientn(colors = rev(brewer.pal(11, 'RdBu')),
                       name = 'Median delay between sequence collection\nin age group A and age group B (in days)') +
  facet_wrap(. ~ name_wave, nrow = 1) +
  coord_fixed() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'bottom'
  )

plt_timing_only_pairs_different_days <- df_timing_sequences_within_pairs_remove_delay_of_0 %>% 
  ggplot(aes(x = age_decade_2, y = age_decade_1, fill = as.numeric(median_delay))) +
  geom_tile() +
  geom_text(aes(label =  round(as.numeric(median_delay), 1))) +
  scale_x_discrete(name = 'Age group A',
                   expand = expansion(mult = c(0., 0.))) +
  scale_y_discrete(name = 'Age group B',
                   expand = expansion(mult = c(0., 0.))) +
  scale_fill_gradientn(colors = rev(brewer.pal(11, 'RdBu')),
                       name = 'Median delay between sequence collection\nin age group A and age group B (in days)') +
  facet_wrap(. ~ name_wave, nrow = 1) +
  coord_fixed() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'bottom'
  )

## Make panel
panel_median_delay <- 
  ggarrange(plt_timing_all_pairs + ggtitle('All pairs'),
            plt_timing_only_pairs_different_days + ggtitle('Only pairs on different days'), 
            nrow = 2,
            labels = 'AUTO')

# png('../../figures/figure_age/median_delay_pairs_id_sequences_age.png',
#     height = 11, width = 14, units = 'in', res = 350)
plot(panel_median_delay)
#dev.off()
