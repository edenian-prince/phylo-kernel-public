## This script reproduces figures exploring the timing of sequences collection between age groups
## within pairs of identical sequences.

library(tidyverse)
library(RColorBrewer)

## Load timing of identical sequence collection information
df_timing_pairs <- read_csv('../../results/direction_transmission/df_timing_pairs_age.csv')
df_timing_pairs_symptom_onset <- read_csv('../../results/direction_transmission/df_timing_pairs_symptom_onset_age.csv')

## Compute proportion of pairs first observed in an age group with uncertainty
df_timing_pairs <- df_timing_pairs %>% 
  group_by(name_wave, age_decade_1, age_decade_2) %>% 
  mutate(prop_1_before_2 = n_1_before_2/n_pairs,
         lower_prop = prop.test(x = n_1_before_2, n = n_pairs, conf.level = 0.95)$conf.int[1],
         upper_prop = prop.test(x = n_1_before_2, n = n_pairs, conf.level = 0.95)$conf.int[2]) %>% 
  ungroup()

## Compute earliness score
df_groups_driving <- df_timing_pairs %>% 
  group_by(name_wave, age_decade_1) %>% 
  summarise(n_1_before_2 = sum(n_1_before_2),
            n_pairs = sum(n_pairs),
            prop_1_before_2 = n_1_before_2/n_pairs,
            lower_prop = prop.test(x = n_1_before_2, n = n_pairs, conf.level = 0.95)$conf.int[1],
            upper_prop = prop.test(x = n_1_before_2, n = n_pairs, conf.level = 0.95)$conf.int[2])

## Compute earliness score using imputed onset dates
df_groups_driving_symptom_onset <- df_timing_pairs_symptom_onset %>% 
  group_by(period, age_decade_1, i_imput)%>% 
  summarise(n_1_before_2 = sum(n_1_before_2),
            n_pairs = sum(n_pairs),
            prop_1_before_2 = n_1_before_2/n_pairs,
            lower_prop = prop.test(x = n_1_before_2, n = n_pairs, conf.level = 0.95)$conf.int[1],
            upper_prop = prop.test(x = n_1_before_2, n = n_pairs, conf.level = 0.95)$conf.int[2]) %>% 
  group_by(period, age_decade_1) %>% 
  summarise(median_earliness_score = median(prop_1_before_2), 
            lower_lower_prop = min(lower_prop),
            upper_upper_prop = max(upper_prop)) 
  
## Compute proportion of pairs with onset first in a given age group
df_timing_pairs_symptom_onset <- df_timing_pairs_symptom_onset %>% 
  group_by(period, age_decade_1, age_decade_2) %>% 
  summarise(median_prop_1_before_2 = median(prop_1_before_2)) %>% 
  ungroup()

vec_name_periods <- c('Wave 4\nMar 2021 - Jun 2021',
                      'Wave 5\nJul 2021 - Nov 2021',
                      'Wave 6\nDec 2021 - Feb 2022',
                      'Wave 7\nMar 2022 - Aug 2022')


## Visualise the results
plt_timing_sequences_age <- df_timing_pairs %>% 
  mutate(id_wave = as.numeric(substr(name_wave, 6, nchar(name_wave)))) %>% 
  mutate(name_period = vec_name_periods[id_wave - 3]) %>% 
  ggplot(aes(x = age_decade_1, y = age_decade_2, fill = prop_1_before_2)) +
  geom_tile() +
  theme_bw() +
  scale_x_discrete(name = 'Age group A',
                   expand = expansion(mult = c(0., 0.))) +
  scale_y_discrete(name = 'Age group B',
                   expand = expansion(mult = c(0., 0.))) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, 'PiYG')),
                       limits = c(0.22, 0.78),
                       breaks = seq(0., 1., 0.1),
                       name = 'Proportion of pairs\nof identical sequences\nfirst collected in A') +
  facet_grid(. ~ name_period) +
  coord_fixed() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
  )

plot(plt_timing_sequences_age)

# pdf('../plots/figure_age/matrix_direction_transmission.pdf', height = 4., width = 10.)
# plot(plt_timing_sequences_age)
# dev.off()
# png('../plots/figure_age/matrix_direction_transmission.png', height = 4., width = 10.,
#     res = 350, units = 'in')
# plot(plt_timing_sequences_age)
# dev.off()

plt_earliness_score <- df_groups_driving %>% 
  mutate(id_wave = as.numeric(substr(name_wave, 6, nchar(name_wave)))) %>% 
  mutate(name_period = vec_name_periods[id_wave - 3]) %>%  
  ggplot(aes(x = age_decade_1)) +
  geom_hline(yintercept = 0.5, linetype = 'dashed', color = 'darkgrey') +
  geom_point(aes(y = prop_1_before_2, colour = (prop_1_before_2 > 0.5))) +
  geom_linerange(aes(ymin = lower_prop, ymax = upper_prop, colour = (prop_1_before_2 > 0.5))) +
  facet_grid(. ~ name_period) +
  scale_x_discrete(name = '') +
  scale_y_continuous(name = 'Earliness score') +
  scale_colour_manual(values = brewer.pal(11, 'PiYG')[c(10, 2)]) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(size = 11),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 12),
        legend.position = 'none'
  ) 

plot(plt_earliness_score)

# pdf('../plots/figure_age/earliness_score.pdf', height = 2., width = 7.5)
# plot(plt_earliness_score)
# dev.off()

#################### Same thing but using inferred symptom onset dates
plt_timing_sequences_age_symptom_onset <- df_timing_pairs_symptom_onset %>% 
  mutate(id_wave = as.numeric(substr(period, 6, nchar(period)))) %>% 
  mutate(name_period = vec_name_periods[id_wave - 3]) %>% 
  ggplot(aes(x = age_decade_1, y = age_decade_2, fill = median_prop_1_before_2)) +
  geom_tile() +
  theme_bw() +
  scale_x_discrete(name = 'Age group A',
                   expand = expansion(mult = c(0., 0.))) +
  scale_y_discrete(name = 'Age group B',
                   expand = expansion(mult = c(0., 0.))) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, 'PiYG')),
                       limits = c(0.25, 0.75),
                       breaks = seq(0., 1., 0.1),
                       name = 'Proportion of pairs\nof identical sequences\nwith onset first in A') +
  facet_grid(. ~ name_period) +
  coord_fixed() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
  )

plot(plt_timing_sequences_age_symptom_onset)

# pdf('../plots/figure_age/matrix_direction_transmission_symptom.pdf', height = 4., width = 10.)
# plot(plt_timing_sequences_age_symptom_onset)
# dev.off()
# png('../plots/figure_age/matrix_direction_transmission_symptom.png', height = 4., width = 10.,
#     res = 350, units = 'in')
# plot(plt_timing_sequences_age)
# dev.off()

## Plot earliness score
plt_earliness_score_symptom_onset <- df_groups_driving_symptom_onset %>% 
  mutate(id_wave = as.numeric(substr(period, 6, nchar(period)))) %>% 
  mutate(name_period = vec_name_periods[id_wave - 3]) %>%  
  ggplot(aes(x = age_decade_1)) +
  geom_hline(yintercept = 0.5, linetype = 'dashed', color = 'darkgrey') +
  geom_point(aes(y = median_earliness_score, colour = (median_earliness_score > 0.5))) +
  geom_linerange(aes(ymin = lower_lower_prop, ymax = upper_upper_prop, colour = (median_earliness_score > 0.5))) +
  facet_grid(. ~ name_period) +
  scale_x_discrete(name = '') +
  scale_y_continuous(name = 'Earliness score') +
  scale_colour_manual(values = brewer.pal(11, 'PiYG')[c(10, 2)]) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(size = 11),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 12),
        legend.position = 'none'
  ) 

plot(plt_earliness_score_symptom_onset)

# pdf('../plots/figure_age/earliness_score_sympton_onset.pdf', height = 2., width = 7.5)
# plot(plt_earliness_score_symptom_onset)
# dev.off()
