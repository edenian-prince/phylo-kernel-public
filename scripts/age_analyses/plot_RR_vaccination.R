## This script reproduces Figure S23 exploring a potential signal for
## preferential mixing of individuals within the same vaccination group.
## As we are interested in isolating this effect from the effect of age,
## we only focus on pairs observed within the same age group.
## Therefore, we look at mixing between vaccination groups within the same age group.

library(tidyverse)
library(RColorBrewer)
library(ggpubr)

## Define age groups
vec_age_groups <- c('0-9y', '10-19y', '20-29y', '30-39y', '40-49y', '50-59y', '60-69y', '70-79y', '80y+')

## Load RR between vaccination groups
df_RR_vacc <- read_csv('../../results/RR_vacc/df_RR_vacc_0_mut_away.csv')


## Tidy vaccination group names
df_RR_for_plot <- df_RR_vacc %>% 
  mutate(group_1 = case_when(group_1 == 'Completed primary series' ~ 'Vaccinated',
                             group_1 == 'Completed primary series + booster' ~ 'Boosted',
                             group_1 == 'No valid vaccination record' ~ 'Unvaccinated'),
         group_2 = case_when(group_2 == 'Completed primary series' ~ 'Vaccinated',
                             group_2 == 'Completed primary series + booster' ~ 'Boosted',
                             group_2 == 'No valid vaccination record' ~ 'Unvaccinated'),
         group_1 = factor(group_1, levels = c('Unvaccinated', 'Vaccinated', 'Boosted')),
         group_2 = factor(group_2, levels = c('Unvaccinated', 'Vaccinated', 'Boosted'))) %>%
  mutate(group_plot = case_when(group_1 == 'Boosted' & group_2 == 'Boosted' ~ 'Between boosted',
                                group_1 == 'Vaccinated' & group_2 == 'Vaccinated' ~ 'Between vaccinated',
                                group_1 == 'Unvaccinated' & group_2 == 'Unvaccinated' ~ 'Between unvaccinated',
                                group_1 == 'Vaccinated' & group_2 == 'Boosted' ~ 'Vaccinated/Boosted',
                                group_1 == 'Boosted' & group_2 == 'Vaccinated' ~ 'Vaccinated/Boosted',
                                group_1 == 'Vaccinated' & group_2 == 'Unvaccinated' ~ 'Unvaccinated/Vaccinated',
                                group_1 == 'Unvaccinated' & group_2 == 'Vaccinated' ~ 'Unvaccinated/Vaccinated',
                                group_1 == 'Unvaccinated' & group_2 == 'Boosted' ~ 'Unvaccinated/Boosted',
                                group_1 == 'Boosted' & group_2 == 'Unvaccinated' ~ 'Unvaccinated/Boosted'))

## Remove combinations of age groups and vaccination groups
## that are not relevant for specific time periods 
## (e.g. children aged 0-9y were not eligible for vaccination during rhe fourth wave)

df_RR_for_plot <- df_RR_for_plot %>% 
  filter(i_period >= 3 | (age_decade != '0-9y'), i_period >= 2 | (age_decade != '10-19y'),
         i_period >= 3 | (group_1 != 'Boosted'), i_period >= 3 | (group_2 != 'Boosted'),
         type != 'Only pairs in different counties') %>% 
  group_by(group_1, i_period, age_decade, type) %>% 
  mutate(RR_within_group = RR[group_1 == group_2],
         ratio_RR_RR_within_group = RR / RR_within_group,
         ratio_RR_within_group_RR = RR_within_group / RR) %>% 
  ungroup() %>% 
  filter(group_1 != group_2) %>% 
  mutate(type = ifelse(type == 'Only pairs in different postal_codes', 'Only pairs in different postal codes', type))


## Define boxplots characteristics for the figure
df_for_boxplot <- df_RR_for_plot %>% 
  group_by(type, i_period) %>% 
  summarise(y00 = quantile(ratio_RR_within_group_RR, 0.025),
            y25 = quantile(ratio_RR_within_group_RR, 0.25),
            y50 = quantile(ratio_RR_within_group_RR, 0.5),
            y75 = quantile(ratio_RR_within_group_RR, 0.75),
            y100 = quantile(ratio_RR_within_group_RR, 0.975))


## Plot of the ratio of the RR within a vaccination group and between vaccination groups
## Values greater than 1 suggest that individuals tend to preferentially transmit to individuals
## within the same vaccination group

ratio_RR_vacc_group_over_time <- df_RR_for_plot %>%   
  ggplot(aes(x = as.factor(i_period))) +
  geom_hline(yintercept = 1., linetype = 'dashed', color = 'darkgrey') +
  geom_boxplot(data = df_for_boxplot,
               aes(group = interaction(type, i_period),
                   ymin = y00, lower = y25, middle = y50, upper = y75, ymax = y100,
                   colour = type), fill = NA,
               stat = 'identity', 
               position = position_dodge(0.7),
               width = 0.6) +
  geom_point(aes(y = ratio_RR_within_group_RR, colour = type),
             position = position_jitterdodge(jitter.height = 0., 
                                             jitter.width = 0.2, 
                                             dodge.width = 0.7),
             alpha = 0.2) +
  scale_colour_manual(name = '', values = c('darkslateblue', 'orange2')) +
  scale_x_discrete(name = 'Wave', breaks = 1:4, labels = (1:4) + 3) +
  scale_y_continuous(name = 'RR within vaccination group / RR', trans = 'log',
                     breaks = c(seq(0.1, 0.9, 0.1), seq(1., 9., 1.), seq(10., 100., 10.), 200),
                     labels = c(0.1, 0.2, rep('', 2), 0.5, rep('', 4),
                                1, 2, rep('', 2), 5, rep('', 4),
                                10, 20, rep('', 2), 50, rep('', 4), 100, 200
                     )) +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = 'bottom',
        panel.grid.minor = element_blank())

## Same figure but zoomed
ratio_RR_vacc_group_over_time_zoomed <- df_RR_for_plot %>%   
  ggplot(aes(x = as.factor(i_period))) +
  geom_hline(yintercept = 1., linetype = 'dashed', color = 'darkgrey') +
  geom_boxplot(data = df_for_boxplot,
               aes(group = interaction(type, i_period),
                   ymin = y00, lower = y25, middle = y50, upper = y75, ymax = y100,
                   colour = type), fill = NA,
               stat = 'identity', 
               position = position_dodge(0.7),
               width = 0.6) +
  geom_point(aes(y = ratio_RR_within_group_RR, colour = type),
             position = position_jitterdodge(jitter.height = 0., 
                                             jitter.width = 0.2, 
                                             dodge.width = 0.7),
             alpha = 0.2) +
  scale_colour_manual(name = '', values = c('darkslateblue', 'orange2')) +
  scale_x_discrete(name = 'Wave', breaks = 1:4, labels = (1:4) + 3) +
  scale_y_continuous(name = 'RR within vaccination group / RR', trans = 'log',
                     breaks = c(seq(0.1, 0.9, 0.1), seq(1., 9., 1.), seq(10., 100., 10.), 200),
                     labels = c(0.1, 0.2, rep('', 2), 0.5, rep('', 4),
                                1, 2, rep('', 2), 5, rep('', 4),
                                10, 20, rep('', 2), 50, rep('', 4), 100, 200
                     )) +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = 'bottom') +
  coord_cartesian(ylim = c(0.5, 5))


## Make panel
panel_ratio_RR_vacc_group_over_time <- ggarrange(ratio_RR_vacc_group_over_time + ggtitle(''), 
                                                 ratio_RR_vacc_group_over_time_zoomed + ggtitle('Zoomed'),
                                                 ncol = 2, labels = 'AUTO',
                                                 common.legend = T)


#pdf('../../plots/RR_vacc/panel_ratio_RR_within_vaccination_group_RR.pdf', height = 4., width = 8.)
plot(panel_ratio_RR_vacc_group_over_time)
#sdev.off()
