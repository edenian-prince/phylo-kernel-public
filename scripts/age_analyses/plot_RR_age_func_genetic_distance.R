## This script reproduces figures depicting the relative risk of observing 
## identical sequences, sequences 1 mutation away, sequences 2 mutations away 
## and sequences 3 mutations away between two age groups.

library(tidyverse)
library(viridis)
my_pal_age <- viridis_pal(option = 'D', direction = -1)(5)[-1]

## Load relative risk of observing identical sequences between two age groups
df_RR_age <- readRDS('../results/RR_age/df_RR_age_0_mut_away.rds') %>% 
  bind_rows(readRDS('../results/RR_age/df_RR_age_1_mut_away.rds')) %>% 
  bind_rows(readRDS('../results/RR_age/df_RR_age_2_mut_away.rds')) %>% 
  bind_rows(readRDS('../results/RR_age/df_RR_age_3_mut_away.rds'))

## Load results from subsampling to compute uncertainty intervals
df_uncertainty_age <- Reduce('bind_rows', lapply(0:3, FUN = function(i_mut){
  readRDS(paste0('../results/RR_age/df_RR_uncertainty_age_', i_mut, '_mut_away.rds')) %>% 
    group_by(n_mutations, group_1, group_2) %>% 
    summarise(median_RR = median(RR), 
              lower_RR = quantile(RR, 0.025), 
              upper_RR = quantile(RR, 0.975))
}))
  

## Plot the relative risk of observing identical sequences
## between two age groups
plt_RR_intergenerational_mixing_func_genetic_distance <- df_RR_age %>% 
  ggplot(aes(x = group_1, colour = as.factor(n_mutations))) +
  geom_point(aes(y = RR)) +
  geom_line(aes(y = RR, group = n_mutations)) +
  geom_linerange(data = df_uncertainty_age, aes(ymin = lower_RR, ymax = upper_RR)) +
  facet_wrap(. ~ group_2, scales = 'free_y') +
  scale_x_discrete(name = 'Age group') +
  scale_y_continuous(name = expression(RR["identical sequences"])) +
  scale_colour_manual(values = my_pal_age, name = 'Genetic distance\n(in # of mutations)') +
  theme_classic() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1.),
        strip.text = element_text(size = 12),
        strip.background = element_rect(colour = 'white'),
        legend.position = 'bottom')

plot(plt_RR_intergenerational_mixing_func_genetic_distance)


# pdf('../plots/figure_age/RR_intergenerational_mixing_func_gen_distance.pdf', height = 7, width = 9)
# plot(plt_RR_intergenerational_mixing_func_genetic_distance)
# dev.off()
# png('../plots/figure_age/RR_intergenerational_mixing_func_gen_distance.png', height = 7, width = 9,
#     res = 350, units = 'in')
# plot(plt_RR_intergenerational_mixing_func_genetic_distance)
# dev.off()
