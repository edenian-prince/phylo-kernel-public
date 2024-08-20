## This script reproduces figures depicting the relative risk of observing 
## identical sequences between two age groups.

library(tidyverse)
library(viridis)
my_pal_age <- viridis_pal(option = 'A', direction = -1)(10)[-1]


## Load relative risk of observing identical sequences between two age groups
df_RR_age <- read_csv('../../results/RR_age/df_RR_age_0_mut_away.csv')

## Load results from subsampling to compute uncertainty intervals
df_uncertainty_age <- readRDS('../../results/RR_age/df_RR_uncertainty_age_0_mut_away.rds') %>% 
  group_by(n_mutations, group_1, group_2) %>% 
  summarise(median_RR = median(RR), 
            lower_RR = quantile(RR, 0.025), 
            upper_RR = quantile(RR, 0.975))

## Plot the relative risk of observing identical sequences between two age groups
plt_RR_intergenerational_mixing <- df_RR_age %>% 
  ggplot(aes(x = group_1, colour = as.factor(group_2))) +
  geom_point(aes(y = RR)) +
  geom_line(aes(y = RR, group = group_2)) +
  geom_linerange(data = df_uncertainty_age, aes(ymin = lower_RR, ymax = upper_RR)) +
  facet_wrap(. ~ group_2, scales = 'free_y') +
  scale_x_discrete(name = 'Age group') +
  scale_y_continuous(name = expression(RR["identical sequences"])) +
  scale_colour_manual(values = my_pal_age) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1.),
        strip.text = element_text(size = 12),
        strip.background = element_rect(colour = 'white'),
        legend.position = 'none')

plot(plt_RR_intergenerational_mixing)


# pdf('../plots/figure_age/RR_intergenerational_mixing.pdf', height = 6, width = 9)
# plot(plt_RR_intergenerational_mixing)
# dev.off()
# png('../plots/figure_age/RR_intergenerational_mixing.png', height = 6, width = 9,
#     res = 350, units = 'in')
# plot(plt_RR_intergenerational_mixing)
# dev.off()

## Only 3 groups
plt_RR_intergenerational_mixing_3_groups <- df_RR_age %>% 
  filter(group_2 %in% c('0-9y', '10-19y', '40-49y')) %>% 
  ggplot(aes(x = group_1, colour = as.factor(group_2))) +
  geom_point(aes(y = RR)) +
  geom_line(aes(y = RR, group = group_2)) +
  geom_linerange(data = df_uncertainty_age%>% 
                   filter(group_2 %in% c('0-9y', '10-19y', '40-49y')),
                 aes(ymin = lower_RR, ymax = upper_RR)) +
  facet_wrap(. ~ group_2, scales = 'free_y') +
  scale_x_discrete(name = 'Age group') +
  scale_y_continuous(name = expression(RR["identical sequences"])) +
  scale_colour_manual(values = my_pal_age[c(1, 2, 5)]) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1.),
        strip.text = element_text(size = 12),
        strip.background = element_rect(colour = 'white'),
        legend.position = 'none')

plot(plt_RR_intergenerational_mixing_3_groups)

# pdf('../plots/figure_age/RR_intergenerational_mixing_3_groups.pdf', height = 2.5, width = 9)
# plot(plt_RR_intergenerational_mixing_3_groups)
# dev.off()
# png('../plots/figure_age/RR_intergenerational_mixing_3_groups.png', height = 2.5, width = 9,
#     res = 350, units = 'in')
# plot(plt_RR_intergenerational_mixing_3_groups)
# dev.off()
