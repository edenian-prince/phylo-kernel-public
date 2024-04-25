library(tidyverse)
library(colorspace)
my_pal <- c(lighten('darkcyan', amount = 0.6), 'darkcyan', darken('darkcyan', amount = 0.6))

## Load relative risk of observing identical sequences between two age groups
## for different spatial scales
df_RR_age_all_pairs <- readRDS('../results/RR_age/df_RR_age_0_mut_away.rds')
df_RR_age_different_counties <- readRDS('../results/RR_age/df_RR_age_different_counties_0_mut_away.rds')
df_RR_age_different_postal_codes <- readRDS('../results/RR_age/df_RR_age_different_postal_codes_0_mut_away.rds') 

df_RR_all <- df_RR_age_all_pairs %>% mutate(type = 'All pairs') %>% 
  bind_rows(df_RR_age_different_counties %>% mutate(type = 'Only pairs in different counties')) %>% 
  bind_rows(df_RR_age_different_postal_codes %>% mutate(type = 'Only pairs in different postal codes')) %>% 
  mutate(type = factor(type, levels = c('All pairs', 'Only pairs in different postal codes', 'Only pairs in different counties')))

## Load results from subsampling to compute uncertainty intervals
df_uncertainty_age_all_pairs <- readRDS('../results/RR_age/df_RR_uncertainty_age_0_mut_away.rds') %>% 
  group_by(n_mutations, group_1, group_2) %>% 
  summarise(median_RR = median(RR), 
            lower_RR = quantile(RR, 0.025), 
            upper_RR = quantile(RR, 0.975)) %>% 
  mutate(type = 'All pairs')

df_uncertainty_age_different_counties <- readRDS('../results/RR_age/df_RR_uncertainty_age_different_counties_0_mut_away.rds') %>% 
  group_by(n_mutations, group_1, group_2) %>% 
  summarise(median_RR = median(RR), 
            lower_RR = quantile(RR, 0.025), 
            upper_RR = quantile(RR, 0.975)) %>% 
  mutate(type = 'Only pairs in different counties')

df_uncertainty_age_different_postal_codes <- readRDS('../results/RR_age/df_RR_uncertainty_age_different_postal_codes_0_mut_away.rds') %>% 
  group_by(n_mutations, group_1, group_2) %>% 
  summarise(median_RR = median(RR), 
            lower_RR = quantile(RR, 0.025), 
            upper_RR = quantile(RR, 0.975)) %>% 
  mutate(type = 'Only pairs in different postal codes')

df_uncertainty <- df_uncertainty_age_all_pairs %>% 
  bind_rows(df_uncertainty_age_different_counties) %>% 
  bind_rows(df_uncertainty_age_different_postal_codes) %>% 
  mutate(type = factor(type, levels = c('All pairs', 'Only pairs in different postal codes', 'Only pairs in different counties')))

## Plot the relative risk of observing identical sequences
## between two age groups across different spatial scales
plt_age_space_all_groups <- df_RR_all %>% 
  ggplot(aes(x = group_1, colour = type, group = type)) +
  geom_line(aes(y = RR), position = position_dodge(0.5), alpha = 0.8) +
  geom_linerange(data = df_uncertainty,
                 aes(ymin = lower_RR, ymax = upper_RR, group = interaction(type, group_1)),
                 position = position_dodge(0.5)) +
  geom_point(aes(y = RR), 
             position = position_dodge(0.5)) +
  scale_x_discrete(name = 'Age group') +
  scale_y_continuous(name = expression(RR["identical sequences"])) +
  scale_colour_manual(name = '', 
                      values = rev(my_pal)) +
  theme_classic() +
  facet_wrap(. ~ group_2, scales = 'free_y') +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1.),
        legend.text = element_text(size = 12),
        legend.background = element_blank(),
        strip.text = element_text(size = 12),
        strip.background = element_rect(colour = 'white'),
        legend.position = 'top') 

plot(plt_age_space_all_groups)

# pdf('../plots/figure_age/RR_age_geography_all.pdf', height = 7, width = 10)
# plot(plt_age_space_all_groups)
# dev.off()
# png('../plots/figure_age/RR_age_geography_all.png', height = 7, width = 10, res = 350, units = 'in')
# plot(plt_age_space_all_groups)
# dev.off()

# Only for children
plt_age_space_children <- df_RR_all %>% 
  filter(group_2 == '0-9y') %>% 
  ggplot(aes(x = group_1, colour = type, group = type)) +
  geom_line(aes(y = RR), position = position_dodge(0.35), alpha = 0.8) +
  geom_linerange(data = df_uncertainty %>% filter(group_2 == '0-9y'),
                 aes(ymin = lower_RR, ymax = upper_RR, group = interaction(type, group_1)),
                 position = position_dodge(0.35)) +
  geom_point(aes(y = RR), 
             position = position_dodge(0.35)) +
  scale_x_discrete(name = 'Age group') +
  scale_y_continuous(name = expression(RR["identical sequences"])) +
  scale_colour_manual(name = '', 
                      values = rev(my_pal)) +
  theme_classic() +
  facet_wrap(. ~ group_2, scales = 'free_y') +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1.),
        legend.text = element_text(size = 12),
        legend.background = element_blank(),
        strip.text = element_text(size = 12),
        strip.background = element_rect(colour = 'white'),
        legend.position = 'top') 

# pdf('../plots/figure_age/RR_age_geography_children.pdf',
#     height = 3.5, width = 4.2)
plot(plt_age_space_children)
#dev.off()
# png('../plots/figure_age/RR_age_geography_children.png',
#     height = 3.5, width = 4.2, res = 350, units = 'in')
# plot(plt_age_space_children)
# dev.off()


df_RR_all %>% filter(group_1 == '80y+', group_1 == group_2)
df_uncertainty %>% filter(group_1 == '80y+', group_1 == group_2)
