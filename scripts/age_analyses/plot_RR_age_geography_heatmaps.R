## This script reproduces heatmaps depicting the relative risk of observing 
## identical sequences between two age groups either by using all pairs of identical sequences,
## only pairs observed in differen postal codes and only pairs in different counties.


library(tidyverse)
library(colorspace)
library(RColorBrewer)
my_pal <- c(lighten('darkcyan', amount = 0.6), 'darkcyan', darken('darkcyan', amount = 0.6))

## Load relative risk of observing identical sequences between two age groups
## for different spatial scales
df_RR_age_all_pairs <- readRDS('../results/RR_age/df_RR_age_0_mut_away.rds')
df_RR_age_different_counties <- readRDS('../results/RR_age/df_RR_age_different_counties_0_mut_away.rds')
df_RR_age_different_postal_codes <- readRDS('../results/RR_age/df_RR_age_different_postal_codes_0_mut_away.rds') 

df_RR_all <- df_RR_age_all_pairs %>% mutate(type = 'All pairs') %>% 
  bind_rows(df_RR_age_different_counties %>% mutate(type = 'Only pairs in different counties')) %>% 
  bind_rows(df_RR_age_different_postal_codes %>% mutate(type = 'Only pairs in different postal codes'))  %>% 
  mutate(type = case_when(type == 'Only pairs in different counties' ~ 'Only pairs in\ndifferent counties',
                          type == 'Only pairs in different postal codes' ~ 'Only pairs in\ndifferent postal codes',
                          T ~ 'All pairs')) %>% 
  mutate(type = factor(type, levels = c('All pairs', 'Only pairs in\ndifferent postal codes', 'Only pairs in\ndifferent counties'))) %>% 
  mutate(log10_RR = log10(RR))

log10_RR_cap_value <- 0.035
log10_RR_cap_value_different_postal_codes <- 0.019
log10_RR_cap_value_different_counties <- 0.01


log10_RR_cap_value <- log10(1.1)
log10_RR_cap_value_different_postal_codes <- log10(1.1)
log10_RR_cap_value_different_counties <- log10(1.1)

df_RR_all %>% 
  filter(type == 'All pairs') %>% 
  arrange(- log10_RR)

df_RR_all %>% 
  filter(type == 'Only pairs in\ndifferent postal codes') %>% 
  arrange(- log10_RR)

df_RR_all %>% 
  filter(type == 'Only pairs in\ndifferent counties') %>% 
  arrange(log10_RR)

df_RR_all %>% 
  filter(type == 'All pairs') %>%
  mutate(is_greater = log10_RR > log10_RR_cap_value) %>% 
  mutate(log10_RR_modif = ifelse(log10_RR > log10_RR_cap_value, log10_RR_cap_value, log10_RR),
         log10_RR_modif = ifelse(log10_RR_modif < - log10_RR_cap_value, - log10_RR_cap_value, log10_RR_modif)) %>% 
  ggplot(aes(x = group_1, y = group_2, fill = log10_RR_modif)) +
  geom_tile() +
  scale_fill_gradientn(name = expression(RR['identical sequences']),
                       colors = brewer.pal(11, 'BrBG'),
                       limits = c(- log10_RR_cap_value, log10_RR_cap_value),
                       breaks = log10(c(0.9, 0.95, 1.0, 1.05, 1.1)),
                       labels = c(0.9, 0.95, 1.0, 1.05, 1.1)) +
  coord_fixed() +
  scale_x_discrete(name = '', expand = expansion(mult = c(0., 0.0))) +
  scale_y_discrete(name = '', expand = expansion(mult = c(0., 0.0))) +
  theme(axis.text.x = element_text(angle = 90),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))



panel_heatmaps <- df_RR_all %>% 
  mutate(log10_RR_modif = ifelse(log10_RR > log10_RR_cap_value, log10_RR_cap_value, log10_RR),
         log10_RR_modif = ifelse(log10_RR_modif < - log10_RR_cap_value, - log10_RR_cap_value, log10_RR_modif)) %>% 
  ggplot(aes(x = group_1, y = group_2, fill = log10_RR_modif)) +
  geom_tile() +
  scale_fill_gradientn(name = expression(RR['identical sequences']),
                       colors = brewer.pal(11, 'BrBG'),
                       limits = c(- log10_RR_cap_value, log10_RR_cap_value),
                       breaks = log10(c(0.9, 0.95, 1.0, 1.05, 1.1)),
                       labels = c(0.9, 0.95, 1.0, 1.05, 1.1)) +
  coord_fixed() +
  scale_x_discrete(name = '', expand = expansion(mult = c(0., 0.0))) +
  scale_y_discrete(name = '', expand = expansion(mult = c(0., 0.0))) +
  facet_wrap(. ~ type) +
  theme(axis.text.x = element_text(angle = 90),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill = 'white'),
        strip.text = element_text(size = 12),
        legend.position = 'bottom',
        legend.key.width = unit(0.9, 'cm'),
        legend.key.height = unit(0.6, 'cm'))

# pdf('../plots/figure_age/heatmaps_age_geography.pdf',
#     height = 4.5, width = 10)
plot(panel_heatmaps)
#dev.off()
