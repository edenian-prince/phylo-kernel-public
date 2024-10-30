## This script compare the relative risk of observing identical
## sequences within counties with zip with female prisons.

library(tidyverse)
library(RColorBrewer)
library(ggpubr)

## Load dataframe (group_1 corresponds to Mason zcta and group_2 to Pierce zcta)
df_RR_zcta_mason_pierce <- read_csv('../../results/RR_zcta_prison/df_RR_zctas_mason_pierce.csv') %>% 
  mutate(zcta_1 = as.character(group_1), zcta_2 = as.character(group_2))

## Load zcta adjacency status
df_adj_zcta <- readRDS('../../data/maps/df_adj_zcta.rds') %>% 
  as_tibble() %>% 
  mutate(zcta_1 = as.character(zcta_1),
         zcta_2 = as.character(zcta_2))

## Load WA prison characteristics
df_char_prisons <- read_csv('../../data/maps/wa_prisons_characteristics.csv') %>% 
  filter(population_gender == 'female')

# Dataframe matching zip and zcta
df_match_county_zcta <- read_csv('../../data/maps/relationship_zcta_county_WA.csv')

vec_zip_mason <- df_match_county_zcta %>% filter(county == 'Mason County') %>% select(zcta) %>% unlist() %>% as.numeric() %>% sort()
vec_zip_pierce <- df_match_county_zcta %>% filter(county == 'Pierce County') %>% select(zcta) %>% unlist() %>% as.numeric() %>% sort()

## Define color for the axis labels
col_prison <- 'red'
col_non_prison <- 'navy'
col_adj <- 'black'

## Heatmaps with RR of observing identical sequences between Mason and Pierce postal codes
plt_female <- df_RR_zcta_mason_pierce %>% 
  ggplot(aes(x = zcta_2, y = zcta_1)) +
  geom_tile(aes(fill = log(RR))) +
  geom_tile(data = df_adj_zcta %>% 
              filter(is_adjacent == 1, zcta_1 %in% vec_zip_mason,  zcta_2 %in% vec_zip_pierce, is_adjacent == 1),
            colour = col_adj, fill = NA, size = 1) +
  geom_tile(data = df_RR_zcta_mason_pierce %>% 
              filter(zcta_1 %in% df_char_prisons$postal_code_location, 
                     zcta_2 %in% df_char_prisons$postal_code_location),
            colour = col_prison, fill = NA, size = 1) +
  scale_x_discrete(name = 'Pierce postal codes') +
  scale_y_discrete(name = 'Mason postal codes') +
  scale_fill_gradientn(name = expression(RR['identical sequences']),
                       breaks = log(c(0.1, 1., 10.)),
                       limits = c(-3.7, 3.7),
                       labels = c(
                         expression(10^{-1}), 
                         expression(10^{0}), 
                         expression(10^{1})
                       ),
                       colors = brewer.pal(11, 'BrBG'),
                       na.value = 'darkgrey') +
  coord_fixed() +
  theme_classic() +
  theme(legend.title = element_text(size = 13),
        axis.text.x = element_text(angle = 45, hjust = 1., size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(size = 12),
        legend.position = 'bottom')

## Plot with RR of observing identical sequences between Pierce and Mason counties postal codes
plot_RR_ordered <- df_RR_zcta_mason_pierce %>% 
  left_join(df_adj_zcta) %>% 
  filter(RR > 0.) %>% 
  arrange(RR) %>% 
  mutate(rank = 1:n()) %>% 
  ggplot(aes(x = as.factor(rank), y = RR,
             colour = (is_prison_1 + is_prison_2 == 2),
             shape = as.factor(is_adjacent))) +
  geom_point() +
  geom_linerange(aes(ymin = lower_RR, ymax = upper_RR)) +
  scale_x_discrete(name = 'Pairs of Pierce and Mason counties postal codes',
                   expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(trans = 'log', name = expression(RR['identical sequences']),
                     breaks = c(0.1, 1., 10., 100.), 
                     labels = c(expression(10^{-1}), expression(10^{0}), 
                                expression(10^{1}), expression(10^{2}))) +
  scale_colour_manual(values = c(col_non_prison, col_prison),
                      name = '', 
                      breaks = c(F, T),
                      labels = c('Non prison postal codes', 'Prison postal codes')) +
  scale_shape_manual(breaks = c(0, 1), 
                     values = c(16, 17),
                     labels = c('Non adjacent', 'Adjacent'), name = '') +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = 'bottom')

## Panel with the two plots
panel_female_zip <- ggarrange(plt_female, plot_RR_ordered, nrow = 2, labels = 'AUTO')

plot(panel_female_zip)

# pdf('../plots/figure_prison/RR_zcta_female_prisons.pdf',
#     height = 7, width = 10)
# plot(panel_female_zip)
# dev.off()
# png('../plots/figure_prison/RR_zcta_female_prisons.png',
#     height = 7, width = 10, res = 350, units = 'in')
# plot(panel_female_zip)
# dev.off()
# 
