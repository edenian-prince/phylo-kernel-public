## This script reproduces the heatmap of relative risk of observing identical sequences between counties.

library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(RColorBrewer)

## Load relative risk of observing identical sequences between two counties
df_RR_counties <- read_csv('../../results/RR_county/df_RR_county_0_mut_away.csv')

## Heatmap of the matrix of RR
plt_heatmap <- df_RR_counties %>%
  mutate(county_1 = strsplit(group_1, ' County') %>% map_chr(., 1),
         county_2 = strsplit(group_2, ' County') %>% map_chr(., 1)) %>% 
  ggplot(aes(x = county_1, y = county_2, fill = log10(RR))) +
  geom_tile() +
  scale_fill_gradientn(name = expression(RR["identical sequences"]),
                       limits = c(-4, 4.),
                       breaks = seq(-4, 4, 1),
                       labels = c(expression(10^{-4}), expression(10^{-3}), expression(10^{-2}),
                                  expression(10^{-1 }), expression(10^{0}), expression(10^{1}), 
                                  expression(10^{2}), expression(10^{3}), expression(10^{4})),
                       colours = brewer.pal(11, 'BrBG'),
                       na.value = 'gray90') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90., hjust=0.95,vjust=0.2),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.key.height = unit(1.5, 'cm')) +
  coord_fixed()

#pdf('../plots/figure_space/heatmap_RR_county.pdf', height = 5, width = 7)
plot(plt_heatmap)
#dev.off()
# png('../plots/figure_space/heatmap_RR_county.png', height = 5, width = 7, res = 350, units = 'in')
# plot(plt_heatmap)
# dev.off()
