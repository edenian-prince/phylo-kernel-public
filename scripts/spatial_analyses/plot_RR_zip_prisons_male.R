## This script reproduces Figure 3D and 3E.

library(tidyverse)
library(RColorBrewer)

# Dataframe matching zip and zcta
df_match_county_zcta <- read_csv('../../data/maps/relationship_zcta_county_WA.csv')

vec_zip_mason <- df_match_county_zcta %>% filter(county == 'Mason County') %>% select(zcta) %>% unlist() %>% as.numeric()
vec_zip_franklin <- df_match_county_zcta %>% filter(county == 'Franklin County') %>% select(zcta) %>% unlist() %>% as.numeric()
vec_zip_walla_walla <- df_match_county_zcta %>% filter(county == 'Walla Walla County') %>% select(zcta) %>% unlist() %>% as.numeric()


## Load WA prison characteristics
df_char_prisons <- read_csv('../../data/maps/wa_prisons_characteristics.csv') %>% 
  filter(population_gender == 'male')

# Load relative risk of observing identical sequences between zcta
df_mason_franklin <- read_csv('../../results/RR_zcta_prison/df_RR_zctas_mason_franklin.csv')
df_mason_ww <- read_csv('../../results/RR_zcta_prison/df_RR_zctas_mason_walla_walla.csv')


## Define color for the axis labels
col_prison <- 'red'
col_non_prison <- 'navy'

vec_zip_mason <- vec_zip_mason[vec_zip_mason %in% df_RR_zcta$zcta_1] %>% sort()
vec_zip_franklin <- vec_zip_franklin[vec_zip_franklin %in% df_RR_zcta$zcta_1] %>% sort()
vec_zip_walla_walla <- vec_zip_walla_walla[vec_zip_walla_walla %in% df_RR_zcta$zcta_1] %>% sort()

col_vec_zip_mason <- ifelse(vec_zip_mason %in% df_char_prisons$postal_code_location, col_prison, col_non_prison)
col_vec_zip_franklin <- ifelse(vec_zip_franklin %in% df_char_prisons$postal_code_location, col_prison, col_non_prison)
col_vec_zip_walla_walla <- ifelse(vec_zip_walla_walla %in% df_char_prisons$postal_code_location, col_prison, col_non_prison)


## Make heatmaps of RR between Mason and Franklin ZCTAs
plt_franklin <- df_mason_franklin %>% 
  ggplot(aes(y = as.character(zcta_1), x = as.character(zcta_2))) +
  geom_tile(aes(fill = log(RR))) +
  geom_tile(data = df_mason_franklin %>% 
              filter(zcta_1 %in% df_char_prisons$postal_code_location, 
                     zcta_2 %in% df_char_prisons$postal_code_location),
            colour = col_prison, fill = NA, size = 1) +
  scale_x_discrete(name = 'Franklin postal codes') +
  scale_y_discrete(name = 'Mason postal codes') +
  scale_fill_gradientn(name = expression(RR['identical sequences']),
                       breaks = log(c(0.1, 1., 10.)),
                       limits = c(-3.7, 3.7),
                       labels = c(
                         expression('1\U00B7'~ 10^{-1}), 
                         expression('1\U00B7'~ 10^{0}), 
                         expression('1\U00B7'~ 10^{1})
                       ),
                       colors = brewer.pal(11, 'BrBG'),
                       na.value = 'darkgrey') +
  coord_fixed() +
  theme_classic() +
  theme(legend.title = element_text(size = 13),
        axis.text.x = element_text(angle = 45, hjust = 1.,
                                   size = 12, colour = col_vec_zip_franklin),
        axis.text.y = element_text(size = 12, colour = col_vec_zip_mason),
        legend.text = element_text(size = 12))

## Make heatmaps of RR between Mason and Walla Walla ZCTAs
plt_ww <- df_mason_ww %>%
  ggplot(aes(y = as.character(zcta_1), x = as.character(zcta_2))) +
  geom_tile(aes(fill = log(RR))) +
  geom_tile(data = df_mason_ww %>% 
              filter(zcta_1 %in% df_char_prisons$postal_code_location, 
                     zcta_2 %in% df_char_prisons$postal_code_location),
            colour = col_prison, fill = NA, size = 1) +
  scale_x_discrete(name = 'Walla Walla postal codes') +
  scale_y_discrete(name = 'Mason postal codes') +
  scale_fill_gradientn(name = expression(RR['identical sequences']),
                       breaks = log(c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10., 20.)),
                       labels = c(expression('1\U00B7'~ 10^{-1}), 
                                  expression('2\U00B7'~ 10^{-1}), 
                                  expression('5\U00B7'~ 10^{-1}), 
                                  expression('1\U00B7'~ 10^{0}), 
                                  expression('2\U00B7'~ 10^{0}), 
                                  expression('5\U00B7'~ 10^{0}), 
                                  expression('1\U00B7'~ 10^{1}), 
                                  expression('2\U00B7'~ 10^{1})
                       ),
                       limits = c(-2.1, 2.1),
                       colors = brewer.pal(11, 'BrBG'),
                       na.value = 'darkgrey') +
  coord_fixed() +
  theme_classic() +
  theme(legend.title = element_text(size = 13),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1.,
                                   size = 12, colour = col_vec_zip_walla_walla),
        axis.text.y = element_text(size = 12, colour = col_vec_zip_mason),
        legend.text = element_text(size = 12))

plot(plt_franklin)
plot(plt_ww)

# png('../plots/figure_prison/RR_zip_mason_franklin.png',
#     height = 3., width = 5., res = 350, units = 'in')
# plot(plt_franklin)
# dev.off()
# png('../plots/figure_prison/RR_zip_mason_wallawalla.png',
#     height = 3., width = 5., res = 350, units = 'in')
# plot(plt_ww)
# dev.off()

# pdf('../plots/figure_prison/RR_zip_mason_franklin.pdf',
#     height = 3., width = 5.)
# plot(plt_franklin)
# dev.off()
# pdf('../plots/figure_prison/RR_zip_mason_wallawalla.pdf',
#     height = 3., width = 5.)
# plot(plt_ww)
# dev.off()