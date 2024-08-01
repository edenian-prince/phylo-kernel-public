## This script reproduces chloropleth maps depicting the relative risk
## of observing identical sequences between counties.

library(tidyr)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)

## Load WA counties shapefile
shape_file_county <- read_sf('../data/maps/shapefile_counties/county10.shp')

## Load relative risk of observing identical sequences between two counties
df_RR_counties <- readRDS('../results/RR_county/df_RR_county_0_mut_away.rds')

## Breaks used for the legend in the different counties (manually defined)
list_labels_legend_all_counties <- list(
  'Adams County' = c(0.1, 0.5, 1.0, 5.0, 10.0),
  'Asotin County' = c(0.1, 1.0, 10.0),
  'Benton County' = c(0.1, 0.2, 0.3, 0.5, 1.0, 2.0, 3.0, 5.0, 10.0),
  'Chelan County' = c(0.5, 0.8, 1.0, 1.5, 2.0),
  'Clallam County' = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0),
  'Clark County' = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0),
  'Columbia County' = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0),
  'Cowlitz County' = c(0.1, 1.0, 10.0),
  'Douglas County' = c(0.5, 0.8, 1.0, 1.5, 2.0),
  'Ferry County' = c(0.1, 1.0, 10.0, 100.0),
  'Franklin County' = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0),
  'Garfield County' = c(0.1, 1.0, 10.0, 100.0),
  'Grant County' = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0),
  'Grays Harbor County' = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0),
  'Island County' = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0),
  'Jefferson County' = c(0.1, 1.0, 10.0),
  'King County' = c(0.5, 0.8, 1.0, 1.5, 2.0),
  'Kitsap County' = c(0.5, 0.8, 1.0, 1.5, 2.0),
  'Kittitas County' = c(0.1, 1.0, 10.0),
  'Klickitat County' = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0),
  'Lewis County' = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0),
  'Lincoln County' = c(0.1, 1.0, 10.0, 100.0),
  'Mason County' = c(0.1, 1.0, 10.0),
  'Okanogan County' = c(0.3, 0.5, 1.0, 2.0, 3.0),
  'Pacific County' = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0),
  'Pend Oreille County' = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0),
  'Pierce County' = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0),
  'San Juan County' = c(0.1, 1.0, 10.0),
  'Skagit County' = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0),
  'Skamania County' = c(0.1, 1.0, 10.0, 100.0),
  'Snohomish County' = c(0.5, 0.8, 1.0, 1.5, 2.0),
  'Spokane County' = c(0.3, 0.5, 1.0, 2.0, 3.0),
  'Stevens County' = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0),
  'Thurston County' = c(0.3, 0.5, 1.0, 2.0, 3.0),
  'Wahkiakum County' = c(0.1, 1.0, 10.0, 100.0),
  'Walla Walla County' = c(0.01, 0.1, 1.0, 10.0, 50.),
  'Whatcom County' = c(0.5, 0.7, 1.0, 1.3, 2.0),
  'Whitman County' = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0),
  'Yakima County' = c( 0.3, 0.5, 1.0, 2.0, 3.0)
)

## Generate the maps (one for each county)
vec_names_counties <- df_RR_counties$group_1 %>% unique() %>% sort()

list_map_all_counties <- lapply(vec_names_counties, FUN = function(curr_county){
  # Get dataset for county-specific map
  df_pairs_for_map <- df_RR_counties %>% 
    filter(group_1 == curr_county) %>% 
    select(group_2, RR) %>% ungroup()
  
  # Add RR to shapefile
  curr_shape_file_county <- shape_file_county %>% 
    left_join(df_pairs_for_map, by = c('NAMELSAD10' = 'group_2'))
  
  # Define scale
  max_log10_RR <- log10(max(df_pairs_for_map$RR[df_pairs_for_map$RR > 0.]))
  min_log10_RR <- log10(min(df_pairs_for_map$RR[df_pairs_for_map$RR > 0.]))
  curr_range <- max(abs(max_log10_RR), abs(min_log10_RR))
  
  # Get the centroid of the focal county (added to the plot)
  centroid_to_add <- st_centroid(curr_shape_file_county %>% filter(NAMELSAD10 == curr_county))
  
  # Make map
  curr_plot <- curr_shape_file_county %>% 
    ggplot() + 
    geom_sf(aes(fill = log10(RR)), colour = 'white') +
    geom_sf(data = centroid_to_add, size = 2, colour = 'firebrick') +
    scale_fill_gradientn(colours = brewer.pal(11, 'BrBG'),
                         limits = c(-curr_range, curr_range),
                         na.value = 'gray90',
                         name = expression(RR["id. seq."]),
                         breaks = log10(list_labels_legend_all_counties[[curr_county]]),
                         labels = list_labels_legend_all_counties[[curr_county]]
    ) +
    ggtitle(strsplit(curr_county, ' County')[[1]][1]) +
    theme_void() +
    theme(legend.key.height = unit(0.45, 'cm'),
          legend.position = 'right',
          plot.margin = margin(0.1, 0.1, 0.5, 0.1, "cm"),
          plot.title = element_text(size = 11))
})

# pdf('../plots/figure_space/maps_RR_all_counties_1.pdf',
#     height = 11, width = 7.5)
plot(ggarrange(plotlist = list_map_all_counties[1:21], nrow = 7, ncol = 3))
#dev.off()
# pdf('../plots/figure_space/maps_RR_all_counties_2.pdf',
#     height = 11*6/7, width = 7.5)
plot(ggarrange(plotlist = list_map_all_counties[22:39], nrow = 6, ncol = 3))
#dev.off()

## Save the map for a subset of counties
id_map_six_counties <- which(names(list_labels_legend_all_counties) %in% c('King County', 'Clallam County', 'Pacific County', 'Benton County', 'Spokane County', 'Stevens County'))
# png('../plots/figure_space/maps_6_counties.png',
#     height = 3.8, width = 8, res = 350, units = 'in')
plot(ggarrange(plotlist = list_map_all_counties[id_map_six_counties], 
               nrow = 2, ncol = 3))
#dev.off()
# pdf('../plots/figure_space/maps_6_counties.pdf',
#     height = 3.8, width = 8)
# plot(ggarrange(plotlist = list_map_all_counties[id_map_six_counties], 
#                nrow = 2, ncol = 3))
# dev.off()

## Save the map just for Stevens county
id_Stevens <- which(names(list_labels_legend_all_counties) == 'Stevens County')
# pdf('../plots/figure_space/maps_stevens.pdf',
#     height = 2., width = 3.)
plot(list_map_all_counties[id_Stevens][[1]])
#dev.off()
