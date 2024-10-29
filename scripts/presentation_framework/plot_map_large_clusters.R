### This script enables to reproduce the toy figure 1D
### depicting the timing of sequence collection within
### 2 large clusters of identical sequences on a map.

library(tidyverse)
library(sf)
library(viridis)
library(ggpubr)

## Load dataframe with timing of sequences within 2 large clusters
df_timing_sequences <- read_csv('../../results/cluster_identical_sequences/timing_large_clusters.csv') %>% 
  mutate(zcta = as.character(zcta))

## Load WA shapefile
shape_file_county <- read_sf('../../data/maps/shapefile_counties/county10.shp')
centroids_zctas <- readRDS('../../data/maps/centroids_zcta_WA.rds')

## Function to create map with the spatial imprint of a cluster of identical sequences
plot_cluster_over_time_by_zcta_static_with_seattle <- function(df_timing_sequences,
                                                               cluster_name_plot = 'Cluster A',
                                                               length_time_window = 7,
                                                               size_upper_limit = 8,
                                                               time_upper_limit = 87){
  
  ## Seattle coordinates to be added on the map
  x_seattle <- -122.32997294603338
  y_seattle <- 47.60459065371228
  
  point_seattle <- st_as_sf(tibble(longitude = x_seattle, latitude = y_seattle), 
                            coords = c("longitude","latitude"))
  st_crs(point_seattle) <- 4269
  
  
  ## Subset the dataframe to only sequences within the cluster of interest
  df_cluster <- df_timing_sequences %>% filter(cluster_name == cluster_name_plot)
  cluster_duration <- df_cluster$days_from_first_seq %>% max()
  cluster_size <- nrow(df_cluster)
  vec_days <- seq(0, cluster_duration, length_time_window)
  
  ## Aggregates by length_time_window 
  ## (if equal to 7, this will aggregate weekly from the time of first sequence collection)
  df_for_plot <- Reduce('bind_rows', lapply(vec_days, FUN = function(curr_days){
    df_cluster %>% 
      filter(days_from_first_seq >= curr_days, days_from_first_seq < curr_days + length_time_window) %>% 
      group_by(zcta) %>% 
      summarise(n_seq = n()) %>% 
      mutate(days = curr_days)
  })) 
  
  labels_time_windows <- sapply(vec_days, FUN = function(curr_day){
    paste0('[', as.character(curr_day), 
           '-',
           as.character(curr_day + length_time_window),
           ' days)')
  })
  
  ## Add plot information to centroids
  df_for_map <- centroids_zctas %>% 
    left_join(df_for_plot, by = c('ZCTA5CE10' = 'zcta')) %>% 
    arrange(-n_seq)
  
  
  ## Make plot
  n_seq_max <- max(df_for_map$n_seq, na.rm = T)
  plt <- df_for_map %>% 
    ggplot() +
    geom_sf(data = shape_file_county,
            fill = 'gray90', colour = 'white',
            size = 0.5) +
    geom_sf(data = point_seattle, size = 4, colour = 'black', shape = 18) +
    geom_sf(data = df_for_map, 
            aes(size = n_seq, colour = days),
            alpha = 0.2) +
    scale_size_continuous(name = 'Number of\nsequences', 
                          breaks = c(1, 2, 5, 10), 
                          range = c(1, max(11, 5)),
                          limits = c(1, max(11, 5))) +
    theme_void() +
    ggtitle('') +
    scale_colour_viridis(name = 'Timing of sequence collection\n(in days)',
                         limits = c(0., time_upper_limit)) +
    guides(size = guide_legend(override.aes = list(alpha = 1.))) +
    theme(legend.position = 'bottom')
  
  return(plt)
}

## Make figure
plt_1 <- plot_cluster_over_time_by_zcta_static_with_seattle(df_timing_sequences, 'Cluster A', 7) + 
  ggtitle('Cluster A')
plt_2 <- plot_cluster_over_time_by_zcta_static_with_seattle(df_timing_sequences, 'Cluster B', 7) + 
  ggtitle('Cluster B')

panel_plot <- ggarrange(plt_1, plt_2, nrow = 1, ncol = 2,
                        common.legend = T, legend = 'bottom')
plot(panel_plot)

# pdf('../../plots/maps_large_cluster_with_seattle.pdf',
#     height = 2.2, width = 4.5)
# plot(panel_plot)
# dev.off()