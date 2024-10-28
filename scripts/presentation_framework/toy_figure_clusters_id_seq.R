### This script enables to reproduce the toy figure 1A
### illustrating that the clustering of identical sequences 
### is informative about transmission patterns at the 
### population level.

library(tidyverse)
library(sf)
library(RColorBrewer)
library(scatterpie)
library(ggpubr)

# Load WA shapefiles
shape_file_county <- read_sf('../../data/maps/shapefile_counties/county10.shp')

# Load correspondance between ZCTA and counties
df_corr_zcta_counties <- read_csv('../../data/maps/relationship_zcta_county_WA.csv')
df_corr_counties_regions <- read_csv('../../data/maps/county_wa.csv') %>% 
  select(county, region)
df_corr_zcta_regions <- df_corr_zcta_counties %>% left_join(df_corr_counties_regions)

# Create region shapefile
shape_file_regions <- shape_file_county %>% 
  left_join(df_corr_counties_regions, by = c('NAMELSAD10' = 'county')) %>% 
  group_by(region) %>% 
  summarize(geometry = st_union(geometry))
regions_centroids <- st_centroid(shape_file_regions)
  
# Get distance between WA ZCTAs
gamma_param <- 1.7
centroids_zctas <- readRDS('../../data/maps/centroids_zcta_WA.rds')
dist_mat_zctas <- st_distance(centroids_zctas) / 1e3
rownames(dist_mat_zctas) <- colnames(dist_mat_zctas) <- centroids_zctas$ZCTA5CE10
dist_mat_zctas <- dist_mat_zctas %>% 
  as_tibble() %>% 
  mutate(zcta_1 = centroids_zctas$ZCTA5CE10) %>% 
  pivot_longer(cols = - 'zcta_1', names_to = 'zcta_2', values_to = 'distance_km') %>% 
  mutate(distance_km = as.numeric(distance_km)) %>% 
  mutate(connectivity = case_when(zcta_1 == zcta_2 ~ 1.,
                                  distance_km < 25. ~ 0.5,
                                  distance_km < 50. ~ 0.1,
                                  T ~ 0.)) %>% 
  group_by(zcta_1) %>% 
  mutate(proba = connectivity / sum(connectivity)) %>% 
  ungroup() %>% 
  arrange(zcta_2)

# Define the start points of the different clusters
n_clusters <- 11
cluster_size <- 45
zcta_start_clusters <- c('98220', '98903', '98855', '99326', '98675', '99029', '98283', '98363',
                         '99139', '98283', '98903')

simulate_toy_cluster <- function(zcta_start_cluster, dist_mat_zctas, cluster_size){
  
  vec_zcta_cluster <- rep(NA, cluster_size)
  vec_zcta_cluster[1] <- zcta_start_cluster
  
  for(i_infect in 1:(cluster_size - 1)){
    # Draw county of new infectee
    curr_zcta_infector <- vec_zcta_cluster[i_infect]
    
    curr_df_connect <- dist_mat_zctas %>% 
      filter(zcta_1 == curr_zcta_infector) %>% 
      select(zcta_2, proba)
    
    curr_id_infectee <- which(rmultinom(n = 1, size = 1, prob = curr_df_connect$proba) == 1)
    curr_zcta_infectee <- as.character(curr_df_connect$zcta_2[curr_id_infectee])
    vec_zcta_cluster[i_infect + 1] <- curr_zcta_infectee
  }
  
  return(vec_zcta_cluster)
}


set.seed(12)
df_clusters <- Reduce('bind_rows', lapply(1:n_clusters, FUN = function(i_cluster){
  tibble(zcta_cluster = simulate_toy_cluster(zcta_start_clusters[i_cluster], dist_mat_zctas, cluster_size),
         cluster_id = rep(i_cluster, cluster_size)) 
}))%>% 
  group_by(cluster_id, zcta_cluster) %>% 
  summarise(n_seq = n()) %>% 
  ungroup()

## Plot results
centroids_to_add <- Reduce('bind_rows', lapply(1:n_clusters, FUN = function(curr_cluster_id){
  curr_df_clusters <- df_clusters %>% filter(cluster_id == curr_cluster_id)
  centroids_zctas %>% 
    filter(GEOID10 %in% curr_df_clusters$zcta_cluster) %>% 
    left_join(curr_df_clusters, by = c('ZCTA5CE10' = 'zcta_cluster')) 
}))

plt_clusters_by_zcta <- centroids_to_add %>% 
  ggplot() +
  geom_sf(data = shape_file_regions, fill = 'gray90', colour = 'white', size = 0.5) +
  geom_sf(aes(size = n_seq, colour = as.factor(cluster_id)), alpha = 0.7) +
  theme_void() +
  scale_colour_manual(breaks = 1:n_clusters, 
                      #values = c('deeppink3', 'darkslateblue', 'darkcyan', 'orange3', 'black'),
                      values = brewer.pal(12, 'Paired')[c(seq(2, 12, 2), 1, 3, 5, 7, 9)]) +
  theme(legend.position = 'none')


centroids_to_add_regions <- df_clusters %>% 
  left_join(df_corr_zcta_regions %>% mutate(zcta = as.character(zcta)),
            by = c('zcta_cluster' = 'zcta')) %>% 
  group_by(cluster_id, region) %>% 
  summarise(n_seq = sum(n_seq)) %>% 
  left_join(regions_centroids %>% 
              bind_cols(st_coordinates(regions_centroids))) 

plt_clusters_by_region <- centroids_to_add_regions %>% 
  mutate(cluster_id = as.factor(cluster_id)) %>% 
  rename(value = n_seq) %>% 
  ggplot() +
  geom_sf(data = shape_file_regions, fill = 'gray90', colour = 'white', size = 0.5) +
  geom_scatterpie(aes(x = X, y = Y), cols = 'cluster_id', long_format = TRUE, 
                  pie_scale = 4, colour = NA) +
  theme_void() +
  scale_fill_manual(breaks = 1:n_clusters, 
                    values = brewer.pal(12, 'Paired')[c(seq(2, 12, 2), 1, 3, 5, 7, 9)])+
  theme(legend.position = 'none')



panel_explanation_method <- ggarrange(plt_clusters_by_zcta, plt_clusters_by_region)
pdf('../../figures/toy_figure_cluster_id_seq.pdf', height = 3., width = 7)
plot(panel_explanation_method)
dev.off()



df_clusters_by_region <- df_clusters %>% 
  left_join(df_corr_zcta_regions %>% mutate(zcta = as.character(zcta)),
            by = c('zcta_cluster' = 'zcta')) %>% 
  group_by(cluster_id, region) %>% 
  summarise(n_seq = sum(n_seq))

df_arrows_shared_clusters <- Reduce('bind_rows', lapply(unique(df_clusters_by_region$cluster_id), FUN = function(curr_id){
  vec_regions <- df_clusters_by_region %>% 
    filter(cluster_id == curr_id) %>% ungroup() %>% 
    select(region) %>% unlist() %>% as.character()
  expand.grid(region_1 = as.character(vec_regions), 
              region_2 = as.character(vec_regions)) %>% 
    as_tibble() %>%
    filter(as.character(region_1) > as.character(region_2)) %>% 
    mutate(cluster_id = curr_id)
})) %>% 
  group_by(region_1, region_2) %>% 
  summarise(n_clusters = n()) %>% 
  ungroup() %>% 
  left_join(regions_centroids %>% bind_cols(st_coordinates(regions_centroids)),
            by = c('region_1' = 'region')) %>% 
  rename(X1 = X, Y1 = Y) %>% 
  left_join(regions_centroids %>% bind_cols(st_coordinates(regions_centroids)),
            by = c('region_2' = 'region')) %>% 
  rename(X2 = X, Y2 = Y)


plt_arrows <- centroids_to_add_regions %>% 
  mutate(cluster_id = as.factor(cluster_id)) %>% 
  rename(value = n_seq) %>% 
  ggplot() +
  geom_sf(data = shape_file_regions, fill = 'gray90', colour = 'white', size = 0.5) +
  geom_scatterpie(aes(x = X, y = Y), cols = 'cluster_id', long_format = TRUE, 
                  pie_scale = 4, colour = NA, alpha = 0.4) +
  geom_segment(data = df_arrows_shared_clusters,
               aes(x = X1, y = Y1, xend = X2, yend = Y2, 
                   linewidth = n_clusters,
                   alpha = n_clusters),
               lineend = "round", color = 'black') +
  theme_void() +
  scale_fill_manual(breaks = 1:n_clusters, 
                    values = brewer.pal(12, 'Paired')[c(seq(2, 12, 2), 1, 3, 5, 7, 9)])+
  scale_linewidth_continuous(range = c(.4, 2.)) +
  scale_alpha_continuous(range = c(0.5, 1.)) +
  theme(legend.position = 'none')

panel_explanation_method_v2 <- ggarrange(plt_clusters_by_zcta, plt_clusters_by_region, plt_arrows, 
                                      nrow = 1, ncol = 3)
panel_explanation_method_v2

pdf('../../figures/toy_figure_cluster_id_seq_v2.pdf', height = 3., width = 10)
plot(panel_explanation_method_v2)
dev.off()
