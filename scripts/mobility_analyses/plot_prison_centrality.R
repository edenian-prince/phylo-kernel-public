## This script uses RR of identical sequences between ZCTAs with
## male prison facilities and analyses the centrality of the network
## where each edge has a weight equal to the RR of identical sequences
## between the two ZCTAs.

library(tidyverse)
library(igraph)

## Load characteristics of WA prisons
df_char_prisons <- read_csv('../../data/maps/wa_prisons_characteristics.csv') %>% 
  filter(population_gender == 'male')

## Load RR of identical sequences between male prison postal codes
df_RR_prisons <- read_csv('/Volumes/homes/phylo-kernel-final/results/RR_DOH_approval/RR_zcta_prison/df_RR_zcta_prison_0_mut_away_corr.csv')

## Define edges of the graph used in the centrality analysis
df_edges <- df_RR_prisons %>% 
  filter(group_1 != 'Other', group_2 != 'Other') %>% 
  select(group_1, group_2, RR)
  
## Function to convert the dataframe with edges to a matrix
get_mat_RR_from_df_RR <- function(df_RR_complete){
  mat_adj <- df_RR_complete %>% 
    arrange(group_1, group_2) %>% 
    select(group_1, group_2, RR) %>% 
    pivot_wider(values_from = 'RR', names_from = 'group_2', values_fill = 0., names_sort = T) %>% 
    select(-group_1) %>% 
    as.matrix()
  rownames(mat_adj) <- colnames(mat_adj)
  diag(mat_adj) <- rep(0, nrow(mat_adj))
  return(mat_adj)
}

## Function to compute eigenvector centrality from an edge matrix
get_centrality_mat_adjacency <- function(mat_adjacency){
  # Create weighted graph
  weighted_graph <- graph.adjacency(adjmatrix = mat_adjacency, weighted = T)
  # Compute centrality
  df_centrality <- tibble(postal_code_location = names(eigen_centrality(graph = weighted_graph)$vector),
                          eigen_centr = eigen_centrality(graph = weighted_graph)$vector)
  
  return(df_centrality)
}

## Get eigenvector centrality for prisons
df_centrality_empirical <- get_centrality_mat_adjacency(get_mat_RR_from_df_RR(df_edges))

## Lollipop plots for centrality scores
plt_centrality <- df_centrality_empirical %>% 
  mutate(postal_code_location = factor(postal_code_location, levels = df_char_prisons$postal_code_location[order(df_char_prisons$capacity)])) %>% 
  ggplot(aes(x = postal_code_location, y = eigen_centr)) +
  geom_point(size = 2) +
  geom_segment(aes(xend = postal_code_location, y = 0, yend = eigen_centr)) +
  theme_classic() +
  scale_x_discrete(name = '') +
  scale_y_continuous(name = 'Centrality score', limits = c(0., NA),
                     expand = expansion(mult = c(0., 0.05)),
                     breaks = seq(0., 1., 0.25)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = 'top') +
  guides(colour = guide_legend(ncol = 1))

plot(plt_centrality)
