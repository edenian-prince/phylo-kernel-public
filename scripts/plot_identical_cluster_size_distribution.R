library(tidyverse)

## Load dataframe with cluster size distribution
df_cluster_size <- read_csv('../results/cluster_identical_sequences/cluster_identical_sequences_wa_sentinel.csv')


df_cluster_size %>% filter(cluster_size > 1) %>% select(n_clusters) %>% sum() # Number of clusters of size > 1
df_cluster_size %>% filter(cluster_size > 1) %>% 
  mutate(n_seq_in_cluster = cluster_size * n_clusters) %>% 
  select(n_seq_in_cluster) %>% sum() # Number of sequences in clusters of size > 1

## Display the cluster size distribution
cluster_size_threshold <- 20

plt_cluster_size_distrib <- df_cluster_size %>% 
  mutate(cluster_size = ifelse(cluster_size > cluster_size_threshold, cluster_size_threshold, cluster_size)) %>% 
  group_by(cluster_size) %>%
  summarise(n_clusters = sum(n_clusters)) %>% 
  ggplot(aes(x = as.factor(cluster_size), y = n_clusters)) +
  geom_bar(stat = 'identity', fill = col_plot) +
  scale_x_discrete(breaks = c(1, 5, 10, 15, cluster_size_threshold),
                   labels = c(1, 5, 10, 15, paste0(cluster_size_threshold, '+'))) +
  scale_y_cut(breaks = c(4000), scales = c(0.5, 1.), which = c(1, 2),
              expand = expansion(add = c(100., 0.), mult = c(0., 0.15))) +
  theme_classic() +
  xlab('Cluster size') + ylab('Number of clusters') +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))


plt_cluster_size_distrib
