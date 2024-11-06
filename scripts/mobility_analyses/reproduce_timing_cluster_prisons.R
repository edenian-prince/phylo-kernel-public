## This script illustrate how Figrue 3G can be reproduce.
## We don't share the data undelrying the Figure but provide the backbone to reproduce the figure
## from user defined data.

library(tidyverse)
library(RColorBrewer)


## Read synthetic data that have the same structure as the ones that were used to generate
## Figure 3G
df_for_plot_arbitrary_data <- read_csv('../../results/cluster_identical_sequences/synthetic_timing_large_clusters_prisons.csv')
# week: Week of sequence collection
# location_id: ID of the location (plotted on the y-axis)
# cluster_name: ID of the cluster
# n_seq: number of sequences collected for the name, that cluster and in that location

synthetic_plot <- df_for_plot_arbitrary_data %>% 
  ggplot(aes(colour = as.character(cluster_name))) +
  geom_jitter(aes(x = week, y = location_id,
                  size = n_seq, fill = as.character(cluster_name)),
              alpha = 0.3, width = 0., height = 0., shape = 21
  ) +
  scale_x_continuous(name = 'Sequence collection week') +
  scale_y_continuous(name = 'Location ID', 
                     expand = expansion(mult = c(0.1, 0.1)),
                     breaks = seq(0, 10, 1)) +
  scale_fill_manual(values = c(rev(brewer.pal(12, 'Paired')[c(1, 3, 5, 7, 9, 11) + 1]), 
                               'cyan3', 'goldenrod2'),
                    name = 'Cluster ID') +
  scale_colour_manual(values = c(rev(brewer.pal(12, 'Paired')[c(1, 3, 5, 7, 9, 11) + 1]), 
                                 'cyan3', 'goldenrod2'),
                      name = 'Cluster ID') +
  scale_size_continuous(name = 'Number of sequences',
                        breaks = c(1, 2, 5, 10, 20), 
                        range = c(3, 10)) +
  theme_bw() +
  theme(legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 11)) +
  guides(
    colour = guide_legend(position = "bottom"),
    size   = guide_legend(position = "right")
  )

plot(synthetic_plot)
