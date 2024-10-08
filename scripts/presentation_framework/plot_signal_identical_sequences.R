## This script reproduces Figure 1E, more precisely the cluster radius, 
## the proportion of clusters that stayed in the same ZCTA and the same
## county as a function of time since first cluster detection

library(tidyverse)

## Load dataframe with cluster radius and probability to stay within the same cluster as a function of time
df_dist_time <- read_csv('../../results/cluster_identical_sequences/signal_function_distance.csv')

## Parameters used to display the results
shift_intercept <- 0. # Parameter used to define the secondary axis
shift_slope <- 75 # Parameter used to define the secondary axis
col_prop <- 'black'
col_distance <- 'firebrick'

plt_signal_func_time <- df_dist_time %>%  
  ggplot(aes(x = days)) +
  # Plot maximum distance
  geom_line(aes(y = central_mean_max_dist), color = col_distance) +
  geom_ribbon(aes(ymin = lower_mean_max_dist, ymax = upper_mean_max_dist),
              alpha = 0.3, fill = col_distance) +
  # Plot proba to be outside zip
  geom_line(aes(y = (1. - central_prop_outside_zip  + shift_intercept)* shift_slope,
                linetype = 'Zip code'), color = col_prop) +
  geom_ribbon(aes(ymin = (1. - upper_prop_outside_zip  + shift_intercept)* shift_slope,
                  ymax = (1. - lower_prop_outside_zip  + shift_intercept)* shift_slope),
              alpha = 0.3, fill = col_prop) +
  # Plot proba to be outside county
  geom_line(aes(y = (1. - central_prop_outside_county  + shift_intercept)* shift_slope,
                linetype = 'County'), color = col_prop) +
  geom_ribbon(aes(ymin = (1. - upper_prop_outside_county  + shift_intercept)* shift_slope,
                  ymax = (1. - lower_prop_outside_county  + shift_intercept)* shift_slope), 
              alpha = 0.3, fill = col_prop) +
  theme_classic() +
  scale_x_continuous(name = 'Time (in days)', expand = expansion(mult = c(0.0, 0.02))) +
  scale_y_continuous(name = 'Cluster radius (in km)', expand = expansion(mult = c(0.0, 0.01)),
                     breaks = seq(0, 80, 10),
                     sec.axis = sec_axis(~ . /shift_slope - shift_intercept,
                                         name = 'Probability of cluster\nremaining in spatial unit\n',
                                         breaks = seq(0., 1., 0.2))) +
  scale_linetype_manual(name = '', values = c(1, 2)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.y.left = element_text(color = col_distance),
        axis.title.y.left = element_text(color = col_distance),
        axis.ticks.y.left = element_line(color = col_distance),
        axis.line.y.left = element_line(color = col_distance),
        axis.text.y.right = element_text(color = col_prop),
        axis.title.y.right = element_text(color = col_prop),
        axis.ticks.y.right = element_line(color = col_prop),
        axis.line.y.right = element_line(color = col_prop),
        legend.position = c(0.5, 0.1), legend.direction = 'horizontal',
        legend.background = element_blank(),
        legend.text = element_text(size = 12))


plot(plt_signal_func_time)

# pdf('../../figures/figure_signal_identical_sequences/plt_signal_clusters_distance_zip_county.pdf',
#     height = 3.5, width = 4.5)
# plot(plt_signal_func_time)
# dev.off()