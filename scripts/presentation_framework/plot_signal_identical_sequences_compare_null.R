## This script reproduces Figure S1, more precisely the cluster radius, 
## the proportion of clusters that stayed in the same ZCTA and the same
## county as a function of time since first cluster detection and how
## that compares with expectations from a null distribution 

library(tidyverse)

## Load dataframe with cluster radius and probability to stay within the same cluster as a function of time
## from WA data and under the null distribution obtained through 100 permutations
df_dist_time <- read_csv('../../results/cluster_identical_sequences/signal_function_distance.csv') ## WA data
df_dist_time_null <- read_csv('../../results/cluster_identical_sequences/signal_function_distance_null.csv') ## Null distribution

# Define colors for plot
col_null <- 'darkgrey'
col_empirical <- 'darkcyan'

# Cluster radius as a function of time
plt_comp_null_mean_dist <- df_dist_time_null %>% 
  ggplot(aes(x = days)) +
  geom_line(aes(y = central_mean_max_dist_null, colour = 'Null')) +
  geom_ribbon(aes(ymin = lower_mean_max_dist_null, ymax = upper_mean_max_dist_null),
              fill = col_null, alpha = 0.3, col = NA) +
  geom_line(data = df_dist_time, 
            aes(y = central_mean_max_dist, colour = 'Empirical')) +
  geom_ribbon(data = df_dist_time, aes(ymin = lower_mean_max_dist, ymax = upper_mean_max_dist),
              fill = col_empirical, alpha = 0.3) +
  theme_classic() +
  scale_x_continuous(name = 'Time (in days)', expand = expansion(mult = c(0.0, 0.03))) +
  scale_y_continuous(name = 'Cluster radius (in km)', expand = expansion(mult = c(0.0, 0.01)),
                     breaks = seq(0, 200, 20)) +
  scale_colour_manual(name = '', breaks = c('Empirical', 'Null'), values = c(col_empirical, col_null)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = c(0.5, 0.1), legend.direction = 'horizontal',
        legend.background = element_blank(),
        legend.text = element_text(size = 12))

# Proportion of pairs within the same ZCTA as a function of time
plt_comp_null_prop_within_zcta <- df_dist_time_null %>% 
  ggplot(aes(x = days)) +
  geom_line(aes(y = 1. - central_prop_outside_zip_null, colour = 'Null')) +
  geom_ribbon(aes(ymin = 1. - lower_prop_outside_zip_null, ymax = 1. - upper_prop_outside_zip_null),
              fill = col_null, alpha = 0.3, col = NA) +
  geom_line(data = df_dist_time, 
            aes(y = 1. - central_prop_outside_zip, colour = 'Empirical')) +
  geom_ribbon(data = df_dist_time,
              aes(ymin = 1. - lower_prop_outside_zip, ymax = 1. - upper_prop_outside_zip),
              fill = col_empirical, alpha = 0.3) +
  theme_classic() +
  scale_x_continuous(name = 'Time (in days)', expand = expansion(mult = c(0.0, 0.03))) +
  scale_y_continuous(name = 'Probability of cluster\nremaining within ZCTA', expand = expansion(mult = c(0.0, 0.01)),
                     breaks = seq(0, 1., 0.2),
                     limits = c(0., 1.)) +
  scale_colour_manual(name = '', breaks = c('Empirical', 'Null'), values = c(col_empirical, col_null)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = c(0.5, 0.1), legend.direction = 'horizontal',
        legend.background = element_blank(),
        legend.text = element_text(size = 12))

# Proportion of pairs within the same county as a function of time
plt_comp_null_prop_within_county <- df_dist_time_null %>% 
  ggplot(aes(x = days)) +
  geom_line(aes(y = 1. - central_prop_outside_county_null, colour = 'Null')) +
  geom_ribbon(aes(ymin = 1. - lower_prop_outside_county_null, ymax = 1. - upper_prop_outside_county_null),
              fill = col_null, alpha = 0.3, col = NA) +
  geom_line(data = df_dist_time, 
            aes(y = 1. - central_prop_outside_county, colour = 'Empirical')) +
  geom_ribbon(data = df_dist_time,
              aes(ymin = 1. - lower_prop_outside_county, ymax = 1. - upper_prop_outside_county),
              fill = col_empirical, alpha = 0.3) +
  theme_classic() +
  scale_x_continuous(name = 'Time (in days)', expand = expansion(mult = c(0.0, 0.03))) +
  scale_y_continuous(name = 'Probability of cluster\nremaining within county', expand = expansion(mult = c(0.0, 0.01)),
                     breaks = seq(0, 1., 0.2),
                     limits = c(0., 1.)) +
  scale_colour_manual(name = '', breaks = c('Empirical', 'Null'), values = c(col_empirical, col_null)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = c(0.5, 0.1), legend.direction = 'horizontal',
        legend.background = element_blank(),
        legend.text = element_text(size = 12))

# Create a panel with the 3 plots
panel_comp_null <- ggarrange(plt_comp_null_mean_dist, 
                             plt_comp_null_prop_within_county, 
                             plt_comp_null_prop_within_zcta, 
                             ncol = 3, labels = 'AUTO', 
                             common.legend = T, legend = 'bottom')

# pdf('../../figures/figure_signal_identical_sequences/comparison_signal_identical_sequences_null.pdf',
#     height = 3.4, width = 9.5)
plot(panel_comp_null)
#dev.off()

