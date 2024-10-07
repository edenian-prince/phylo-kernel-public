## This script enables to reproduce the figure describing the characteristics of clusters
## of identical sequences during our study period in WA.

library(tidyverse)

## Load dataframe with number of clusters by week of first cluster detection
## Week 0 corresponds to 1 Mar 2021 - 7 Mar 2021
df_cluster_charact <- read_csv('../../results/cluster_identical_sequences/charact_clusters_by_detection_week.csv')


## Plot number of new clusters per week, mean size of clusters and 
### Characteristic of secondary axis
scaling_slope_y_axis <- 1100
scaling_intercept_y_axis <- 1100
### Characteristic of tertiary axis
scaling_slope_y_axis_2 <- 85
scaling_intercept_y_axis_2 <- 0
### Breaks for third axis
breaks_cluster_duration <- c(0, 5, 10, 15, 20, 25)
scaled_breaks_cluster_duration <- breaks_cluster_duration * scaling_slope_y_axis_2 - scaling_intercept_y_axis_2

date_start_plot <- as.Date('2021-03-01')

plt_cluster_charact_over_time <- df_cluster_charact %>% 
  ggplot(aes(x = week)) +
  geom_bar(aes(y = n_clusters), stat = 'identity', fill = 'gray') +
  geom_line(aes(y = mean_cluster_size * scaling_slope_y_axis - scaling_intercept_y_axis), 
            color = 'orange2') +
  geom_line(aes(y = mean_cluster_duration * scaling_slope_y_axis_2 - scaling_intercept_y_axis_2), 
            color = 'darkcyan') +
  annotate("segment", x = 109, y = 0, xend = 109, yend = 2300, color = 'darkcyan') +
  annotate("text", x = 115, y = 1150, color = 'darkcyan', angle = -90, hjust = 0.5, 
           label = 'Mean cluster duration (in days)') +
  annotate('segment', x = 109, xend = 109.5, 
           y = scaled_breaks_cluster_duration[1], yend = scaled_breaks_cluster_duration[1],
           color = 'darkcyan') +
  annotate('segment', x = 109, xend = 109.5, 
           y = scaled_breaks_cluster_duration[2], yend = scaled_breaks_cluster_duration[2],
           color = 'darkcyan') +
  annotate('segment', x = 109, xend = 109.5, 
           y = scaled_breaks_cluster_duration[3], yend = scaled_breaks_cluster_duration[3],
           color = 'darkcyan') +
  annotate('segment', x = 109, xend = 109.5, 
           y = scaled_breaks_cluster_duration[4], yend = scaled_breaks_cluster_duration[4],
           color = 'darkcyan') +
  annotate('segment', x = 109, xend = 109.5, 
           y = scaled_breaks_cluster_duration[5], yend = scaled_breaks_cluster_duration[5],
           color = 'darkcyan') +
  annotate('segment', x = 109, xend = 109.5, 
           y = scaled_breaks_cluster_duration[6], yend = scaled_breaks_cluster_duration[6],
           color = 'darkcyan') +
  annotate('text', x = 110, y = scaled_breaks_cluster_duration[1], 
           color = 'darkcyan', label = breaks_cluster_duration[1],
           hjust = 0) +
  annotate('text', x = 110, y = scaled_breaks_cluster_duration[2], 
           color = 'darkcyan', label = breaks_cluster_duration[2],
           hjust = 0) +
  annotate('text', x = 110, y = scaled_breaks_cluster_duration[3], 
           color = 'darkcyan', label = breaks_cluster_duration[3],
           hjust = 0) +
  annotate('text', x = 110, y = scaled_breaks_cluster_duration[4], 
           color = 'darkcyan', label = breaks_cluster_duration[4],
           hjust = 0) +
  annotate('text', x = 110, y = scaled_breaks_cluster_duration[5], 
           color = 'darkcyan', label = breaks_cluster_duration[5],
           hjust = 0) +
  annotate('text', x = 110, y = scaled_breaks_cluster_duration[6], 
           color = 'darkcyan', label = breaks_cluster_duration[6],
           hjust = 0) +
  scale_x_continuous(name = 'Week of first cluster detection',
                     breaks = c(0, 15, 30, 45, 60, 75, 90),
                     labels = format(date_start_plot + c(0, 15, 30, 45, 60, 75, 90) * 7, '%d %b\n%Y'),
                     expand = expansion(mult = c(0.03, 0.03))) +
  scale_y_continuous(name = 'Number of new clusters',
                     expand = expansion(mult = c(0., 0.1)),
                     sec.axis = sec_axis(~ (. + scaling_intercept_y_axis)/ scaling_slope_y_axis, 
                                         name = 'Mean cluster size')) +
  theme_classic() +
  coord_cartesian(xlim = c(0, 95),  ylim = c(0, 2100), clip = "off") + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.y.right = element_text(colour = 'orange2'),
        axis.title.y.right = element_text(colour = 'orange2'),
        axis.line.y.right = element_line(colour = 'orange2'),
        axis.ticks.y.right = element_line(colour = 'orange2'),
        plot.margin = unit(c(1,4,1,1), "lines"))


pdf('../../figures/supplementary_figures/charact_clusters_over_time.pdf',
    height = 3., width = 8)
plot(plt_cluster_charact_over_time)
dev.off()

png('../../figures/supplementary_figures/charact_clusters_over_time.png',
    height = 3., width = 8, res = 350, units = 'in')
plot(plt_cluster_charact_over_time)
dev.off()


