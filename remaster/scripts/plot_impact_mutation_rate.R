library(tidyverse)
library(ggpubr)
library(RColorBrewer)

## Load dataframe with correlation coefficient per simulated dataset
df_cor_RR_by_mut_rate <- read_csv('../results_remaster/df_summary_impact_mutation_rate.csv')



## Plot parameters
width_left <- 0.08
dodge_left <- 0.6
width_right <- 0.6
dodge_right <- 0.6

## Make plot for each distance threshold
list_plots <- lapply(0:10, FUN = function(curr_threshold){
  curr_df_RR <- df_cor_RR_by_mut_rate %>% 
    filter(n_mut_threshold == curr_threshold) %>% 
    group_by(n_mut_threshold, mut_rate_scaling) %>% 
    summarise(y05 = quantile(cor_spearman, 0.025),
              y25 = quantile(cor_spearman, 0.25),
              y50 = quantile(cor_spearman, 0.5),
              y75 = quantile(cor_spearman, 0.75),
              y95 = quantile(cor_spearman, 0.975)) %>% 
    mutate(`Mut. threshold` = n_mut_threshold) 

  left_plot <- curr_df_RR %>% 
    filter(mut_rate_scaling <= 3.) %>% 
    ggplot(aes(x = mut_rate_scaling, colour = as.factor(n_mut_threshold))) +
    geom_boxplot(aes(ymin = y05, lower = y25, middle = y50, upper = y75, ymax = y95,
                     group = interaction(n_mut_threshold, mut_rate_scaling)),
                 stat = "identity", position = position_dodge(dodge_left), 
                 width = width_left
    ) +
    scale_x_continuous(name = 'Mutation rate scaling', breaks = seq(0., 10., 1)) +
    scale_y_continuous(name = 'Spearman correlation',
                       breaks = seq(-1, 1., 0.5),
                       limits = c(-1, 1.)) + 
    scale_colour_manual(breaks = c(0:10),
                        values =  rev(viridis::magma(12)[-12]),
                        name = 'Genomic distance\nthreshold\n(# of mutations)') +
    theme_classic() +
    theme(legend.position = 'none',
          axis.text = element_text(size = 12), 
          axis.title = element_blank()) +
    ggtitle(paste0(curr_threshold, ' mutation threshold'))
  
  right_plot <- curr_df_RR %>% 
    filter(mut_rate_scaling > 3.) %>% 
    ggplot(aes(x = mut_rate_scaling, colour = as.factor(n_mut_threshold))) +
    geom_boxplot(aes(ymin = y05, lower = y25, middle = y50, upper = y75, ymax = y95,
                     group = interaction(n_mut_threshold, mut_rate_scaling)),
                 stat = "identity", position = position_dodge(dodge_right), 
                 width = width_right
    ) +
    scale_x_continuous(name = 'Mutation rate scaling', breaks = seq(0., 10., 1)) +
    scale_y_continuous(name = 'Spearman correlation',
                       breaks = seq(-1, 1., 0.5),
                       limits = c(-1, 1.)) + 
    scale_colour_manual(breaks = c(0:10),
                        values =  rev(viridis::magma(12)[-12]),
                        name = 'Genomic distance\nthreshold\n(# of mutations)') +
    theme_classic() +
    theme(axis.text = element_text(size = 12), 
          axis.title.x = element_blank(), 
          axis.line.y = element_blank(),
          axis.title = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()
    )+
    ggtitle('')
  
  
  ggarrange(left_plot + coord_cartesian(ylim = c(-1., 1.)), 
            right_plot + coord_cartesian(ylim = c(-1., 1.)), legend = 'none', widths = c(2., 1.))
})

## Plot for the legend
plt_with_legend <- tibble(n_mut_threshold = 0:10) %>% 
  ggplot(aes(x = n_mut_threshold, y = n_mut_threshold,
             colour = as.factor(n_mut_threshold))) +
  geom_point(alpha = 0) +
  scale_colour_manual(breaks = c(0:10),
                      values =  rev(viridis::magma(12)[-12]),
                      name = 'Genomic distance\nthreshold\n(# of mutations)') +
  guides(colour = guide_legend(override.aes = list(alpha=1), ncol = 3)) +
  theme_void() +
  theme(legend.position = c(0.5, 0.5))

## Panel with thresholds
panel_all_thresholds <- ggarrange(
  ggarrange(plotlist = list_plots[1:3], ncol = 3) %>% annotate_figure(left = 'Spearman correlation'),
  ggarrange(plotlist = list_plots[4:6], ncol = 3) %>% annotate_figure(left = 'Spearman correlation'),
  ggarrange(plotlist = list_plots[7:9], ncol = 3) %>% annotate_figure(left = 'Spearman correlation'),
  ggarrange(list_plots[10][[1]], list_plots[11][[1]], plt_with_legend,  ncol = 3) %>% annotate_figure(left = 'Spearman correlation'),
  nrow = 4
) %>% 
  annotate_figure(bottom = text_grob('Mutation rate scaling compared to a SARS-CoV-2 like pathogen'))


# png('../mutation_rate_hamming_distance_threshold_v2.png', height = 8, width = 11.5,
#     res = 350, units = 'in')
plot(panel_all_thresholds)
#dev.off()