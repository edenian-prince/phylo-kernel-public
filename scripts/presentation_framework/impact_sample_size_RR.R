## This script enables to reproduce Figures S34 and S27 detailing the results
## of the subsampling analysis used to understand the dataset size required
## to estimate the RR

library(tidyverse)
library(viridis)
library(ggpubr)
library(RColorBrewer)
library(scales)

## Load dataframe with the results of the subsampling analysis
df_RR_incremental <- read_csv('../../results/sampling_analysis/df_RR_incremental_by_group_size.csv') %>% 
  mutate(abs_diff_RR = RR - RR_full,
         rel_diff_RR = abs_diff_RR / RR_full)

## Figure S34 A. Illustration of how the RR stabilises as more sequences are included in the analysis
## Example with RR within the 0-9y
plt_illustration_CV <- df_RR_incremental %>% 
  filter(i_rep <= 10, group_1 == '0-9y', group_2 == '0-9y') %>% 
  ggplot(aes(x = n_pairs, y = RR, colour = as.factor(i_rep), group = i_rep)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(trans = 'log', name = 'Number of pairs',
                     breaks = c(seq(1, 9, 1), seq(1e1, 9e1, 1e1), seq(1e2, 9e2, 1e2),
                                seq(1e3, 9e3, 1e3), seq(1e4, 9e4, 1e4), seq(1e5, 9e5, 1e5)),
                     labels = c(expression(10^{0}), rep('', 8),
                                expression(10^{1}), rep('', 8),
                                expression(10^{2}), rep('', 8),
                                expression(10^{3}), rep('', 8),
                                expression(10^{4}), rep('', 8),
                                expression(10^{5}), rep('', 8))) +
  scale_y_continuous(trans = 'log', name = expression(RR^d),
                     breaks = c(seq(0.1, 0.9, 0.1), seq(1, 9, 1), seq(1e1, 9e1, 1e1), seq(1e2, 9e2, 1e2),
                                seq(1e3, 9e3, 1e3), seq(1e4, 9e4, 1e4), seq(1e5, 9e5, 1e5)),
                     labels = c(expression(10^{-1}), rep('', 8),
                                expression(10^{0}), rep('', 8),
                                expression(10^{1}), rep('', 8),
                                expression(10^{2}), rep('', 8),
                                expression(10^{3}), rep('', 8),
                                expression(10^{4}), rep('', 8),
                                expression(10^{5}), rep('', 8))) +
  scale_colour_manual(name = 'ID Replicate', values = brewer.pal(12, 'Paired')) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12))

## Figure S34 B - Illustration of how the error decreases as the sample size increases
plt_error_across_nb_age_groups <- df_RR_incremental %>% 
  mutate(nb_age_groups_char = paste0(nb_age_groups, ' age groups')) %>% 
  ggplot(aes(x = n_pairs, y = rel_diff_RR, colour = n_seq_to_sample)) +
  geom_point(alpha = 0.01) +
  geom_hline(yintercept = 0.1, linetype = 'dashed') +
  geom_hline(yintercept = - 0.1, linetype = 'dashed') +
  scale_x_continuous(name = 'Number of pairs',
                     trans = 'log', breaks = c(1, 10, 100, 1000, 1e4, 1e5, 1e6),
                     labels = c(expression(10^{0}), expression(10^{1}),
                                expression(10^{2}), expression(10^{3}),
                                expression(10^{4}), expression(10^{5}),
                                expression(10^{6}))) +
  scale_y_continuous(name = 'Error', labels = label_percent()) +
  scale_colour_viridis(trans = 'log', name = 'Dataset size\n', 
                       breaks = c(1e2, 1e3, 1e4, 1e5),
                       labels = c(expression(10^{2}), expression(10^{3}),
                                  expression(10^{4}), expression(10^{5}))) +
  theme_bw() +
  coord_cartesian(ylim = c(-1, 1.)) +
  facet_wrap(. ~ nb_age_groups_char) +
  theme(strip.background = element_rect(fill = 'gray22'),
        strip.text = element_text(colour = 'white', size = 12),
        axis.text = element_text(size = 12), axis.title = element_text(size = 12),
        legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        legend.position = 'bottom')


### Make a panel with these two figures
blank_plot <- tibble() %>% ggplot() + theme_void()

panel_subsampling_strategy <- ggarrange(ggarrange(plt_illustration_CV + 
                                                    guides(colour = guide_legend(ncol = 2)) + 
                                                    theme(legend.position = 'bottom'),
                                                  blank_plot, nrow = 2, heights = c(1., 0.4)),
                                        plt_error_across_nb_age_groups, labels = 'AUTO', ncol = 2,
                                        widths = c(1.1, 2.))


# pdf('../../plots/impact_dataset_size/panel_illustration_downsampling_strategy.pdf',
#     height = 6., width = 9)
plot(panel_subsampling_strategy)
# dev.off()

##############
## Impact of the number of groups included in the analysis on the dataset size 
## required for the error in the RR of observing identical sequences
## to be lower than 10%.


## Figure S27 A - Number of pairs required to reach an error < 10% for the RR 
## as a function of the number of age groups required

# Dataframe used for the boxplot
df_for_boxplot <- df_RR_incremental %>% 
  group_by(i_rep, nb_age_groups) %>% 
  filter(abs(rel_diff_RR) > 0.1) %>% 
  summarise(max_n_pairs = max(n_pairs)) %>%
  group_by(nb_age_groups) %>% 
  summarise(y00 = quantile(max_n_pairs, 0.025),
            y25 = quantile(max_n_pairs, 0.25),
            y50 = quantile(max_n_pairs, 0.5),
            y75 = quantile(max_n_pairs, 0.75),
            y100 = quantile(max_n_pairs, 0.975))

plt_nb_pairs_required <- df_for_boxplot %>% 
  ggplot(aes(x = as.factor(nb_age_groups))) +
  geom_boxplot(
    aes(ymin = y00, lower = y25, middle = y50, upper = y75, ymax = y100),
    stat = 'identity', fill = 'gray80', colour = 'gray22', width = 0.6
  ) +
  scale_x_discrete(name = 'Number of age groups') +
  scale_y_continuous(trans = 'log', name = 'Number of pairs required\nfor error < 10%',
                     breaks = c(seq(1, 9, 1), seq(1e1, 9e1, 1e1), seq(1e2, 9e2, 1e2),
                                seq(1e3, 9e3, 1e3), 1e4),
                     labels = c(expression(10^{0}), rep('', 8),
                                expression(10^{1}), rep('', 8),
                                expression(10^{2}), rep('', 8),
                                expression(10^{3}), rep('', 8),
                                expression(10^{4}))) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12))

## Figure S27 B - Number of sequences required to reach an error < 10% for the RR 
## as a function of the number of age groups required
plt_corr_nb_sequences <- df_RR_incremental %>% 
  left_join(df_for_boxplot) %>% 
  filter(n_pairs >= y50) %>% 
  group_by(nb_age_groups, group_1, group_2, i_rep) %>% 
  filter(n_seq_to_sample == min(n_seq_to_sample)) %>% 
  ungroup() %>% 
  filter(group_1 == group_2) %>% 
  mutate(nb_age_groups_char = paste0(nb_age_groups, ' age\ngroups')) %>% 
  ggplot(aes(x = group_1, y = n_seq_to_sample)) +
  geom_jitter(height = 0., width = 0.3, alpha = 0.3, color = 'darkgrey') +
  stat_summary(fun = 'median', color = 'darkslateblue', shape = 17, size = 0.8) +
  scale_x_discrete(name = '') +
  scale_y_continuous(breaks = c(seq(1, 9, 1), seq(1e1, 9e1, 1e1), seq(1e2, 9e2, 1e2),
                                seq(1e3, 9e3, 1e3), seq(1e4, 9e4, 1e4), seq(1e5, 9e5, 1e5)),
                     labels = c(expression(10^{0}), rep('', 8),
                                expression(10^{1}), rep('', 8),
                                expression(10^{2}), rep('', 8),
                                expression(10^{3}), rep('', 8),
                                expression(10^{4}), rep('', 8),
                                expression(10^{5}),  rep('', 8)),
                     name = 'Number of sequences required',
                     trans = 'log',
                     limits = c(1e2, 2e5)) +
  facet_grid( ~ nb_age_groups_char, scales = "free_x", space = "free_x") +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'gray22'),
        strip.text = element_text(colour = 'white', size = 12),
        axis.text = element_text(size = 12), axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1.),
        legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        legend.position = 'bottom',
        panel.grid.minor = element_blank()) 

## Make a panel
panel_nb_sequences_required <- ggarrange(ggarrange(blank_plot, 
                                                   plt_nb_pairs_required, 
                                                   blank_plot,
                                                   nrow = 1, ncol = 3,
                                                   widths = c(0.5, 1., 0.5),
                                                   labels = c('', 'A', '')),
                                         plt_corr_nb_sequences, nrow = 2, labels = c('', 'B'))

# pdf('../../plots/impact_dataset_size/plt_number_seq_required.pdf',
#     height = 8., width = 8)
# plot(panel_nb_sequences_required)
# dev.off()

