## This script reproduces the subsampling analysis used
## to quantify the marginal gain of adding an additional 
## sequence in the dataset (in terms of added information)

library(colorspace)
library(tidyverse)

## Load dataframe with the results of the resampling analysis
df_subsamples_100 <- read_csv('../../results/sampling_analysis/df_marginal_gain_additional_sequence.csv')
### Column characteristics
### i_subsample: replicate ID (for each replicate ID, sequences are progressively included in the analysis)
### curr_subsample_step : Dataset size
### n_new_clusters_per_sample: number of new clusters of identical sequences per additional sequence
### n_new_seq_non_singleton_per_sample: additional number of sequences that have another identical sequence per additional sequence


## Define colours for the plot
col_signal_RR <- 'darkcyan'
col_signal_DTA <- 'firebrick'


## Make plot of the number of additional clusters of identical sequences per additional sequence
## and of the number of additional sequences with another identical sequences in the dataset
## per additional sequence included in the dataset as a function of the dataset size.

plt_marginal_value_additional_sequence <- df_subsamples_100  %>% 
  select(i_subsample_step) %>% 
  filter(curr_subsample_step >= 1000) %>% 
  ggplot(aes(x = curr_subsample_step)) +
  geom_point(aes(y = n_new_clusters_per_sample), 
             col = lighten(col_signal_DTA, 0.5), 
             alpha = 0.02) +
  geom_point(aes(y = n_new_seq_non_singleton_per_sample), 
             col = lighten(col_signal_RR, 0.5), alpha = 0.02) +
  geom_smooth(aes(y = n_new_clusters_per_sample), 
              col = col_signal_DTA, fill = col_signal_DTA, alpha = 0.5,
              method = 'loess') +
  geom_smooth(aes(y = n_new_seq_non_singleton_per_sample), 
              col = col_signal_RR, fill = col_signal_RR, alpha = 0.5,
              method = 'loess') +
  scale_x_continuous(trans = 'log', breaks = c(seq(1000, 9000, 1e3), 
                                               seq(1e4, 9e4, 1e4), 1e5),
                     labels = c(expression(10^{3}), rep('', 8),
                                expression(10^{4}), rep('', 8), expression(10^{5})),
                     name = 'Dataset size (in number of sequences)') +
  scale_y_continuous(breaks = seq(0., 1., 0.2), 
                     name = 'Additional clusters of identical\nsequences per additional sequence',
                     sec.axis = sec_axis(~. * 1., name = 'Additional sequences with another identical\nsequence per additional sequence\n',
                                         breaks = seq(0., 1., 0.2))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13),
        axis.title = element_text(size = 12),
        axis.line.y.left = element_line(colour = col_signal_DTA),
        axis.line.y.right = element_line(colour = col_signal_RR),
        axis.ticks.y.left = element_line(colour = col_signal_DTA),
        axis.ticks.y.right = element_line(colour = col_signal_RR),
        axis.title.y.left = element_text(colour = col_signal_DTA),
        axis.title.y.right = element_text(colour = col_signal_RR),
        axis.text.y.left = element_text(colour = col_signal_DTA, size = 12),
        axis.text.y.right = element_text(colour = col_signal_RR, size = 12),
        panel.grid.minor = element_blank())

# pdf('../../plots/value_framework/marginal_signal_additional_sequences_100.pdf',
#     height = 4., width = 5.5)
plot(plt_marginal_value_additional_sequence)
#dev.off()
