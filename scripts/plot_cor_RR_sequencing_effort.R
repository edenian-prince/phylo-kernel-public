library(dplyr)
library(ggplot2)
library(viridis)

## Load relative risk of observing identical sequences between counties
df_RR_counties <- readRDS('../results/RR_county/df_RR_county_0_mut_away.rds')

## Load number of sequences from the different counties
df_seq_per_county <- read_csv('../data/sequence_info/sequences_per_county.csv', col_select = - 1)

df_RR_counties_for_comparison <- df_RR_counties %>% 
  left_join(df_seq_per_county,by = c('group_1' = 'county')) %>% 
  group_by(group_1) %>% 
  mutate(n_pairs_1_x = sum(n_pairs)) %>% 
  group_by(group_2) %>% 
  mutate(n_pairs_x_2 = sum(n_pairs)) %>% 
  ungroup() %>%
  mutate(n_pairs_x_x = sum(n_pairs)) # 1 is B, 2 is A
  
zero_value_prop <- 1e-7
zero_value_RR <- 1e-7

## Proportion of pairs in counties AB among pairs in A as a function of the proportion of pairs observed in B
df_1 <- df_RR_counties_for_comparison %>% 
  mutate(x_to_plot = n_pairs_1_x / n_pairs_x_x, y_to_plot = n_pairs / n_pairs_x_2) %>% 
  mutate(y_to_plot_cor = ifelse(y_to_plot == 0., zero_value_prop, y_to_plot),
         x_to_plot_cor = x_to_plot)
cor_1 <- df_1 %>% summarise(cor_spearman = cor(x_to_plot, y_to_plot, method = 'spearman')) %>% unlist() %>% as.numeric() %>% round(digits = 2)


plt_1 <- df_1 %>% 
  ggplot(aes(x = x_to_plot_cor, y = y_to_plot_cor)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(trans = 'log',
                     breaks = c(1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3, 1e4),
                     labels = c(expression(10^{-5}), expression(10^{-4}), 
                                expression(10^{-3}), expression(10^{-2}), 
                                expression(10^{-1}), expression(10^{0}),
                                expression(10^{1}), expression(10^{2}),
                                expression(10^{3}), expression(10^{4})),
                     name = 'Proportion of pairs of identical\nsequences observed in county B') +
  scale_y_continuous(trans = 'log',
                     breaks = c(zero_value_prop, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3, 1e4),
                     labels = c(0, expression(10^{-5}), expression(10^{-4}), 
                                expression(10^{-3}), expression(10^{-2}), 
                                expression(10^{-1}), expression(10^{0}),
                                expression(10^{1}), expression(10^{2}),
                                expression(10^{3}), expression(10^{4})),
                     name = 'Proportion of pairs in counties A-B\namong pairs in counties A',
                     expand = expansion(mult = c(0.1, 0.1))) +
  geom_text(data = tibble(x_to_plot_cor = 0.05, y_to_plot_cor = 1e-5, y_to_plot = 1.),
            label = paste0('Spearman r = ', cor_1), size = 4) +
  facet_grid((y_to_plot == 0.) ~ ., scales = 'free', space = 'free_y') +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.background = element_blank(),
        strip.text = element_blank()) 
  

## RR as a function of the proportion of pairs observed in B
df_2 <- df_RR_counties_for_comparison %>% 
  mutate(x_to_plot = n_pairs_1_x / n_pairs_x_x, y_to_plot = RR) %>% 
  mutate(y_to_plot_cor = ifelse(y_to_plot == 0., zero_value_RR, y_to_plot),
         x_to_plot_cor = x_to_plot)
cor_2 <- df_2 %>% summarise(cor_spearman = cor(x_to_plot, y_to_plot, method = 'spearman')) %>% unlist() %>% as.numeric() %>% round(digits = 2)


plt_2 <- df_2 %>% 
  ggplot(aes(x = x_to_plot_cor, y = y_to_plot_cor)) +
  geom_point(alpha = 0.5)  +
  scale_x_continuous(trans = 'log',
                     breaks = c(1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3, 1e4),
                     labels = c(expression(10^{-5}), expression(10^{-4}), 
                                expression(10^{-3}), expression(10^{-2}), 
                                expression(10^{-1}), expression(10^{0}),
                                expression(10^{1}), expression(10^{2}),
                                expression(10^{3}), expression(10^{4})),
                     name = 'Proportion of pairs of identical\nsequences observed in county B') +
  scale_y_continuous(trans = 'log',
                     breaks = c(zero_value_RR, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3, 1e4),
                     labels = c(0, expression(10^{-5}), expression(10^{-4}), 
                                expression(10^{-3}), expression(10^{-2}), 
                                expression(10^{-1}), expression(10^{0}),
                                expression(10^{1}), expression(10^{2}),
                                expression(10^{3}), expression(10^{4})),
                     name = 'RR of observing pairs of identical\nsequences in counties A-B',
                     expand = expansion(mult = c(0.1, 0.1))) +
  geom_text(data = tibble(x_to_plot_cor = 0.05, y_to_plot_cor = 1e4, y_to_plot = 1.),
            label = paste0('Spearman r = ', cor_2), size = 4) +
  facet_grid((y_to_plot == 0.) ~ ., scales = 'free', space = 'free_y') +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.background = element_blank(),
        strip.text = element_blank()) 

## Proportion of pairs in counties AB among pairs in A as a function of the number of sequences in B
df_3 <- df_RR_counties_for_comparison %>% 
  mutate(x_to_plot = n_seq, y_to_plot =  n_pairs / n_pairs_x_2) %>% 
  mutate(y_to_plot_cor = ifelse(y_to_plot == 0., zero_value_prop, y_to_plot),
         x_to_plot_cor = x_to_plot)
cor_3 <- df_3 %>% summarise(cor_spearman = cor(x_to_plot, y_to_plot, method = 'spearman')) %>% unlist() %>% as.numeric() %>% round(digits = 2)

plt_3 <- df_3 %>% 
  ggplot(aes(x = x_to_plot_cor, y = y_to_plot_cor)) +
  geom_point(alpha = 0.5)  +
  scale_x_continuous(trans = 'log',
                     breaks = c(1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3, 1e4),
                     labels = c(expression(10^{-5}), expression(10^{-4}), 
                                expression(10^{-3}), expression(10^{-2}), 
                                expression(10^{-1}), expression(10^{0}),
                                expression(10^{1}), expression(10^{2}),
                                expression(10^{3}), expression(10^{4})),
                     name = 'Number of sequences in county B') +
  scale_y_continuous(trans = 'log',
                     breaks = c(zero_value_prop, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3, 1e4),
                     labels = c(0, expression(10^{-5}), expression(10^{-4}), 
                                expression(10^{-3}), expression(10^{-2}), 
                                expression(10^{-1}), expression(10^{0}),
                                expression(10^{1}), expression(10^{2}),
                                expression(10^{3}), expression(10^{4})),
                     name = 'Proportion of pairs in counties A-B\namong pairs in counties A',
                     expand = expansion(mult = c(0.1, 0.1)))  +
  geom_text(data = tibble(x_to_plot_cor = 1e4, y_to_plot_cor = 1e-5, y_to_plot = 1.),
            label = paste0('Spearman r = ', cor_3), size = 4) +
  facet_grid((y_to_plot == 0.) ~ ., scales = 'free', space = 'free_y') +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.background = element_blank(),
        strip.text = element_blank()) 

## RR as a function of the number of sequences in B
df_4 <- df_RR_counties_for_comparison %>% 
  mutate(x_to_plot = n_seq, y_to_plot = RR) %>% 
  mutate(y_to_plot_cor = ifelse(y_to_plot == 0., zero_value_RR, y_to_plot),
         x_to_plot_cor = x_to_plot)
cor_4 <- df_4 %>% summarise(cor_spearman = cor(x_to_plot, y_to_plot, method = 'spearman')) %>% unlist() %>% as.numeric() %>% round(digits = 2)


plt_4 <- df_4 %>% 
  ggplot(aes(x = x_to_plot_cor, y = y_to_plot_cor)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(trans = 'log',
                     breaks = c(1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3, 1e4),
                     labels = c(expression(10^{-5}), expression(10^{-4}), 
                                expression(10^{-3}), expression(10^{-2}), 
                                expression(10^{-1}), expression(10^{0}),
                                expression(10^{1}), expression(10^{2}),
                                expression(10^{3}), expression(10^{4})),
                     name = 'Number of sequences in county B') +
  scale_y_continuous(trans = 'log',
                     breaks = c(zero_value_RR, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3, 1e4),
                     labels = c(0, expression(10^{-5}), expression(10^{-4}), 
                                expression(10^{-3}), expression(10^{-2}), 
                                expression(10^{-1}), expression(10^{0}),
                                expression(10^{1}), expression(10^{2}),
                                expression(10^{3}), expression(10^{4})),
                     name = 'RR of observing pairs of identical\nsequences in counties A-B',
                     expand = expansion(mult = c(0.1, 0.1)))  +
  geom_text(data = tibble(x_to_plot_cor = 5e3, y_to_plot_cor = 1e4, y_to_plot = 1.),
            label = paste0('Spearman r = ', cor_4), size = 4) +
  facet_grid((y_to_plot == 0.) ~ ., scales = 'free', space = 'free_y') +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.background = element_blank(),
        strip.text = element_blank()) 
  

panel <- ggarrange(plt_1, plt_2, plt_3, plt_4,
          labels = 'AUTO')

# pdf('../plots/figure_framework/RR_cor_sequencing_effort.pdf', height = 8, width = 8)
# plot(panel)
# dev.off()

plot(panel)

png('../plots/figure_framework/RR_cor_sequencing_effort.png', height = 8, width = 8,
    res = 350, units = 'in')
plot(panel)
dev.off()

