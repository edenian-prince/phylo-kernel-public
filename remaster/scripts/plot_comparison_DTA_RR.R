library(tidyverse)
library(ggpubr)
library(RColorBrewer)

## Define file paths for the results of the simulations
file_path_biased_DTA <- '../results/migration_rate_tables/sym_dta/migration_rates_beast_rep-2-seq-density-01-biased.csv'
file_path_unbiased_DTA <- '../results/migration_rate_tables/sym_dta/migration_rates_beast_rep-2-seq-density-01-unbiased.csv'
file_path_biased_RR <- '../results_remaster/5-demes-rep-1-seq-density-2-biased-pop-100k-higher-beta/df_RR.rds'
file_path_unbiased_RR <- '../results_remaster/5-demes-rep-1-seq-density-2-unbiased-pop-100k-higher-beta/df_RR.rds'
file_path_biased_uncertainty_RR <- '../results_remaster/5-demes-rep-1-seq-density-2-biased-pop-100k-higher-beta/df_uncertainty_RR.rds'
file_path_unbiased_uncertainty_RR <- '../results_remaster/5-demes-rep-1-seq-density-2-unbiased-pop-100k-higher-beta/df_uncertainty_RR.rds'

## Load true migration rates used to run the simulations
## and compute the daily movement probability
true_df_migration <- readRDS('../input_files/migration_matrix_5_demes.rds') %>% 
  as_tibble() %>% 
  mutate(subgroup_1 = 0:4) %>% 
  pivot_longer(cols = - 'subgroup_1', values_to = 'migration_rate', names_to = 'subgroup_2', names_prefix = 'V') %>% 
  mutate(subgroup_2 = as.integer(as.numeric(subgroup_2) - 1)) %>% 
  mutate(daily_proba_migration = ifelse(subgroup_1 == subgroup_2, 0., 1. - exp(-migration_rate * 1))) %>% 
  group_by(subgroup_1) %>% 
  mutate(daily_proba_migration = ifelse(daily_proba_migration == 0., 1. - sum(daily_proba_migration), daily_proba_migration)) %>% 
  ungroup()

## Load DTA results
df_DTA_biased <- read_csv(file_path_biased_DTA) %>% select(- `...1`) %>% 
  left_join(true_df_migration, by = c('origin' = 'subgroup_1', 'destination' = 'subgroup_2'))
df_DTA_unbiased <- read_csv(file_path_unbiased_DTA) %>% select(- `...1`) %>% 
  left_join(true_df_migration, by = c('origin' = 'subgroup_1', 'destination' = 'subgroup_2'))
df_combined_DTA <- bind_rows(df_DTA_biased %>% mutate(type = 'Biased'), df_DTA_unbiased %>% mutate(type = 'Unbiased'))

## Load RR results
df_RR_biased <- readRDS(file_path_biased_RR) %>% filter(subgroup_1 >= subgroup_2) %>% 
  left_join(true_df_migration, by = c('subgroup_1', 'subgroup_2')) %>%
  mutate(RR = exp(log_RR))
df_RR_unbiased <- readRDS(file_path_unbiased_RR) %>% filter(subgroup_1 >= subgroup_2) %>% 
  left_join(true_df_migration, by = c('subgroup_1', 'subgroup_2')) %>%
  mutate(RR = exp(log_RR))
df_combined_RR <- bind_rows(df_RR_biased %>% mutate(type = 'Biased'), df_RR_unbiased %>% mutate(type = 'Unbiased'))

df_uncertainty_RR_biased <- readRDS(file_path_biased_uncertainty_RR)%>% 
  group_by(subgroup_1, subgroup_2) %>% 
  summarise(median_RR = exp(median(log_RR)), upper_RR = exp(quantile(log_RR, 0.025)), lower_RR = exp(quantile(log_RR, 0.975))) %>% 
  filter(subgroup_1 >= subgroup_2)
df_uncertainty_RR_unbiased <- readRDS(file_path_unbiased_uncertainty_RR)%>% 
  group_by(subgroup_1, subgroup_2) %>% 
  summarise(median_RR = exp(median(log_RR)), upper_RR = exp(quantile(log_RR, 0.025)), lower_RR = exp(quantile(log_RR, 0.975))) %>% 
  filter(subgroup_1 >= subgroup_2)

df_combined_uncertainty_RR <- bind_rows(df_uncertainty_RR_biased %>% mutate(type = 'Biased'), df_uncertainty_RR_unbiased %>% mutate(type = 'Unbiased'))

####################
col_RR <- 'firebrick'
col_DTA <- 'darkslateblue'

# Figure A - Compare DTA estimates to migration rates
df_comp_dta <- df_combined_DTA %>% 
  select(origin, destination, instant_rates, instant_rates_upper_95hpd, instant_rates_lower_95hpd, type) %>% 
  group_by(origin, destination) %>% 
  pivot_wider(names_from = 'type', values_from = c('instant_rates', 'instant_rates_upper_95hpd', 'instant_rates_lower_95hpd'))

plt_comp_dta_biased_unbiased <- df_comp_dta %>% 
  ggplot() +
  geom_abline(slope = 1., intercept = 0., linetype = 'dashed', color = 'darkgrey') +
  stat_cor(method = 'spearman', cor.coef.name = 'rho',
           aes(x = instant_rates_Unbiased, y = instant_rates_Biased),
           label.x = -Inf, label.y = Inf, vjust = 2., hjust = -0.2) +
  geom_linerange(aes(x = instant_rates_Unbiased, 
                     ymin = instant_rates_lower_95hpd_Biased, ymax = instant_rates_upper_95hpd_Biased),
                 colour = col_DTA) +
  geom_linerange(aes(y = instant_rates_Biased, 
                     xmin = instant_rates_lower_95hpd_Unbiased, xmax = instant_rates_upper_95hpd_Unbiased),
                 alpha = 0.5,
                 colour = col_DTA)  +
  geom_point(aes(x = instant_rates_Unbiased, y = instant_rates_Biased), size = 2,
             colour = col_DTA) +
  scale_x_continuous(trans = 'log', name = 'Migration rates from unbiased sampling',
                     breaks = c(seq(1e-4, 9e-4, 1e-4), seq(1e-3, 9e-3, 1e-3), seq(1e-2, 9e-2, 1e-2)),
                     labels = c(expression(10^{-4}), rep('', 8), expression(10^{-3}), rep('', 8), expression(10^{-2}), rep('', 8))) + 
  scale_y_continuous(trans = 'log', name = 'Migration rates from biased sampling',
                     breaks = c(seq(1e-4, 9e-4, 1e-4), seq(1e-3, 9e-3, 1e-3), seq(1e-2, 9e-2, 1e-2)),
                     labels = c(expression(10^{-4}), rep('', 8), expression(10^{-3}), rep('', 8), expression(10^{-2}), rep('', 8))) +
  coord_cartesian(ylim = c(1e-3, NA), xlim = c(1e-3, NA)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        panel.spacing = unit(1, "lines"))


# Figure B - Compare RR estimates to migration probabilities
plt_comp_RR_migration_proba <- df_combined_uncertainty_RR %>% 
  left_join(true_df_migration) %>% 
  left_join(df_combined_RR) %>% 
  mutate(type = factor(type, levels= c('Unbiased', 'Biased'))) %>% 
  filter(subgroup_1 >= subgroup_2) %>% 
  ggplot(aes(x = daily_proba_migration, y = RR)) +
  geom_smooth(method = 'lm', colour = col_RR, fill = col_RR) +
  geom_point() +
  stat_cor(method = 'spearman', cor.coef.name = 'rho',
           label.x = -Inf, label.y = Inf, vjust = 2., hjust = -0.2) +
  geom_linerange(aes(ymin = lower_RR, ymax = upper_RR)) +
  scale_x_continuous(trans = 'logit',
                     name = 'Daily migration probability',
                     breaks = c(0.0001, 0.001, 0.01, 0.1, 0.5, 0.9, 0.99, 0.999, 0.9999))  +
  scale_y_continuous(trans = 'log',
                     breaks = c(0.1, 0.2, 0.5, 1., 2., 5., 10),
                     name = expression(RR['identical sequences'])) +
  facet_wrap(. ~ type, nrow = 1, ncol = 2) +
  
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1.),
        strip.background = element_rect(fill = 'gray22'),
        strip.text = element_text(colour = 'white', size = 12),
        panel.spacing = unit(1, "lines"))

# Figure C - Compare DTA estimates in the biased and unbiased scenario
plt_comp_dta_migration_rates <- df_combined_DTA %>% 
  mutate(type = factor(type, levels = c('Unbiased', 'Biased'))) %>% 
  ggplot(aes(x = migration_rate, y = instant_rates)) +
  stat_cor(method = 'spearman', cor.coef.name = 'rho',
           label.x = -Inf, label.y = Inf, vjust = 2., hjust = -0.2) +
  geom_point() +
  geom_smooth(method = 'lm', colour = col_DTA, fill = col_DTA) +
  geom_linerange(aes(ymin = instant_rates_upper_95hpd, ymax = instant_rates_lower_95hpd)) +
  facet_wrap(. ~ type) +
  scale_x_continuous(trans = 'log', name = 'True migration rate',
                     breaks = c(seq(1e-4, 9e-4, 1e-4), seq(1e-3, 9e-3, 1e-3), seq(1e-2, 9e-2, 1e-2)),
                     labels = c(expression(10^{-4}), rep('', 8), expression(10^{-3}), rep('', 8), expression(10^{-2}), rep('', 8))) + 
  scale_y_continuous(trans = 'log', name = 'Estimated migration rates',
                     breaks = c(seq(1e-4, 9e-4, 1e-4), seq(1e-3, 9e-3, 1e-3), seq(1e-2, 9e-2, 1e-2)),
                     labels = c(expression(10^{-4}), rep('', 8), expression(10^{-3}), rep('', 8), expression(10^{-2}), rep('', 8))) +
  theme_bw() +
  coord_cartesian(ylim = c(1e-3, NA)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        strip.background = element_rect(fill = 'gray22'),
        strip.text = element_text(colour = 'white', size = 12),
        panel.spacing = unit(1, "lines"))

# Figure D - Compare RR estimates in the biased and unbiased scenario
df_comp_RR <- df_combined_RR %>% select(subgroup_1, subgroup_2, RR, type) %>% 
  left_join(df_combined_uncertainty_RR, by = c('subgroup_1', 'subgroup_2', 'type')) %>% 
  select(subgroup_1, subgroup_2, RR, lower_RR, upper_RR, type) %>% 
  group_by(subgroup_1, subgroup_2) %>% 
  pivot_wider(names_from = 'type', values_from = c('RR', 'lower_RR', 'upper_RR')) %>% 
  ungroup()

plt_comp_RR_biased_unbiased <- df_comp_RR %>% 
  filter(subgroup_1 >= subgroup_2) %>% 
  ggplot() +
  geom_abline(slope = 1., intercept = 0., linetype = 'dashed', color = 'darkgrey') +
  stat_cor(aes(x = RR_Unbiased, y = RR_Biased), method = 'spearman', cor.coef.name = 'rho',
           label.x = -Inf, label.y = Inf, vjust = 2., hjust = -0.2) +
  geom_point(aes(x = RR_Unbiased, y = RR_Biased), colour = col_RR) +
  geom_linerange(aes(x = RR_Unbiased, ymin = lower_RR_Biased, ymax = upper_RR_Biased), colour = col_RR) +
  geom_linerange(aes(y = RR_Biased, xmin = lower_RR_Unbiased, xmax = upper_RR_Unbiased), colour = col_RR)  +
  scale_x_continuous(trans = 'log', name = 'RR from unbiased sampling', breaks = c(0.5, 1., 2., 5., 10)) + 
  scale_y_continuous(trans = 'log', name = 'RR from biased sampling',  breaks = c(0.5, 1., 2., 5., 10)) +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

######## Merge panels
panel_pop_10000 <- 
  ggarrange(ggarrange(plt_comp_dta_migration_rates + ggtitle('Discrete Trait Analysis (~1750 sequences)'),
                      plt_comp_RR_migration_proba + ggtitle('RR Analysis (~34500 sequences)'), ncol = 2,
                      labels = c('A', 'B')),
            ggarrange(plt_comp_dta_biased_unbiased  +  ggtitle('Discrete Trait Analysis'), 
                      plt_comp_RR_biased_unbiased +  ggtitle('RR Analysis'),
                      labels = c('C', 'D'),
                      ncol = 2),
            nrow = 2)

plot(panel_pop_10000)

#pdf('../../figures/supplementary_figures/comparison_dta_RR.pdf', height = 7, width = 10)
plot(panel_pop_10000)
#dev.off()
