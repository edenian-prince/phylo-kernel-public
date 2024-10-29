## This script compares the number of visits reported in the Safegraph mobility data
## and the number of work commutes from the workflow data.

library(tidyverse)

## Load number of visits between counties from Safegraph mobility data
df_visits <- read.csv('../data/mobility/safegraph_visits_march_2021_june_2022.csv', row.names = NULL) %>% 
  as_tibble()

## Add flows equal to 0 to the Safegraph data
df_visits_full <- expand.grid(county_origin = unique(df_visits$county_origin),
                              county_destination = unique(df_visits$county_destination)) %>% 
  left_join(df_visits)

## Load workflow data
df_workflows <- read.csv('../data/mobility/commuting_flows_WA_2020.csv', row.names = NULL) %>% 
  as_tibble() %>% select(-X)

## Create a dataframe with both the Safegraph and workflow mobility data
df_for_comparison <- df_visits_full %>% 
  full_join(df_workflows, by = c('county_origin' = 'county_residence', 'county_destination' = 'county_workplace')) %>% 
  mutate(n_workflows = replace_na(n_workflows, 0.),
         n_scaled_visits_county = replace_na(n_scaled_visits_county, 0.))

## Correlation coefficients between Safegraph flows and commuting flows
cor_spearman <- round(cor(df_for_comparison$n_scaled_visits_county, df_for_comparison$n_workflows, method = 'spearman'), 2)
cor_pearson <- round(cor(df_for_comparison$n_scaled_visits_county, df_for_comparison$n_workflows, method = 'pearson'), 2)

## Display the relationship between the Safegraph and commuting flows
yaxis_zero <- 0.1
xaxis_zero <- 0.1

plt_comp_workflow_safegraph <- df_for_comparison %>% 
  mutate(n_workflows_plot = ifelse(n_workflows == 0., xaxis_zero, n_workflows),
         n_scaled_visits_county_plot = ifelse(n_scaled_visits_county == 0., yaxis_zero, n_scaled_visits_county)) %>% 
  ggplot(aes(x = n_workflows_plot, y = n_scaled_visits_county_plot)) +
  geom_point(alpha = 0.2) +
  geom_text(data = tibble(n_scaled_visits_county = 1., 
                          n_workflows = 1.,
                          n_scaled_visits_county_plot = 1e8,
                          n_workflows_plot = 2),
            aes(label = paste0('Spearman r = ', cor_spearman)), hjust = 0.) +
  geom_text(data = tibble(n_scaled_visits_county = 1., 
                          n_workflows = 1.,
                          n_scaled_visits_county_plot = 3e7,
                          n_workflows_plot = 2),
            aes(label = paste0('Pearson r = ', cor_pearson)), hjust = 0.) +
  scale_x_continuous(trans = 'log', 
                     expand = expansion(add = c(0.2, 0.2)),
                     name = 'Number of work commutes (Commuting data)',
                     breaks = c(xaxis_zero, 1, 10, 1e2, 1e3, 1e4, 1e5, 1e6),
                     labels = c('0', expression(10^{0}), expression(10^{1}), expression(10^{2}), 
                                expression(10^{3}), expression(10^{4}), expression(10^{5}),
                                expression(10^{6}))) +
  scale_y_continuous(trans = 'log', 
                     expand = expansion(add = c(0.2, 0.2)),
                     name = 'Number of visits (Safegraph data)',
                     breaks = c(yaxis_zero, 1, 10, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10),
                     labels = c('0', expression(10^{0}), expression(10^{1}), expression(10^{2}), 
                                expression(10^{3}), expression(10^{4}), expression(10^{5}),
                                expression(10^{6}), expression(10^{7}), expression(10^{8}), 
                                expression(10^{9}), expression(10^{10}))) +
  facet_grid((n_scaled_visits_county == 0.) ~ (n_workflows != 0.), 
             space = 'free', scales = 'free') +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 12))

#pdf('../plots/figure_mobility/comparison_workflow_safegraph.pdf', height = 4., width = 4.5)
plot(plt_comp_workflow_safegraph)
#dev.off()
