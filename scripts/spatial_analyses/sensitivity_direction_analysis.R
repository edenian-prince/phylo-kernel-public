## This script reproduces the sensitivity analysis exploring the robustness
## of the direction analysis when considering pairs of clusters where only two groups
## are observed

library(tidyverse)
library(ggpubr)

## Function to compute bootstrap CI around proportions
get_bootstrap_prop_CI <- function(x = 10, n = 100, n_bootstrap = 1000){
  obs_prop <- x / n
  
  vec_bootstrap <- rbinom(n = n_bootstrap, prob = obs_prop, size = n) / n
  list_res <- list(
    mean = mean(vec_bootstrap),
    median = median(vec_bootstrap),
    conf.int = quantile(vec_bootstrap, probs = c(0.025, 0.975))
  )
  return(list_res)
}

## Load Eastern / Western WA direction analysis data
### From clusters
df_clusters_WE <- read_csv('../../results/direction_transmission/df_timing_clusters_EW.csv')
### From pairs of ID seq
df_pairs_WE <- read_csv('../../results/direction_transmission/df_timing_pairs_EW.csv')

## Load region direction analysis data
### From clusters
df_clusters_regions <- read_csv('../../results/direction_transmission/df_timing_clusters_regions.csv')
### From pairs of ID seq
df_pairs_regions <- read_csv('../../results/direction_transmission/df_timing_pairs_regions.csv')

## Load age direction analysis data
### From clusters
df_clusters_ages <- read_csv('../../results/direction_transmission/df_timing_clusters_ages.csv') %>% 
  filter(n_tot_clusters != 0) %>% 
  group_by(age_decade_first, age_decade_last, i_wave) %>% 
  mutate(prop = n_clusters / n_tot_clusters,
         lower_prop = prop.test(x = n_clusters, n = n_tot_clusters)$conf.int[1],
         upper_prop = prop.test(x = n_clusters, n = n_tot_clusters)$conf.int[2],
         lower_prop_bootstrap = get_bootstrap_prop_CI(x = n_clusters, n = n_tot_clusters, n_bootstrap = 1e5)$conf.int[1],
         upper_prop_bootstrap = get_bootstrap_prop_CI(x = n_clusters, n = n_tot_clusters, n_bootstrap = 1e5)$conf.int[2])

### From pairs
df_pairs_ages <- read_csv('../../results/direction_transmission/df_timing_pairs_age_for_sensitivity.csv')

## A - Plot results for the direction analysis between Eastern and Western WA from clusters
col_border <- 'darkslateblue'

label_waves <- c('Wave 4\nMar-Jun 2021',
                 'Wave 5\nJul-Nov 2021',
                 'Wave 6\nDec 2021-Feb 2022',
                 'Wave 7\nMar 2022-Aug 2022')

plt_first_EW_among_clusters <- df_clusters_WE %>% 
  ggplot(aes(x = -i_wave)) +
  geom_point(aes(y = prop_west_before_east)) +
  geom_linerange(aes(ymin =  lower_prop, ymax =  upper_prop)) +
  coord_flip(ylim = c(0.2, 0.8),
             xlim = c(-4.2, -0.8)) +
  scale_x_continuous(name = '', breaks = seq(-4, -1), labels = rev(label_waves)) +
  scale_y_continuous(name = 'Proportion of clusters\nfirst collected in Western WA',
                     breaks = seq(0., 1., 0.1)) +
  geom_hline(linetype = 'dashed', color = col_border, yintercept = 0.5) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.grid.minor = element_blank())

## B - Plot comparison between results obtained from pairs and from clusters
## at the regional level
plt_comp_prop_regions <- df_pairs_regions %>% 
  left_join(df_clusters_regions, 
            by = c('region_1' = 'region_first', 'region_2' = 'region_last', 'i_wave')) %>% 
  filter(! is.na(prop)) %>% 
  mutate(is_signif.x = (lower_prop.x > 0.5 | upper_prop.x < 0.5),
         is_signif.y = (lower_prop.y > 0.5 | upper_prop.y < 0.5),
         is_signif = is_signif.x & is_signif.y) %>% 
  ggplot(aes(x = prop_1_before_2, y = prop)) +
  geom_linerange(aes(xmin = lower_prop.x, xmax = upper_prop.x, colour = is_signif), alpha = 0.2) +
  geom_linerange(aes(ymin = lower_prop.y, ymax = upper_prop.y, colour = is_signif), alpha = 0.2) +
  geom_point(aes(colour = is_signif), alpha = 0.8) +
  stat_cor(method = "spearman",  cor.coef.name = 'rho') +
  stat_cor(aes(colour = is_signif, alpha = is_signif), method = "spearman",  cor.coef.name = 'rho') +
  scale_colour_manual(breaks = c(T, F), values = c('firebrick', 'gray30')) +
  scale_x_continuous('Proportion from pairs',
                     breaks = c(0., 0.5, 1.)) +
  scale_y_continuous('Proportion from clusters',
                     breaks = c(0., 0.5, 1.)) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),)

## C - Plot comparison between results obtained from pairs and from clusters
## at the regional level stratifying by epidemic wave
plt_comp_prop_regions_by_wave <- df_pairs_regions %>% 
  left_join(df_clusters_regions, 
            by = c('region_1' = 'region_first', 'region_2' = 'region_last', 'i_wave')) %>% 
  filter(! is.na(prop)) %>% 
  mutate(is_signif.x = (lower_prop.x > 0.5 | upper_prop.x < 0.5),
         is_signif.y = (lower_prop.y > 0.5 | upper_prop.y < 0.5),
         is_signif = is_signif.x & is_signif.y) %>% 
  mutate(name_wave = paste0('Wave ', i_wave + 3)) %>% 
  ggplot(aes(x = prop_1_before_2, y = prop)) +
  geom_linerange(aes(xmin = lower_prop.x, xmax = upper_prop.x, colour = is_signif), alpha = 0.2) +
  geom_linerange(aes(ymin = lower_prop.y, ymax = upper_prop.y, colour = is_signif), alpha = 0.2) +
  geom_point(aes(colour = is_signif), alpha = 0.8) +
  stat_cor(method = "spearman",  cor.coef.name = 'rho') +
  stat_cor(aes(colour = is_signif, alpha = is_signif), method = "spearman",  cor.coef.name = 'rho')  +
  facet_wrap(. ~ name_wave, nrow = 1) +
  scale_colour_manual(breaks = c(T, F), values = c('firebrick', 'gray30')) +
  scale_x_continuous('Proportion from pairs',
                     breaks = c(0., 0.5, 1.)) +
  scale_y_continuous('Proportion from clusters',
                     breaks = c(0., 0.5, 1.)) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.background = element_rect(fill = 'gray22'),
        strip.text = element_text(colour = 'white'),
        panel.spacing = unit(1, "lines"))

## C - Plot comparison between results obtained from pairs and from clusters
## at the age group level stratifying by epidemic wave
plt_comp_prop_ages_by_wave <- df_pairs_ages %>% 
  left_join(df_clusters_ages, 
            by = c('age_decade_1' = 'age_decade_first', 'age_decade_2' = 'age_decade_last', 'i_wave')) %>% 
  mutate(is_signif.x = (lower_prop.x > 0.5 | upper_prop.x < 0.5),
         is_signif.y = (lower_prop.y > 0.5 | upper_prop.y < 0.5),
         is_signif = is_signif.x & is_signif.y) %>%
  mutate(name_wave = paste0('Wave ', i_wave + 3)) %>% 
  ggplot(aes(x = prop_1_before_2, y = prop)) +
  geom_linerange(aes(xmin = lower_prop.x, xmax = upper_prop.x, colour = is_signif), alpha = 0.2) +
  geom_linerange(aes(ymin = lower_prop.y, ymax = upper_prop.y, colour = is_signif), alpha = 0.2) +
  geom_point(aes(colour = is_signif), alpha = 0.8) +
  stat_cor(method = "spearman",  cor.coef.name = 'rho') +
  stat_cor(aes(colour = is_signif, alpha = is_signif), method = "spearman",  cor.coef.name = 'rho') +
  scale_colour_manual(breaks = c(T, F), values = c('firebrick', 'gray30')) +
  facet_wrap(. ~ name_wave, nrow = 1) +
  theme_bw() +
  coord_fixed() +
  scale_x_continuous(name = 'Proportion from pairs', limits = c(0., 1.),
                     breaks = c(0., 0.5, 1.)) +
  scale_y_continuous(name = 'Proportion from clusters', limits = c(0., 1.),
                     breaks = c(0., 0.5, 1.)) +
  theme(strip.text = element_text(colour = 'white'),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.background = element_rect(fill = 'gray22'),
        legend.position = 'none',
        panel.spacing = unit(1, "lines"))

## Make panel with all the results
panel_sensitivity <- ggarrange(ggarrange(plt_first_EW_among_clusters + ggtitle('Between Eastern /\nWestern WA'),
                    plt_comp_prop_regions + ggtitle('Between regions'), labels = 'AUTO'),
          plt_comp_prop_regions_by_wave + ggtitle('Between regions'),
          plt_comp_prop_ages_by_wave + ggtitle('Between age groups'),
          nrow = 3, ncol = 1,
          labels = c('', 'C', 'D'))

# pdf('../../figures/supplementary_figures/sensitivity_analysis_direction.pdf',
#     height = 10, width = 10)
# plot(panel_sensitivity)
# dev.off()
# png('../../figures/supplementary_figures/sensitivity_analysis_direction.png',
#     height = 10, width = 10, res = 350, units = 'in')
# plot(panel_sensitivity)
# dev.off()
  
######### Look at total number of clusters between age groups across waves
df_clusters_ages %>% 
  filter(i_wave == 1) %>% 
  ggplot(aes(x = age_decade_first, y = age_decade_last, fill = n_tot_clusters)) +
  geom_tile()


df_clusters_ages %>% 
  filter(age_decade_first < age_decade_last) %>% ungroup() %>% 
  group_by(i_wave) %>% 
  summarise(n = n(), n_below_10 = sum(n_tot_clusters <= 10))
