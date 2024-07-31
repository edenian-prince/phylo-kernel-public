library(mgcv)
library(tidyverse)
library(vegan)
library(broom)
library(ggrepel)
library(ggpubr)
source('utils_comp_RR.R')

## Load relative risk of observing identical sequences between counties
df_RR_counties <- readRDS('../results/RR_county/df_RR_county_0_mut_away.rds') %>% rename(RR_seq = RR) %>% ungroup()
df_RR_uncertainty_counties <- readRDS('../results/RR_county/df_RR_uncertainty_county_0_mut_away.rds')
df_RR_regions <- readRDS('../results/RR_region/df_RR_region_0_mut_away.rds') %>% rename(RR_seq = RR) %>% ungroup()

## Load relative risk of movements between counties
df_RR_mobility_commute <- readRDS('../results/RR_mobility/RR_workflow_county_WA.rds') %>% rename(RR_workflow = RR)
df_RR_mobility_commute_region <- readRDS('../results/RR_mobility/RR_workflow_region_WA.rds') %>% rename(RR_workflow = RR)
df_RR_mobility_mobile_phone <- readRDS('../results/RR_mobility/RR_mobile_phone_county_WA.rds') %>% rename(RR_mobile_phone = RR)
df_RR_mobility_mobile_phone_region <- readRDS('../results/RR_mobility/RR_mobile_phone_region_WA.rds') %>% rename(RR_mobile_phone = RR)
df_distance <- readRDS('../data/maps/df_dist_county.rds')
df_distance_region <- readRDS('../data/maps/df_dist_region.rds')

df_RR_for_comparison_counties <- df_RR_counties %>% select(group_1, group_2, RR_seq) %>% 
  left_join(df_RR_mobility_commute %>% select(county_1, county_2, RR_workflow),
            by = c('group_1' = 'county_1', 'group_2' = 'county_2')) %>% 
  left_join(df_RR_mobility_mobile_phone %>% select(county_1, county_2, RR_mobile_phone),
            by = c('group_1' = 'county_1', 'group_2' = 'county_2')) %>% 
  left_join(df_distance, by = c('group_1' = 'county_1', 'group_2' = 'county_2')) %>% 
  mutate(log_RR_seq = log(RR_seq), log_RR_workflow = log(RR_workflow), log_RR_mobile_phone = log(RR_mobile_phone)) %>% 
  filter(group_1 >= group_2)

df_RR_for_comparison_regions <- df_RR_regions %>% select(group_1, group_2, RR_seq) %>% 
  left_join(df_RR_mobility_commute_region %>% select(region_1, region_2, RR_workflow),
            by = c('group_1' = 'region_1', 'group_2' = 'region_2')) %>% 
  left_join(df_RR_mobility_mobile_phone_region %>% select(region_1, region_2, RR_mobile_phone),
            by = c('group_1' = 'region_1', 'group_2' = 'region_2')) %>% 
  left_join(df_distance_region, by = c('group_1' = 'region_1', 'group_2' = 'region_2')) %>% 
  mutate(log_RR_seq = log(RR_seq), log_RR_workflow = log(RR_workflow), log_RR_mobile_phone = log(RR_mobile_phone)) %>% 
  filter(group_1 >= group_2)

## Load adjacency between counties
df_adj_county <- readRDS('../data/maps/df_adj_county.rds') %>% 
  as_tibble() %>% 
  rename(group_1 = county_1, group_2 = county_2)

## Run GAM and get percentage of variance explained
gam_seq_workflow_counties <- run_gam(df_with_RR_for_gam = df_RR_for_comparison_counties %>% filter(RR_workflow > 0., RR_seq > 0.), 
                                     predictor_name = 'log_RR_workflow', response_name = 'log_RR_seq', 
                                     k_gam = 5,
                                     transform_func_var_response = 'exp',
                                     transform_func_var_predictor = 'exp',
                                     df_adj = df_adj_county, 
                                     bool_get_outliers = T, df_with_pairs = df_RR_counties, 
                                     min_n_pairs = 100, threshold_residuals_outliers = 3.,
                                     bool_get_plot_outliers = T)
summary(gam_seq_workflow_counties$mod)$r.sq
summary(gam_seq_workflow_counties$mod)$s.pv
gam_seq_workflow_counties$df_outliers_fit
gam_seq_workflow_counties$plt_fit
gam_seq_workflow_counties$plt_outliers

plt_fit_workflow_counties_with_R2 <- gam_seq_workflow_counties$plt_fit +
  annotate('text', x = 1e-1, y = Inf,  hjust = 1, vjust = 1.,
           label = paste0('R^{2}~`=`~', 
                          round(summary(gam_seq_workflow_counties$mod)$r.sq, 2)), 
           parse = T, size = 5)

gam_seq_mobile_phone_counties <- run_gam(df_RR_for_comparison_counties %>% filter(RR_mobile_phone > 0., RR_seq > 0.), 
                                         'log_RR_mobile_phone', 'log_RR_seq',
                                         k_gam = 5, transform_func_var_response = 'exp',
                                         transform_func_var_predictor = 'exp',
                                         df_adj = df_adj_county, 
                                         bool_get_outliers = T, df_with_pairs = df_RR_counties, 
                                         min_n_pairs = 100, threshold_residuals_outliers = 3.,
                                         bool_get_plot_outliers = T)
summary(gam_seq_mobile_phone_counties$mod)$r.sq
summary(gam_seq_mobile_phone_counties$mod)$s.pv
gam_seq_mobile_phone_counties$df_outliers_fit %>% View()
gam_seq_mobile_phone_counties$plt_fit
gam_seq_mobile_phone_counties$plt_outliers

plt_fit_mobile_phone_counties_with_R2 <- gam_seq_mobile_phone_counties$plt_fit +
  annotate('text', x = 1., y = Inf,  hjust = 1, vjust = 1.,
           label = paste0('R^{2}~`=`~', 
                          round(summary(gam_seq_mobile_phone_counties$mod)$r.sq, 2)), 
           parse = T, size = 5)

identity <- function(x){return(x)}
gam_seq_distance_counties <- run_gam(df_RR_for_comparison_counties %>% filter(RR_seq > 0.), 
                                     'distance_km', 'log_RR_seq',
                                     k_gam = 5,
                                     transform_func_var_response = 'exp',
                                     transform_func_var_predictor = 'identity',
                                     df_adj = df_adj_county, 
                                     bool_get_outliers = T, df_with_pairs = df_RR_counties, 
                                     min_n_pairs = 100, threshold_residuals_outliers = 3.,
                                     bool_get_plot_outliers = T)
summary(gam_seq_distance_counties$mod)$r.sq
summary(gam_seq_distance_counties$mod)$s.pv
gam_seq_distance_counties$df_outliers_fit
gam_seq_distance_counties$plt_fit
gam_seq_distance_counties$plt_outliers

plt_fit_distance_counties_with_R2 <- gam_seq_distance_counties$plt_fit +
  scale_x_continuous(name = 'Distance (in km)') +
  annotate('text', x = 200., y = Inf,  hjust = 1, vjust = 1.,
           label = paste0('R^{2}~`=`~', 
                          round(summary(gam_seq_distance_counties$mod)$r.sq, 2)), 
           parse = T, size = 5)

gam_seq_workflow_regions <- run_gam(df_with_RR_for_gam = df_RR_for_comparison_regions %>% filter(RR_workflow > 0., RR_seq > 0.), 
                                    'log_RR_workflow', 'log_RR_seq',
                                    k_gam = 5, 
                                    transform_func_var_response = 'exp',
                                    transform_func_var_predictor = 'exp',
                                    df_adj = NULL, 
                                    bool_get_outliers = T, df_with_pairs = df_RR_regions, 
                                    min_n_pairs = 100, threshold_residuals_outliers = 3.,
                                    bool_get_plot_outliers = F)

summary(gam_seq_workflow_regions$mod)$r.sq
summary(gam_seq_workflow_regions$mod)$s.pv
gam_seq_workflow_regions$df_outliers_fit
gam_seq_workflow_regions$plt_fit

plt_fit_workflow_regions_with_R2 <- gam_seq_workflow_regions$plt_fit +
  annotate('text', x = 1e-1, y = Inf,  hjust = 1, vjust = 1.,
           label = paste0('R^{2}~`=`~', 
                          round(summary(gam_seq_workflow_regions$mod)$r.sq, 2)), 
           parse = T, size = 5)

gam_seq_mobile_phone_regions <- run_gam(df_RR_for_comparison_regions %>% filter(RR_mobile_phone > 0., RR_seq > 0.), 
                                        'log_RR_mobile_phone', 'log_RR_seq',
                                        k_gam = 5,
                                        transform_func_var_response = 'exp',
                                        transform_func_var_predictor = 'exp',
                                        df_adj = NULL, 
                                        bool_get_outliers = T, df_with_pairs = df_RR_regions, 
                                        min_n_pairs = 100, threshold_residuals_outliers = 3.,
                                        bool_get_plot_outliers = F)
summary(gam_seq_mobile_phone_regions$mod)$r.sq
summary(gam_seq_mobile_phone_regions$mod)$s.pv
gam_seq_mobile_phone_regions$df_outliers_fit
gam_seq_mobile_phone_regions$plt_fit

plt_fit_mobile_phone_regions_with_R2 <- gam_seq_mobile_phone_regions$plt_fit +
  annotate('text', x = 1e0, y = Inf,  hjust = 1, vjust = 1.,
           label = paste0('R^{2}~`=`~', 
                          round(summary(gam_seq_mobile_phone_regions$mod)$r.sq, 2)), 
           parse = T, size = 5)

gam_seq_distance_regions <- run_gam(df_RR_for_comparison_regions %>% filter(RR_seq > 0.), 
                                    'distance_km', 'log_RR_seq',
                                    k_gam = 5,
                                    transform_func_var_response = 'exp',
                                    transform_func_var_predictor = 'identity',
                                    df_adj = NULL, 
                                    bool_get_outliers = T, df_with_pairs = df_RR_regions, 
                                    min_n_pairs = 100, threshold_residuals_outliers = 3.,
                                    bool_get_plot_outliers = F)

summary(gam_seq_distance_regions$mod)$r.sq
summary(gam_seq_distance_regions$mod)$s.pv
gam_seq_distance_regions$df_outliers_fit
gam_seq_distance_regions$plt_fit

plt_fit_distance_regions_with_R2 <- gam_seq_distance_regions$plt_fit +
  scale_x_continuous(name = 'Distance (in km)') +
  annotate('text', x = 200, y = Inf,  hjust = 1, vjust = 1.,
           label = paste0('R^{2}~`=`~', 
                          round(summary(gam_seq_distance_regions$mod)$r.sq, 2)), 
           parse = T, size = 5)

pdf('../plots/figure_mobility/fit_gam_safegraph_counties.pdf',
    height = 3., width = 3.9)
plot(gam_seq_mobile_phone_counties$plt_fit +
       theme(legend.position = c(0.35, 0.8),
             legend.background = element_blank()) +
       guides(colour = guide_legend(override.aes = list(alpha = 1.))))
dev.off()
pdf('../plots/figure_mobility/outliers_gam_safegraph_counties.pdf',
    height = 3., width = 3.9)
plot(gam_seq_mobile_phone_counties$plt_outliers + theme(legend.position = 'none'))
dev.off()

pdf('../plots/figure_mobility/panel_gam_safegraph_counties.pdf',
    height = 3.5, width = 8.)
plot(ggarrange(gam_seq_mobile_phone_counties$plt_fit +
                guides(colour = guide_legend(override.aes = list(alpha = 1.))), 
              gam_seq_mobile_phone_counties$plt_outliers , common.legend = T, legend = 'top'))
dev.off()

pdf('../plots/figure_mobility/panel_gam_safegraph_counties_with_R2.pdf',
    height = 3.5, width = 8.)
plot(ggarrange(plt_fit_mobile_phone_counties_with_R2 +
                 guides(colour = guide_legend(override.aes = list(alpha = 1.))), 
               gam_seq_mobile_phone_counties$plt_outliers , common.legend = T, legend = 'top'))
dev.off()


panel_fit_counties <- ggarrange(plt_fit_mobile_phone_counties_with_R2 + 
                                  theme(legend.background = element_blank(), legend.position = c(0.3, 0.8)) +
                                  guides(colour = guide_legend(override.aes = list(alpha = 1.)),
                                         alpha = 'none') +
                                  ggtitle('Mobile phone mobility data'),
                                gam_seq_mobile_phone_counties$plt_outliers +
                                  theme(legend.position = 'none') +
                                  ggtitle('Mobile phone mobility data'),
                                plt_fit_workflow_counties_with_R2 +
                                theme(legend.position = 'none') +
                                  ggtitle('Commuting mobility data'),
                                gam_seq_workflow_counties$plt_outliers  + 
                                  theme(legend.background = element_blank(), legend.position = c(0.7, 0.92)) +
                                  guides(colour = guide_legend(override.aes = list(alpha = 1.)),
                                         alpha = 'none') +
                                  ggtitle('Commuting mobility data'),
                                plt_fit_distance_counties_with_R2 + 
                                  theme(legend.position = 'none') +
                                  ggtitle('Distance'),
                                gam_seq_distance_counties$plt_outliers +
                                  theme(legend.background = element_blank(), legend.position = c(0.7, 0.95)) +
                                  guides(colour = guide_legend(override.aes = list(alpha = 1.)),
                                         alpha = 'none') +
                                  ggtitle('Distance'),
                                labels = 'AUTO', nrow = 3, ncol = 2)

plot(panel_fit_counties)

pdf('../plots/figure_mobility/summary_gam_safegraph_workflow_counties.pdf',
    height = 10.5, width = 9.0)
plot(panel_fit_counties)
dev.off()

plt_residuals_regions <- gam_seq_workflow_regions$df_pred %>% mutate(type = 'Commuting') %>% 
  bind_rows(gam_seq_mobile_phone_regions$df_pred %>% mutate(type = 'Mobile phone')) %>% 
  bind_rows(gam_seq_distance_regions$df_pred %>% mutate(type = 'Distance')) %>% 
  mutate(type = factor(type, levels = c('Mobile phone', 'Commuting', 'Distance'))) %>% 
  ggplot(aes(x = type, y = scaled_pearson_resid, colour = (group_1 == group_2))) +
  geom_jitter(width = 0.2) +
  scale_x_discrete(name = '') +
  scale_colour_manual(name = '', breaks = c(T, F), values = c('firebrick2', 'black'), 
                      labels = c('Within region', 'Between regions')) +
  scale_y_continuous(name = 'Scaled Pearson residuals', breaks = seq(-4, 4, 1),
                     limits = c(-4, 4)) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.background = element_blank(),
        legend.position = c(0.3, 0.95))

panel_fit_regions <- ggarrange(plt_fit_mobile_phone_regions_with_R2 + ggtitle('Mobile phone mobility data'), 
                               plt_fit_workflow_regions_with_R2 + ggtitle('Commuting mobility data'), 
                               plt_fit_distance_regions_with_R2 + ggtitle('Distance'), 
                               plt_residuals_regions, labels = 'AUTO')

pdf('../plots/figure_mobility/summary_gam_safegraph_workflow_regions.pdf',
    height = 6.5, width = 7.5)
plot(panel_fit_regions)
dev.off()


##
df_RR_counties %>% filter(group_1 == 'Franklin County', group_2 == 'Mason County')
df_RR_counties %>% filter(group_1 == 'Walla Walla County', group_2 == 'Mason County')
df_RR_counties %>% filter(group_1 == 'Pierce County', group_2 == 'Mason County')
df_RR_uncertainty_counties %>% filter(group_1 == 'Franklin County', group_2 == 'Mason County')
df_RR_uncertainty_counties %>% filter(group_1 == 'Walla Walla County', group_2 == 'Mason County')
df_RR_uncertainty_counties %>% filter(group_1 == 'Pierce County', group_2 == 'Mason County')


gam_seq_workflow_counties$df_outliers_fit %>% 
  mutate(type = 'Workflow') %>% 
  bind_rows(gam_seq_mobile_phone_counties$df_outliers_fit %>% 
              mutate(type = 'Mobile phone')) %>% 
  bind_rows(gam_seq_distance_counties$df_outliers_fit %>% 
              mutate(type = 'Ditance')) %>% View()
