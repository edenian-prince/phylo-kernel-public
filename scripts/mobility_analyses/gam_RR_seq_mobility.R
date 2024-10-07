## This script implements GAM to evaluate the percentage of the variance in the genetic data 
## explained by genetic data. We also identify (and plot) outliers in the relationship between 
## genetic and mobility from the GAM.

library(mgcv)
library(tidyverse)
library(vegan)
library(broom)
library(ggrepel)
library(ggpubr)
source('../utils_comp_RR.R')

## Load relative risk of observing identical sequences between counties
df_RR_counties <- read_csv('../../results/RR_county/df_RR_county_0_mut_away.csv') %>% rename(RR_seq = RR) 
df_RR_uncertainty_counties <- readRDS('../../results/RR_county/df_RR_uncertainty_county_0_mut_away.rds')
df_RR_regions <- read_csv('../../results/RR_region/df_RR_region_0_mut_away.csv') %>% rename(RR_seq = RR)
df_RR_uncertainty_regions <- readRDS('../../results/RR_region/df_RR_uncertainty_region_0_mut_away.rds')

## Load relative risk of movements between counties
df_RR_mobility_commute <- readRDS('../../results/RR_mobility/RR_workflow_county_WA.rds') %>% rename(RR_workflow = RR)
df_RR_mobility_commute_region <- readRDS('../../results/RR_mobility/RR_workflow_region_WA.rds') %>% rename(RR_workflow = RR)
df_RR_mobility_mobile_phone <- readRDS('../../results/RR_mobility/RR_mobile_phone_county_WA.rds') %>% rename(RR_mobile_phone = RR)
df_RR_mobility_mobile_phone_region <- readRDS('../../results/RR_mobility/RR_mobile_phone_region_WA.rds') %>% rename(RR_mobile_phone = RR)
df_distance <- readRDS('../../data/maps/df_dist_county.rds')
df_distance_region <- readRDS('../../data/maps/df_dist_region.rds')

## Join RR of identical sequences with RR of movement at the county level
df_RR_for_comparison_counties <- df_RR_counties %>% select(group_1, group_2, RR_seq, n_pairs) %>% 
  left_join(df_RR_mobility_commute %>% select(county_1, county_2, RR_workflow),
            by = c('group_1' = 'county_1', 'group_2' = 'county_2')) %>% 
  left_join(df_RR_mobility_mobile_phone %>% select(county_1, county_2, RR_mobile_phone),
            by = c('group_1' = 'county_1', 'group_2' = 'county_2')) %>% 
  left_join(df_distance, by = c('group_1' = 'county_1', 'group_2' = 'county_2')) %>% 
  mutate(log_RR_seq = log(RR_seq), log_RR_workflow = log(RR_workflow), log_RR_mobile_phone = log(RR_mobile_phone)) %>% 
  filter(group_1 >= group_2)

## Join RR of identical sequences with RR of movement at the region level
df_RR_for_comparison_regions <- df_RR_regions %>% select(group_1, group_2, RR_seq, n_pairs) %>% 
  left_join(df_RR_mobility_commute_region %>% select(region_1, region_2, RR_workflow),
            by = c('group_1' = 'region_1', 'group_2' = 'region_2')) %>% 
  left_join(df_RR_mobility_mobile_phone_region %>% select(region_1, region_2, RR_mobile_phone),
            by = c('group_1' = 'region_1', 'group_2' = 'region_2')) %>% 
  left_join(df_distance_region, by = c('group_1' = 'region_1', 'group_2' = 'region_2')) %>% 
  mutate(log_RR_seq = log(RR_seq), log_RR_workflow = log(RR_workflow), log_RR_mobile_phone = log(RR_mobile_phone)) %>% 
  filter(group_1 >= group_2)

## Load adjacency between counties
df_adj_county <- readRDS('../../data/maps/df_adj_county.rds') %>% as_tibble() %>% 
  rename(group_1 = county_1, group_2 = county_2)

###########################################################################
# RELATIONSHIP BETWEEN RR OF ID SEQ AND RR OF COMMUTE AT THE COUNTY LEVEL # 
###########################################################################
## Run GAM and get percentage of variance explained
gam_seq_workflow_counties <- run_gam(df_with_RR_for_gam = df_RR_for_comparison_counties %>% filter(RR_workflow > 0., RR_seq > 0.),
                                     predictor_name = 'log_RR_workflow', response_name = 'log_RR_seq', 
                                     k_gam = 5)

summary(gam_seq_workflow_counties$mod)$r.sq
summary(gam_seq_workflow_counties$mod)$s.pv

## Display the fit
plt_fit_gam_seq_workflow_counties <- plot_fit_gam_counties(res_gam = gam_seq_workflow_counties, 
                                                           transform_func_response = 'exp', transform_func_predictor = 'exp',
                                                           name_x_axis = expression(RR['mobility']),
                                                           breaks_x_axis = c(1e-3, 1e-2, 1e-1, 1., 1e1, 1e2, 1e3, 1e4),
                                                           labels_x_axis = c(expression(10^{-3}), expression(10^{-2}), expression(10^{-1}),
                                                                             expression(10^{0}), expression(10^{1}), expression(10^{2}), expression(10^{3}),
                                                                             expression(10^{4})),
                                                           trans_x_axis_plot = 'log',
                                                           name_y_axis = expression(RR['identical sequences']),
                                                           breaks_y_axis = c(1e-3, 1e-2, 1e-1, 1., 1e1, 1e2, 1e3, 1e4),
                                                           labels_y_axis = c(expression(10^{-3}), expression(10^{-2}), expression(10^{-1}),
                                                                             expression(10^{0}), expression(10^{1}), expression(10^{2}), expression(10^{3}),
                                                                             expression(10^{4})),
                                                           trans_y_axis_plot = 'log',
                                                           df_adj = df_adj_county)

plt_fit_workflow_counties_with_R2 <- plt_fit_gam_seq_workflow_counties +
  annotate('text', x = 1e-1, y = Inf,  hjust = 1, vjust = 1.,
           label = paste0('R^{2}~`=`~', 
                          round(summary(gam_seq_workflow_counties$mod)$r.sq, 2)), 
           parse = T, size = 5)

## Get outliers in the relationship between genetic and mobility data
df_outliers_workflow_mobility <- get_outliers_fit(res_gam = gam_seq_workflow_counties, min_n_pairs = 100, threshold_residuals_outliers = 3.)
df_outliers_workflow_mobility

## Display outliers
plt_outliers_workflow_mobility_counties <- plot_outliers_fit_counties(res_gam = gam_seq_workflow_counties, min_n_pairs = 100, threshold_residuals_outliers = 3.)

##############################################################################################
# RELATIONSHIP BETWEEN RR OF ID SEQ AND RR OF MOVEMENT FROM MOBILE PHONE AT THE COUNTY LEVEL # 
##############################################################################################
## Run GAM and get percentage of variance explained
gam_seq_mobile_phone_counties <- run_gam(df_with_RR_for_gam = df_RR_for_comparison_counties %>% filter(RR_mobile_phone > 0., RR_seq > 0.),
                                         predictor_name = 'log_RR_mobile_phone', response_name = 'log_RR_seq', 
                                         k_gam = 5)

summary(gam_seq_mobile_phone_counties$mod)$r.sq
summary(gam_seq_mobile_phone_counties$mod)$s.pv

## Display the fit
plt_fit_gam_seq_mobile_phone_counties <- plot_fit_gam_counties(res_gam = gam_seq_mobile_phone_counties, 
                                                               transform_func_response = 'exp', transform_func_predictor = 'exp',
                                                               name_x_axis = expression(RR['mobility']),
                                                               breaks_x_axis = c(1e-3, 1e-2, 1e-1, 1., 1e1, 1e2, 1e3, 1e4),
                                                               labels_x_axis = c(expression(10^{-3}), expression(10^{-2}), expression(10^{-1}),
                                                                                 expression(10^{0}), expression(10^{1}), expression(10^{2}), expression(10^{3}),
                                                                                 expression(10^{4})),
                                                               trans_x_axis_plot = 'log',
                                                               name_y_axis = expression(RR['identical sequences']),
                                                               breaks_y_axis = c(1e-3, 1e-2, 1e-1, 1., 1e1, 1e2, 1e3, 1e4),
                                                               labels_y_axis = c(expression(10^{-3}), expression(10^{-2}), expression(10^{-1}),
                                                                                 expression(10^{0}), expression(10^{1}), expression(10^{2}), expression(10^{3}),
                                                                                 expression(10^{4})),
                                                               trans_y_axis_plot = 'log',
                                                               df_adj = df_adj_county)

plt_fit_mobile_phone_counties_with_R2 <- plt_fit_gam_seq_mobile_phone_counties +
  annotate('text', x = 1., y = Inf,  hjust = 1, vjust = 1.,
           label = paste0('R^{2}~`=`~', 
                          round(summary(gam_seq_mobile_phone_counties$mod)$r.sq, 2)), 
           parse = T, size = 5)

## Get outliers in the relationship between genetic and mobility data
df_outliers_mobile_phone_mobility <- get_outliers_fit(res_gam = gam_seq_mobile_phone_counties, min_n_pairs = 100, threshold_residuals_outliers = 3.)
df_outliers_mobile_phone_mobility

## Display outliers
plt_outliers_mobile_phone_mobility_counties <- plot_outliers_fit_counties(res_gam = gam_seq_mobile_phone_counties,
                                                                          min_n_pairs = 100, threshold_residuals_outliers = 3.)


######################################################################
# RELATIONSHIP BETWEEN RR OF ID SEQ AND DISTANCE AT THE COUNTY LEVEL # 
######################################################################
gam_seq_distance_counties <- run_gam(df_with_RR_for_gam = df_RR_for_comparison_counties %>% filter(RR_seq > 0.),
                                     predictor_name = 'distance_km', response_name = 'log_RR_seq', 
                                     k_gam = 5)

summary(gam_seq_distance_counties$mod)$r.sq
summary(gam_seq_distance_counties$mod)$s.pv

## Display the fit
plt_fit_gam_seq_distance_counties <- plot_fit_gam_counties(res_gam = gam_seq_distance_counties, 
                                                           transform_func_response = 'exp', transform_func_predictor = 'identity',
                                                           name_x_axis = 'Distance (in km)',
                                                           breaks_x_axis = seq(0., 600., 100.),
                                                           labels_x_axis = seq(0., 600., 100.),
                                                           trans_x_axis_plot = 'identity',
                                                           name_y_axis = expression(RR['identical sequences']),
                                                           breaks_y_axis = c(1e-3, 1e-2, 1e-1, 1., 1e1, 1e2, 1e3, 1e4),
                                                           labels_y_axis = c(expression(10^{-3}), expression(10^{-2}), expression(10^{-1}),
                                                                             expression(10^{0}), expression(10^{1}), expression(10^{2}), expression(10^{3}),
                                                                             expression(10^{4})),
                                                           trans_y_axis_plot = 'log',
                                                           df_adj = df_adj_county)

plt_fit_distance_counties_with_R2 <- plt_fit_gam_seq_distance_counties +
  annotate('text', x = 200, y = Inf,  hjust = 1, vjust = 1.,
           label = paste0('R^{2}~`=`~', 
                          round(summary(gam_seq_distance_counties$mod)$r.sq, 2)), 
           parse = T, size = 5)

## Get outliers in the relationship between genetic and mobility data
df_outliers_distance_mobility <- get_outliers_fit(res_gam = gam_seq_distance_counties, min_n_pairs = 100, threshold_residuals_outliers = 3.)
df_outliers_distance_mobility

## Display outliers
plt_outliers_distance_counties <- plot_outliers_fit_counties(res_gam = gam_seq_distance_counties,
                                                             min_n_pairs = 100, threshold_residuals_outliers = 3.)

###########################################################################
# RELATIONSHIP BETWEEN RR OF ID SEQ AND RR OF COMMUTE AT THE REGION LEVEL # 
###########################################################################
## Run GAM and get percentage of variance explained
gam_seq_workflow_regions <- run_gam(df_with_RR_for_gam = df_RR_for_comparison_regions %>% filter(RR_workflow > 0., RR_seq > 0.),
                                    predictor_name = 'log_RR_workflow', response_name = 'log_RR_seq', 
                                    k_gam = 5)

summary(gam_seq_workflow_regions$mod)$r.sq
summary(gam_seq_workflow_regions$mod)$s.pv

## Display the fit
plt_fit_workflow_regions <- plot_fit_gam_regions(res_gam = gam_seq_workflow_regions, 
                                                 transform_func_response = 'exp', transform_func_predictor = 'exp',
                                                 name_x_axis = expression(RR['mobility']),
                                                 breaks_x_axis = c(1e-3, 1e-2, 1e-1, 1., 1e1, 1e2, 1e3, 1e4),
                                                 labels_x_axis = c(expression(10^{-3}), expression(10^{-2}), expression(10^{-1}),
                                                                   expression(10^{0}), expression(10^{1}), expression(10^{2}), expression(10^{3}),
                                                                   expression(10^{4})),
                                                 trans_x_axis_plot = 'log',
                                                 name_y_axis = expression(RR['identical sequences']),
                                                 breaks_y_axis = c(1, 2, 5),
                                                 labels_y_axis =c(1, 2, 5),
                                                 trans_y_axis_plot = 'log')

plt_fit_workflow_regions_with_R2 <- plt_fit_workflow_regions +
  annotate('text', x = 1e-1, y = Inf,  hjust = 1, vjust = 1.,
           label = paste0('R^{2}~`=`~', 
                          round(summary(gam_seq_workflow_regions$mod)$r.sq, 2)), 
           parse = T, size = 5)

##############################################################################################
# RELATIONSHIP BETWEEN RR OF ID SEQ AND RR OF MOVEMENT FROM MOBILE PHONE AT THE REGION LEVEL # 
##############################################################################################
## Run GAM and get percentage of variance explained
gam_seq_mobile_phone_regions <- run_gam(df_with_RR_for_gam = df_RR_for_comparison_regions %>% filter(RR_mobile_phone > 0., RR_seq > 0.),
                                        predictor_name = 'log_RR_mobile_phone', response_name = 'log_RR_seq', 
                                        k_gam = 5)

summary(gam_seq_mobile_phone_regions$mod)$r.sq
summary(gam_seq_mobile_phone_regions$mod)$s.pv

## Display the fit
plt_fit_mobile_phone_regions <- plot_fit_gam_regions(res_gam = gam_seq_mobile_phone_regions, 
                                                     transform_func_response = 'exp', transform_func_predictor = 'exp',
                                                     name_x_axis = expression(RR['mobility']),
                                                     breaks_x_axis = c(1e-3, 1e-2, 1e-1, 1., 1e1, 1e2, 1e3, 1e4),
                                                     labels_x_axis = c(expression(10^{-3}), expression(10^{-2}), expression(10^{-1}),
                                                                       expression(10^{0}), expression(10^{1}), expression(10^{2}), expression(10^{3}),
                                                                       expression(10^{4})),
                                                     trans_x_axis_plot = 'log',
                                                     name_y_axis = expression(RR['identical sequences']),
                                                     breaks_y_axis = c(1, 2, 5),
                                                     labels_y_axis =c(1, 2, 5),
                                                     trans_y_axis_plot = 'log')

plt_fit_mobile_phone_regions_with_R2 <- plt_fit_mobile_phone_regions +
  annotate('text', x = 1, y = Inf,  hjust = 1, vjust = 1.,
           label = paste0('R^{2}~`=`~', 
                          round(summary(gam_seq_workflow_regions$mod)$r.sq, 2)), 
           parse = T, size = 5)

######################################################################
# RELATIONSHIP BETWEEN RR OF ID SEQ AND DISTANCE AT THE REGION LEVEL # 
######################################################################
## Run GAM and get percentage of variance explained
gam_seq_distance_regions <- run_gam(df_with_RR_for_gam = df_RR_for_comparison_regions %>% filter(RR_seq > 0.),
                                    predictor_name = 'distance_km', response_name = 'log_RR_seq', 
                                    k_gam = 5)

summary(gam_seq_distance_regions$mod)$r.sq
summary(gam_seq_distance_regions$mod)$s.pv

## Display the fit
plt_fit_distance_regions <- plot_fit_gam_regions(res_gam = gam_seq_distance_regions, 
                                                 transform_func_response = 'exp', transform_func_predictor = 'identity',
                                                 name_x_axis = 'Distance (in km)',
                                                 breaks_x_axis = seq(0., 600., 100.),
                                                 labels_x_axis = seq(0., 600., 100.),
                                                 trans_x_axis_plot = 'identity',
                                                 name_y_axis = expression(RR['identical sequences']),
                                                 breaks_y_axis = c(1, 2, 5),
                                                 labels_y_axis =c(1, 2, 5),
                                                 trans_y_axis_plot = 'log')

plt_fit_distance_regions_with_R2 <- plt_fit_distance_regions +
  annotate('text', x = 200, y = Inf,  hjust = 1, vjust = 1.,
           label = paste0('R^{2}~`=`~', 
                          round(summary(gam_seq_distance_regions$mod)$r.sq, 2)), 
           parse = T, size = 5)

#################################################################
# SCATTERPLOT OF THE RESIDUALS OF THE GAM AT THE REGIONAL LEVEL #
#################################################################
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


##################
## SAVE FIGURES ##
##################

## GAM between RR of id seq and RR of movement from mobile phone data (Figure 3A) 

# pdf('../plots/figure_mobility/fit_gam_safegraph_counties.pdf',
#     height = 3., width = 3.9)
plot(plt_fit_mobile_phone_counties_with_R2 +
       theme(legend.position = c(0.35, 0.8),
             legend.background = element_blank()) +
       guides(colour = guide_legend(override.aes = list(alpha = 1.))))
#dev.off()

## Outliers from GAM between RR of id seq and RR of movement from mobile phone data (Figure 3B) 
#pdf('../plots/figure_mobility/outliers_gam_safegraph_counties.pdf',
#    height = 3., width = 3.9)
plot(plt_outliers_mobile_phone_mobility_counties + theme(legend.position = 'none'))
#dev.off()

## Combination of Figure 3A and 3B (with the same legend)
# pdf('../plots/figure_mobility/panel_gam_safegraph_counties_with_R2.pdf',
#     height = 3.5, width = 8.)
plot(ggarrange(plt_fit_mobile_phone_counties_with_R2 +
                 guides(colour = guide_legend(override.aes = list(alpha = 1.))), 
               plt_outliers_mobile_phone_mobility_counties, common.legend = T, legend = 'top'))
#dev.off()

## Figure with the results of the fit of the RR of id sequences to the 3 mobility indicators at the county level (Figure S10)
panel_fit_counties <- ggarrange(plt_fit_mobile_phone_counties_with_R2 + 
                                  theme(legend.background = element_blank(), legend.position = c(0.3, 0.8)) +
                                  guides(colour = guide_legend(override.aes = list(alpha = 1.)),
                                         alpha = 'none') +
                                  ggtitle('Mobile phone mobility data'),
                                plt_outliers_mobile_phone_mobility_counties +
                                  theme(legend.position = 'none') +
                                  ggtitle('Mobile phone mobility data'),
                                plt_fit_workflow_counties_with_R2 +
                                theme(legend.position = 'none') +
                                  ggtitle('Commuting mobility data'),
                                plt_outliers_workflow_mobility_counties  + 
                                  theme(legend.background = element_blank(), legend.position = c(0.7, 0.92)) +
                                  guides(colour = guide_legend(override.aes = list(alpha = 1.)),
                                         alpha = 'none') +
                                  ggtitle('Commuting mobility data'),
                                plt_fit_distance_counties_with_R2 + 
                                  theme(legend.position = 'none') +
                                  ggtitle('Distance'),
                                plt_outliers_distance_mobility_counties +
                                  theme(legend.background = element_blank(), legend.position = c(0.7, 0.95)) +
                                  guides(colour = guide_legend(override.aes = list(alpha = 1.)),
                                         alpha = 'none') +
                                  ggtitle('Distance'),
                                labels = 'AUTO', nrow = 3, ncol = 2)

# pdf('../plots/figure_mobility/summary_gam_safegraph_workflow_counties.pdf',
#     height = 10.5, width = 9.0)
plot(panel_fit_counties)
# dev.off()

## Figure with the results of the fit of the RR of id sequences to the 3 mobility indicators at the regional level (Figure S12)
panel_fit_regions <- ggarrange(plt_fit_mobile_phone_regions_with_R2 + ggtitle('Mobile phone mobility data'), 
                               plt_fit_workflow_regions_with_R2 + ggtitle('Commuting mobility data'), 
                               plt_fit_distance_regions_with_R2 + ggtitle('Distance'), 
                               plt_residuals_regions, labels = 'AUTO')

# pdf('../plots/figure_mobility/summary_gam_safegraph_workflow_regions.pdf',
#     height = 6.5, width = 7.5)
plot(panel_fit_regions)
#dev.off()


## Look at a couple values of the RR of identical sequences between counties
df_RR_counties %>% filter(group_1 == 'Franklin County', group_2 == 'Mason County')
df_RR_counties %>% filter(group_1 == 'Walla Walla County', group_2 == 'Mason County')
df_RR_counties %>% filter(group_1 == 'Pierce County', group_2 == 'Mason County')
df_RR_uncertainty_counties %>% filter(group_1 == 'Franklin County', group_2 == 'Mason County')
df_RR_uncertainty_counties %>% filter(group_1 == 'Walla Walla County', group_2 == 'Mason County')
df_RR_uncertainty_counties %>% filter(group_1 == 'Pierce County', group_2 == 'Mason County')

## Look at outliers in the relationship between genetic and mobility data (for our 3 mobility indicators)
df_outliers_workflow_mobility %>% 
  mutate(type = 'Workflow') %>% 
  bind_rows(df_outliers_mobile_phone_mobility %>% 
              mutate(type = 'Mobile phone')) %>% 
  bind_rows(df_outliers_distance_mobility %>% 
              mutate(type = 'Distance'))
