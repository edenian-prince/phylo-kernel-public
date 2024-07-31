library(mgcv)
library(tidyverse)
library(vegan)
library(broom)
library(ggrepel)
library(ggpubr)
source('utils_comp_RR.R')

## Load relative risk of observing identical sequences between regions
vec_periods <- 1:4
df_RR_regions_by_period <- Reduce('bind_rows', lapply(vec_periods, FUN = function(i_period){
  readRDS(paste0('../results/RR_region_by_period/df_RR_region_0_mut_away_period_', i_period, '.rds')) %>% 
    rename(RR_seq = RR) %>% ungroup() %>% 
    mutate(i_period = i_period)
}))
df_uncertainty_RR_regions_by_period <- Reduce('bind_rows', lapply(vec_periods, FUN = function(i_period){
  readRDS(paste0('../results/RR_region_by_period/df_RR_uncertainty_region_0_mut_away_period_', i_period, '.rds')) %>% 
    ungroup()
}))

## Load relative risk of movements between regions
df_RR_mobility_mobile_phone_region_by_period <- Reduce('bind_rows', lapply(vec_periods, FUN = function(i_period){
  readRDS(paste0('../results/RR_mobility/RR_mobile_phone_region_WA_period_', i_period, '.rds')) %>% 
    rename(RR_mobile_phone = RR) %>% ungroup() %>% 
    mutate(i_period = i_period)
}))

df_RR_for_comparison_regions <- df_RR_regions_by_period %>% select(group_1, group_2, RR_seq, i_period) %>% 
  left_join(df_uncertainty_RR_regions_by_period) %>% 
  left_join(df_RR_mobility_mobile_phone_region_by_period %>% select(region_1, region_2, RR_mobile_phone, i_period),
            by = c('group_1' = 'region_1', 'group_2' = 'region_2', 'i_period')) %>% 
  mutate(log_RR_seq = log(RR_seq), log_RR_workflow = log(RR_workflow), log_RR_mobile_phone = log(RR_mobile_phone)) %>% 
  filter(group_1 >= group_2)

## Load adjacency between counties
df_adj_county <- readRDS('../data/maps/df_adj_county.rds') %>% 
  as_tibble() %>% 
  rename(group_1 = county_1, group_2 = county_2)



## Mobile phone
gam_seq_mobile_phone_regions_by_period <- lapply(vec_periods, FUN = function(curr_i_period){
  gam_seq_mobile_phone_regions <- run_gam(df_RR_for_comparison_regions %>% 
                                            filter(RR_mobile_phone > 0., RR_seq > 0., i_period == curr_i_period), 
                                          'log_RR_mobile_phone', 'log_RR_seq',
                                          k_gam = 5,
                                          transform_func_var_response = 'exp',
                                          transform_func_var_predictor = 'exp',
                                          df_adj = NULL, 
                                          bool_get_outliers = T, df_with_pairs = df_RR_regions_by_period, 
                                          min_n_pairs = 100, threshold_residuals_outliers = 3.,
                                          bool_get_plot_outliers = F,
                                          bool_add_uncertainty_RR = T,
                                          df_uncertainty_RR = df_uncertainty_RR_regions_by_period %>% filter(i_period == curr_i_period))
})

sapply(gam_seq_mobile_phone_regions_by_period, FUN = function(curr_gam){
  summary(curr_gam$mod)$r.sq
})
sapply(gam_seq_mobile_phone_regions_by_period, FUN = function(curr_gam){
  summary(curr_gam$mod)$s.pv
})
lapply(gam_seq_mobile_phone_regions_by_period, FUN = function(curr_gam){
  curr_gam$df_outliers_fit
})



panel_gam_mobile_phone_over_time <- ggarrange(plotlist = lapply(vec_periods, FUN = function(i_period){
  curr_gam <- gam_seq_mobile_phone_regions_by_period[[i_period]]
  curr_gam$plt_fit + 
    ggtitle(paste0('Wave ', 3 + i_period)) +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), 
                             "inches")) +
    annotate(x = 0, y = Inf, 
             geom = 'text', 
             label = paste0('R^{2}~`=`~', round(summary(curr_gam$mod)$r.sq, 2)),
             hjust = -.5, vjust = 1., parse = T)
}), nrow = 2, ncol = 2, 
labels = 'AUTO') 

# pdf('../plots/figure_mobility/panel_gam_safegraph_counties_over_time_with_R2.pdf',
#     height = 8, width = 8)
plot(panel_gam_mobile_phone_over_time)
#dev.off()
