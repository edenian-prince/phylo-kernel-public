library(corrplot)
library(tidyverse)


df_RR_mobility_mobile_phone_region_by_period <- Reduce('bind_rows', lapply(1:4, FUN = function(i_period){
  readRDS(paste0('../results/RR_mobility/RR_mobile_phone_region_WA_period_', i_period, '.rds')) %>% 
    rename(RR_mobile_phone = RR) %>% ungroup() %>% 
    mutate(i_period = i_period)
}))

df_RR_mobility_mobile_phone_county_by_period <- Reduce('bind_rows', lapply(1:4, FUN = function(i_period){
  readRDS(paste0('../results/RR_mobility/RR_mobile_phone_county_WA_period_', i_period, '.rds')) %>% 
    rename(RR_mobile_phone = RR) %>% ungroup() %>% 
    mutate(i_period = i_period)
}))


df_RR_mobility_mobile_phone_region_by_period %>% 
  mutate(prop = n_tot_visits / n_tot_visits_1_x) %>% 
  select(region_1, region_2, prop, i_period) %>% 
  ggplot(aes(x = ))

df_RR_wide_n_visits <- df_RR_mobility_mobile_phone_county_by_period %>% 
  select(county_1, county_2, n_tot_visits, i_period) %>% 
  pivot_wider(names_from = 'i_period', values_from = 'n_tot_visits', 
              names_prefix = 'n_tot_visits_')

cor_mat_n_visits <- cor(df_RR_wide_n_visits %>% 
                          select(-county_1, - county_2) %>% 
                          rename(`Wave 4` = n_tot_visits_1,
                                 `Wave 5` = n_tot_visits_2,
                                 `Wave 6` = n_tot_visits_3,
                                 `Wave 7` = n_tot_visits_4),
                        method = 'spearman')


corrplot(cor_mat_n_visits, type = 'upper', method = 'number')

list_all_plots <- lapply(2:4, FUN = function(i_period_1){
  lapply(1:3, FUN = function(i_period_2){
    name_var_1 <- paste0('n_tot_visits_', i_period_1)
    name_var_2 <- paste0('n_tot_visits_', i_period_2)
    name_lab_1 <- paste0('Wave ', 4:7)[i_period_1]
    name_lab_2 <- paste0('Wave ', 4:7)[i_period_2]
    
    if(i_period_1 > i_period_2){
      curr_df <-  df_RR_wide_n_visits %>% 
        rename(x_var = name_var_1, y_var = name_var_2)
      cor_spearman <- curr_df %>% summarise(cor = cor(x_var, y_var, method = 'spearman')) %>% unlist() %>% round(digits = 2) %>% as.numeric()
      
      min_val <- min(c(curr_df$x_var[curr_df$x_var > 0.], curr_df$y_var[curr_df$y_var > 0.])) * 0.01
      
      plt <- curr_df %>% 
        mutate(x_var_crop = ifelse(x_var == 0., min_val, x_var),
               y_var_crop = ifelse(y_var == 0., min_val, y_var)) %>% 
        ggplot() +
        geom_point(aes(x = x_var_crop, y = y_var_crop), alpha = 0.1) +
        geom_text(data = tibble(y_var = 1, x_var = 1), 
                  aes(x = 0, y = Inf), hjust = 0., vjust = 1.,
                  label = paste0('Spearman r = ', cor_spearman)) +
        scale_x_continuous(trans = 'log', name = name_lab_1, 
                           breaks = c(min_val, 1e2, 1e4, 1e6, 1e8),
                           labels = c(0, expression(10^2), expression(10^4),expression(10^6),expression(10^8)),
                           expand = expansion(add = c(1., 1.))) +
        scale_y_continuous(trans = 'log', name = name_lab_2,
                           breaks = c(min_val, 1e2, 1e4, 1e6, 1e8),
                           labels = c(0, expression(10^2), expression(10^4),expression(10^6),expression(10^8)),
                           expand = expansion(add = c(1., 1.))) +
        facet_grid((y_var == 0.) ~ (x_var != 0.), space = 'free', scales = 'free') +
        theme_classic() +
        theme(strip.text = element_blank(),
              strip.background = element_blank(),
              axis.text = element_text(size = 12))
        
      plt
    } else{
      tibble() %>% 
        ggplot() +
        theme_void()
    }
  })
})

panel_comp_visits_across_waves <- ggarrange(ggarrange(plotlist = list_all_plots[[1]], nrow = 1, ncol = 3),
          ggarrange(plotlist = list_all_plots[[2]], nrow = 1, ncol = 3),
          ggarrange(plotlist = list_all_plots[[3]], nrow = 1, ncol = 3),
          ncol = 1, nrow = 3
)

pdf('../plots/figure_mobility/correlation_safegraph_across_waves.pdf', height = 7, width = 8)
plot(panel_comp_visits_across_waves)
dev.off()
