get_mat_RR_from_df_RR <- function(df_RR, RR_name, name_group){
  
  mat_RR <- df_RR %>% 
    rename(RR = RR_name, group_1 = paste0(name_group, '_1'), group_2 = paste0(name_group, '_2')) %>%
    select(group_1, group_2, RR) %>% 
    arrange(group_1) %>%
    pivot_wider(id_cols = 1, values_from = 'RR', names_from = 'group_2') %>%
    select(-group_1) %>% as.matrix()
  
  rownames(mat_RR) <- colnames(mat_RR)
  
  return(mat_RR)
}

get_mantel_comparison_from_mat_RR <- function(mat_RR_1, mat_RR_2, n_permut = 1e5 - 1, corr_method = 'spearman'){
  mantel_test <- mantel(xdis = mat_RR_1, ydis = mat_RR_2, method = corr_method, permutation = n_permut)
  return(mantel_test)
}

run_gam_no_plot <- function(df_with_RR_for_gam = df_RR_for_comparison, 
                            predictor_name = 'log_RR_contact', response_name = 'log_RR_seq',
                            k_gam = -1, transform_func_var = 'exp'){
  
  df_for_gam <- df_with_RR_for_gam %>% 
    rename(RR_predictor = predictor_name, RR_response = response_name) %>% 
    select(group_1, group_2, RR_response, RR_predictor)
  
  gam_mod <- gam(data = df_for_gam, formula = RR_response ~ s(RR_predictor, k = k_gam))
  
  df_pred <- df_for_gam %>% 
    select(group_1, group_2, RR_predictor) %>% 
    mutate(RR_response_pred_central = augment(gam_mod)$.fitted,
           RR_response_pred_lower = augment(gam_mod)$.fitted - 1.96 * (augment(gam_mod)$.se.fit),
           RR_response_pred_upper = augment(gam_mod)$.fitted + 1.96 * (augment(gam_mod)$.se.fit),
           scaled_pearson_resid = residuals(gam_mod, type = 'scaled.pearson'))
  
  
  return(list(mod = gam_mod, 
              df_pred = df_pred))
}

run_gam <- function(df_with_RR_for_gam = df_RR_for_comparison_counties %>% filter(RR_mobile_phone > 0., RR_seq > 0.), 
                    predictor_name = 'log_RR_mobile_phone', response_name = 'log_RR_seq',
                    k_gam = -1,
                    transform_func_var_response = 'exp', transform_func_var_predictor = 'exp',
                    df_adj = df_adj_county, 
                    bool_get_outliers = T, df_with_pairs = NULL, min_n_pairs = 100, threshold_residuals_outliers = 3.,
                    bool_get_plot_outliers = T, bool_add_uncertainty_RR = F, df_uncertainty_RR = NULL){
  
  if(transform_func_var_predictor == 'exp'){
    breaks_x_axis <- c(1e-3, 1e-2, 1e-1, 1., 1e1, 1e2, 1e3, 1e4)
    labels_x_axis <- c(expression(10^{-3}), expression(10^{-2}), expression(10^{-1}),
                       expression(10^{0}), expression(10^{1}), expression(10^{2}), expression(10^{3}),
                       expression(10^{4}))
    trans_x_axis_plot <- 'log'
  } else{
    breaks_x_axis <- seq(0., 600, 100)
    labels_x_axis <-  seq(0., 600, 100)
    identity <- function(x){return(x)}
    trans_x_axis_plot <- 'identity'
  }
  
  df_for_gam <- df_with_RR_for_gam %>% 
    rename(RR_predictor = predictor_name, RR_response = response_name) %>% 
    select(group_1, group_2, RR_response, RR_predictor)
  
  gam_mod <- gam(data = df_for_gam, formula = RR_response ~ s(RR_predictor, k = k_gam))
  
  df_pred <- df_for_gam %>% 
    select(group_1, group_2, RR_predictor) %>% 
    mutate(RR_response_pred_central = augment(gam_mod)$.fitted,
           RR_response_pred_lower = augment(gam_mod)$.fitted - 1.96 * (augment(gam_mod)$.se.fit),
           RR_response_pred_upper = augment(gam_mod)$.fitted + 1.96 * (augment(gam_mod)$.se.fit),
           scaled_pearson_resid = residuals(gam_mod, type = 'scaled.pearson'))
  
  if(!is.null(df_adj)){
    plt_fit <- plot_fit_gam_RR_counties(df_for_gam, df_pred, df_adj, 
                                        transform_func_response = transform_func_var_response,
                                        transform_func_predictor = transform_func_var_predictor,
                                        trans_x_axis = trans_x_axis_plot,
                                        breaks_x_axis = breaks_x_axis,
                                        labels_x_axis = labels_x_axis)
  } else{
    if(bool_add_uncertainty_RR){
      df_pred <- df_pred %>% left_join(df_uncertainty_RR %>% select(group_1, group_2, lower_RR, upper_RR))
      plt_fit <- plot_fit_gam_RR_regions_with_uncertainty(df_for_gam, df_pred, 
                                                          transform_func_response = transform_func_var_response,
                                                          transform_func_predictor = transform_func_var_predictor,
                                                          trans_x_axis = trans_x_axis_plot,
                                                          breaks_x_axis = breaks_x_axis,
                                                          labels_x_axis = labels_x_axis)
    } else{
      plt_fit <- plot_fit_gam_RR_regions(df_for_gam, df_pred, 
                                         transform_func_response = transform_func_var_response,
                                         transform_func_predictor = transform_func_var_predictor,
                                         trans_x_axis = trans_x_axis_plot,
                                         breaks_x_axis = breaks_x_axis,
                                         labels_x_axis = labels_x_axis)
    }
  }
  if(bool_get_outliers){
    df_outliers_fit <- get_outliers_fit(df_pred, df_with_pairs, min_n_pairs, threshold_residuals_outliers)
  } else{
    df_outliers_fit <- NULL
  }
  if(bool_get_plot_outliers){
    plt_outliers <- plot_outliers_fit_counties(df_for_gam, df_with_pairs, df_pred, df_adj, min_n_pairs, threshold_residuals_outliers)
  }
  else{
    plt_outliers <- NULL
  }
  
  return(list(mod = gam_mod, 
              df_pred = df_pred, 
              plt_fit = plt_fit,
              df_outliers_fit = df_outliers_fit,
              plt_outliers = plt_outliers))
}

plot_fit_gam_RR_counties <- function(df_for_gam, df_pred, df_adj = df_adj_county, 
                                     transform_func_response = 'exp', transform_func_predictor = 'exp',
                                     trans_x_axis,
                                     breaks_x_axis = c(1e-3, 1e-2, 1e-1, 1., 1e1, 1e2, 1e3, 1e4),
                                     labels_x_axis = c(expression(10^{-3}), expression(10^{-2}), expression(10^{-1}),
                                                       expression(10^{0}), expression(10^{1}), expression(10^{2}), expression(10^{3}),
                                                       expression(10^{4}))){
  
  plt_fit_gam <- df_pred %>% 
    left_join(df_for_gam %>% select(-RR_predictor), by = c('group_1', 'group_2')) %>% 
    left_join(df_adj, by = c('group_1', 'group_2')) %>% 
    mutate(RR_predictor_transform = get(transform_func_predictor)(RR_predictor),
           RR_response_transform = get(transform_func_response)(RR_response),
           RR_response_pred_central_transform = get(transform_func_response)(RR_response_pred_central),
           RR_response_pred_lower_transform = get(transform_func_response)(RR_response_pred_lower),
           RR_response_pred_upper_transform = get(transform_func_response)(RR_response_pred_upper),
           pair_status = case_when(group_1 == group_2 ~ 'Within county',
                                   is_adjacent == T ~ 'Adjacent counties',
                                   T ~ 'Non adjacent counties')) %>% 
    ggplot(aes(x = RR_predictor_transform)) +
    geom_point(aes(y = RR_response_transform, colour = pair_status), alpha = 0.3) +
    geom_line(aes(y = RR_response_pred_central_transform), col = 'gray22') +
    geom_ribbon(aes(ymin = RR_response_pred_lower_transform, ymax = RR_response_pred_upper_transform),
                fill = 'gray22', alpha = 0.2) +
    scale_x_continuous(name = expression(RR["mobility"]), 
                       trans = trans_x_axis,
                       breaks = breaks_x_axis,
                       expand = expansion(mult = c(0.05, 0.08)),
                       labels = labels_x_axis) +
    scale_y_continuous(name = expression(RR["identical sequences"]), trans = 'log',
                       breaks = c(1e-1, 1., 1e1, 1e2, 1e3, 1e4, 1e5),
                       expand = expansion(mult = c(0.05, 0.05)),
                       labels = c(expression(10^{-1}),
                                  expression(10^{0}), expression(10^{1}), expression(10^{2}), expression(10^{3}),
                                  expression(10^{4}), expression(10^{5}))) +
    scale_colour_manual(name = '', values = c('darkgrey', 'aquamarine4', 'darkgoldenrod1'), 
                        breaks = c('Non adjacent counties', 'Adjacent counties', 'Within county')) +
    scale_fill_manual(name = '', values = c('darkgrey', 'aquamarine4', 'darkgoldenrod1'), 
                      breaks = c('Non adjacent counties', 'Adjacent counties', 'Within county')) +
    theme_classic() +
    theme(axis.title = element_text(size = 13),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          strip.background = element_blank(),
          strip.text = element_blank()
    ) +
    guides(fill = guide_legend(override.aes = list(stroke = NA, alpha = 1.))) 
  
  return(plt_fit_gam)
}
plot_fit_gam_RR_regions <- function(df_for_gam, df_pred, 
                                    transform_func_response, transform_func_predictor,
                                    trans_x_axis = 'log',
                                    breaks_x_axis = c(1e-3, 1e-2, 1e-1, 1., 1e1, 1e2, 1e3, 1e4),
                                    labels_x_axis = c(expression(10^{-3}), expression(10^{-2}), expression(10^{-1}),
                                                      expression(10^{0}), expression(10^{1}), expression(10^{2}), expression(10^{3}),
                                                      expression(10^{4}))){
  
  plt_fit_gam <- df_pred %>% 
    left_join(df_for_gam %>% select(-RR_predictor), by = c('group_1', 'group_2')) %>% 
    mutate(RR_predictor_transform = get(transform_func_predictor)(RR_predictor),
           RR_response_transform = get(transform_func_response)(RR_response),
           RR_response_pred_central_transform = get(transform_func_response)(RR_response_pred_central),
           RR_response_pred_lower_transform = get(transform_func_response)(RR_response_pred_lower),
           RR_response_pred_upper_transform = get(transform_func_response)(RR_response_pred_upper)) %>% 
    ggplot(aes(x = RR_predictor_transform)) +
    geom_point(aes(y = RR_response_transform), alpha = 0.3) +
    geom_line(aes(y = RR_response_pred_central_transform), col = 'gray22') +
    geom_ribbon(aes(ymin = RR_response_pred_lower_transform, ymax = RR_response_pred_upper_transform),
                fill = 'gray22', alpha = 0.2) +
    scale_x_continuous(name = expression(RR["mobility"]), trans = trans_x_axis,
                       breaks = breaks_x_axis,
                       expand = expansion(mult = c(0.05, 0.05)),
                       labels = labels_x_axis) +
    scale_y_continuous(name = expression(RR["identical sequences"]), trans = 'log',
                       breaks = c(0.1, 1., 2., 5., 10.),
                       expand = expansion(mult = c(0.05, 0.05))) +
    theme_classic() +
    theme(axis.title = element_text(size = 13),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          strip.background = element_blank(),
          strip.text = element_blank()
    ) +
    guides(fill = guide_legend(override.aes = list(stroke = NA, alpha = 1.)))
  
  return(plt_fit_gam)
}


plot_fit_gam_RR_regions_with_uncertainty <- function(df_for_gam, df_pred, 
                                                     transform_func_response, transform_func_predictor,
                                                     trans_x_axis = 'log',
                                                     breaks_x_axis = c(1e-3, 1e-2, 1e-1, 1., 1e1, 1e2, 1e3, 1e4),
                                                     labels_x_axis = c(expression(10^{-3}), expression(10^{-2}), expression(10^{-1}),
                                                                       expression(10^{0}), expression(10^{1}), expression(10^{2}), expression(10^{3}),
                                                                       expression(10^{4}))){
  
  plt_fit_gam <- df_pred %>% 
    left_join(df_for_gam %>% select(-RR_predictor), by = c('group_1', 'group_2')) %>% 
    mutate(RR_predictor_transform = get(transform_func_predictor)(RR_predictor),
           RR_response_transform = get(transform_func_response)(RR_response),
           RR_response_pred_central_transform = get(transform_func_response)(RR_response_pred_central),
           RR_response_pred_lower_transform = get(transform_func_response)(RR_response_pred_lower),
           RR_response_pred_upper_transform = get(transform_func_response)(RR_response_pred_upper)) %>% 
    ggplot(aes(x = RR_predictor_transform)) +
    geom_point(aes(y = RR_response_transform), alpha = 0.3) +
    geom_linerange(aes(ymin = lower_RR, ymax = upper_RR)) +
    geom_line(aes(y = RR_response_pred_central_transform), col = 'gray22') +
    geom_ribbon(aes(ymin = RR_response_pred_lower_transform, ymax = RR_response_pred_upper_transform),
                fill = 'gray22', alpha = 0.2) +
    scale_x_continuous(name = expression(RR["mobility"]), trans = trans_x_axis,
                       breaks = breaks_x_axis,
                       expand = expansion(mult = c(0.05, 0.05)),
                       labels = labels_x_axis) +
    scale_y_continuous(name = expression(RR["identical sequences"]), trans = 'log',
                       breaks = c(0.1, 0.2, 0.5, 1., 2., 5., 10., 20., 50, 100, 200),
                       expand = expansion(mult = c(0.05, 0.05))) +
    theme_classic() +
    theme(axis.title = element_text(size = 13),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          strip.background = element_blank(),
          strip.text = element_blank()
    ) +
    guides(fill = guide_legend(override.aes = list(stroke = NA, alpha = 1.)))
  
  return(plt_fit_gam)
}


get_outliers_fit <- function(df_pred, df_with_pairs, min_n_pairs = 100, threshold_residuals_outliers = 3.){
  
  df_pred %>% 
    left_join(df_with_pairs) %>% 
    filter(n_pairs >= min_n_pairs, 
           abs(scaled_pearson_resid) > threshold_residuals_outliers,
           group_1 >= group_2) %>% 
    return()
}

plot_outliers_fit_counties <- function(df_for_gam, df_with_pairs, df_pred, df_adj, min_n_pairs = 100, threshold_residuals_outliers = 3.){
  df_for_plot <- df_pred %>% 
    left_join(df_with_pairs %>% select(group_1, group_2, n_pairs), by = c('group_1', 'group_2')) %>% 
    left_join(df_for_gam %>% select(- RR_predictor), by = c('group_1', 'group_2')) %>% 
    left_join(df_adj, by = c('group_1', 'group_2')) %>% 
    mutate(is_outlier = abs(scaled_pearson_resid) > threshold_residuals_outliers,
           is_above_pair_threshold = (n_pairs >= min_n_pairs),
           pair_status = case_when(group_1 == group_2 ~ 'Within county',
                                   is_adjacent == T ~ 'Adjacent counties',
                                   T ~ 'Non adjacent counties'))
  
  plt_outliers <- df_for_plot %>% 
    ggplot(aes(x = n_pairs, y = scaled_pearson_resid)) +
    geom_point(aes(colour = pair_status, alpha = is_outlier)) +
    geom_text_repel(data = df_for_plot %>% 
                      filter(is_above_pair_threshold == T, is_outlier == T, pair_status != 'Within county', group_1 >= group_2) %>% 
                      mutate(group_1_short = strsplit(group_1, ' County') %>% map_chr(., 1),
                             group_2_short = strsplit(group_2, ' County') %>% map_chr(., 1)),
                    aes(label = paste(group_1_short, "&", group_2_short)),
                    max.overlaps = 100, min.segment.length = 0., 
                    nudge_x = 2.5, nudge_y = 0.5) +
    scale_x_continuous(trans = 'log', name = 'Number of pairs',
                       breaks = c(1., 1e1, 1e2, 1e3, 1e4, 1e5, 1e6),
                       expand = expansion(mult = c(0.05, 0.05)),
                       labels = c(expression(10^{0}), expression(10^{1}), expression(10^{2}), expression(10^{3}),
                                  expression(10^{4}), expression(10^{5}), expression(10^{6}))) +
    scale_y_continuous(name = 'Scaled Pearson residuals',
                       breaks = c(-7. -5, -3, 0., 3., 5., 7.)) +
    scale_alpha_manual(name = 'Outlier', breaks = c(T, F), values = c(1.0, 0.15),
                       labels = c('Yes', 'No')) +
    scale_colour_manual(name = '', values = c('darkgrey', 'aquamarine4', 'darkgoldenrod1'), 
                        breaks = c('Non adjacent counties', 'Adjacent counties', 'Within county')) +
    theme_classic() +
    theme(axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12))
  
  return(plt_outliers)
}
