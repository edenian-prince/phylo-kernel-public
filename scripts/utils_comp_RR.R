## Transform dataframe with pairs of identical sequences between groups
## to a square RR matrix
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

## Get Mantel r (and perform a Mantel test with a given number of permutations)
get_mantel_comparison_from_mat_RR <- function(mat_RR_1, mat_RR_2, n_permut = 1e5 - 1, corr_method = 'spearman'){
  mantel_test <- mantel(xdis = mat_RR_1, ydis = mat_RR_2, method = corr_method, permutation = n_permut)
  return(mantel_test)
}

## Runs a GAM using a spline smooth term with of dimension k_gam
## between columns predictor_name and response_name.
## This returns the fitted GAM and a dataframe with predicted
## value from the GAM (and 95% prediction intervals). 
run_gam <- function(df_with_RR_for_gam = df_RR_for_comparison, 
                    predictor_name = 'log_RR_contact', response_name = 'log_RR_seq',
                    k_gam = -1){
  
  df_for_gam <- df_with_RR_for_gam %>% 
    rename(RR_predictor = predictor_name, RR_response = response_name) %>% 
    select(group_1, group_2, RR_response, RR_predictor, n_pairs)
  
  gam_mod <- gam(data = df_for_gam, formula = RR_response ~ s(RR_predictor, k = k_gam))
  
  df_pred <- df_for_gam %>% 
    mutate(RR_response_pred_central = augment(gam_mod)$.fitted,
           RR_response_pred_lower = augment(gam_mod)$.fitted - 1.96 * (augment(gam_mod)$.se.fit),
           RR_response_pred_upper = augment(gam_mod)$.fitted + 1.96 * (augment(gam_mod)$.se.fit),
           scaled_pearson_resid = residuals(gam_mod, type = 'scaled.pearson'))
  
  
  return(list(mod = gam_mod, df_pred = df_pred))
}

## Plots the fitted GAM. 
## The function enables to define a function to transform the response (transform_func_var_response)
## and the predictor variables (transform_func_var_predictor).  
## trans_x_axis_plot and trans_y_axis_plot can be used to specify a transform of the axes (needs to be acceptable by ggplot).
## Point are coloured based on the adjacency status of pairs of counties (indicated by df_adj)
plot_fit_gam_counties <- function(res_gam = gam_seq_workflow_counties, 
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
                                  df_adj
){
  
  if(trans_x_axis_plot == 'identity' | trans_y_axis_plot == 'identity'){
    identity <- function(x){return(x)}
  }
  
  plt_fit_gam <- res_gam$df_pred %>% 
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
    scale_x_continuous(name = name_x_axis, 
                       trans = trans_x_axis_plot,
                       breaks = breaks_x_axis,
                       expand = expansion(mult = c(0.05, 0.08)),
                       labels = labels_x_axis) +
    scale_y_continuous(name = name_y_axis, 
                       trans = trans_y_axis_plot,
                       breaks = breaks_y_axis,
                       expand = expansion(mult = c(0.05, 0.05)),
                       labels = labels_y_axis) +
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

## Function to get outliers in the relationship between genetic and mobility data
## (as inferred by the GAM res_gam). Outliers are defined as pairs of groups for
## which the number of pairs of identical sequences is greater than min_n_pairs
## and for which the absolute value of the Pearson scaled residuals is greater 
## than threshold_residuals_outliers
get_outliers_fit <- function(res_gam = gam_seq_workflow_counties, min_n_pairs = 100, threshold_residuals_outliers = 3.){
  
  res_gam$df_pred %>% 
    filter(n_pairs >= min_n_pairs, abs(scaled_pearson_resid) > threshold_residuals_outliers) %>% 
    return()
  
}

## Function to display outliers in the relationship between genetic and mobility data
plot_outliers_fit_counties <- function(res_gam, min_n_pairs = 100, threshold_residuals_outliers = 3.){
  
  ## Get outliers
  df_outliers <- get_outliers_fit(res_gam, min_n_pairs, threshold_residuals_outliers)
  
  df_for_plot <- res_gam$df_pred %>% 
    left_join(df_adj, by = c('group_1', 'group_2'))  %>% 
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


plot_fit_gam_regions_with_uncertainty <- function(res_gam = gam_seq_workflow_regions, 
                                                  df_RR_uncertainty_regions,
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
                                                  trans_y_axis_plot = 'log'
){
  
  if(trans_x_axis_plot == 'identity' | trans_y_axis_plot == 'identity'){
    identity <- function(x){return(x)}
  }
  
  plt_fit_gam <- res_gam$df_pred %>% 
    left_join(df_RR_uncertainty_regions) %>% 
    mutate(RR_predictor_transform = get(transform_func_predictor)(RR_predictor),
           RR_response_transform = get(transform_func_response)(RR_response),
           RR_response_pred_central_transform = get(transform_func_response)(RR_response_pred_central),
           RR_response_pred_lower_transform = get(transform_func_response)(RR_response_pred_lower),
           RR_response_pred_upper_transform = get(transform_func_response)(RR_response_pred_upper)) %>% 
    ggplot(aes(x = RR_predictor_transform)) +
    geom_point(aes(y = RR_response_transform)) +
    geom_line(aes(y = RR_response_pred_central_transform), col = 'gray22') +
    geom_linerange(aes(ymin = lower_RR, ymax = upper_RR)) +
    geom_ribbon(aes(ymin = RR_response_pred_lower_transform, ymax = RR_response_pred_upper_transform),
                fill = 'gray22', alpha = 0.2) +
    scale_x_continuous(name = name_x_axis, 
                       trans = trans_x_axis_plot,
                       breaks = breaks_x_axis,
                       expand = expansion(mult = c(0.05, 0.08)),
                       labels = labels_x_axis) +
    scale_y_continuous(name = name_y_axis, 
                       trans = trans_y_axis_plot,
                       breaks = breaks_y_axis,
                       expand = expansion(mult = c(0.05, 0.05)),
                       labels = labels_y_axis) +
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

## Function to plot the results of the GAM (regional level)
plot_fit_gam_regions <- function(res_gam = gam_seq_workflow_regions, 
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
                                 trans_y_axis_plot = 'log'
){
  
  if(trans_x_axis_plot == 'identity' | trans_y_axis_plot == 'identity'){
    identity <- function(x){return(x)}
  }
  
  plt_fit_gam <- res_gam$df_pred %>% 
    mutate(RR_predictor_transform = get(transform_func_predictor)(RR_predictor),
           RR_response_transform = get(transform_func_response)(RR_response),
           RR_response_pred_central_transform = get(transform_func_response)(RR_response_pred_central),
           RR_response_pred_lower_transform = get(transform_func_response)(RR_response_pred_lower),
           RR_response_pred_upper_transform = get(transform_func_response)(RR_response_pred_upper)) %>% 
    ggplot(aes(x = RR_predictor_transform)) +
    geom_point(aes(y = RR_response_transform)) +
    geom_line(aes(y = RR_response_pred_central_transform), col = 'gray22') +
    geom_ribbon(aes(ymin = RR_response_pred_lower_transform, ymax = RR_response_pred_upper_transform),
                fill = 'gray22', alpha = 0.2) +
    scale_x_continuous(name = name_x_axis, 
                       trans = trans_x_axis_plot,
                       breaks = breaks_x_axis,
                       expand = expansion(mult = c(0.05, 0.08)),
                       labels = labels_x_axis) +
    scale_y_continuous(name = name_y_axis, 
                       trans = trans_y_axis_plot,
                       breaks = breaks_y_axis,
                       expand = expansion(mult = c(0.05, 0.05)),
                       labels = labels_y_axis) +
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

