library(tidyverse)

## Load characteristics of counties
df_char_counties <- read.csv('../data/maps/county_wa.csv') %>% 
  as_tibble() %>% select(county, region)

## Load workflow mobility data
df_workflows <- read.csv('../data/mobility/commuting_flows_WA_2020.csv', row.names = NULL) %>% 
  as_tibble() %>% select(-X)

## Compute RR of movements between places from commuting data
df_RR_mobility_complete <- expand.grid(county_1 = unique(df_workflows$county_residence),
                                       county_2 = unique(df_workflows$county_workplace))

  
df_RR_mobility_complete$n_tot_workflows <- sapply(1:nrow(df_RR_mobility_complete), FUN = function(i_row){
    
    curr_county_origin <- df_RR_mobility_complete$county_1[i_row]
    curr_county_destination <- df_RR_mobility_complete$county_2[i_row]
    
    workflow_1 <- df_workflows$n_workflows[
      df_workflows$county_residence == curr_county_origin & 
        df_workflows$county_workplace == curr_county_destination
    ]
    workflow_1 <- ifelse(length(workflow_1) == 0., 0., workflow_1)
    
    workflow_2 <- df_workflows$n_workflows[
      df_workflows$county_residence == curr_county_destination & 
        df_workflows$county_workplace == curr_county_origin
    ]
    workflow_2 <- ifelse(length(workflow_2) == 0., 0., workflow_2)
    
    workflow_1 + workflow_2
})

df_RR_mobility_complete <- df_RR_mobility_complete %>% as_tibble() %>% 
  group_by(county_1) %>% 
  mutate(n_tot_workflows_1_x = sum(n_tot_workflows)) %>% 
  group_by(county_2) %>% 
  mutate(n_tot_workflows_x_2 = sum(n_tot_workflows)) %>% 
  ungroup() %>% 
  mutate(n_tot_workflows_x_x = sum(n_tot_workflows),
         RR = n_tot_workflows / n_tot_workflows_1_x / n_tot_workflows_x_2 * n_tot_workflows_x_x) 
  
## RR but aggregated at the regional level
df_RR_mobility_complete_region <- df_RR_mobility_complete %>% 
  left_join(df_char_counties, by = c('county_1' = 'county')) %>% 
  rename(region_1 = region) %>% 
  left_join(df_char_counties, by = c('county_2' = 'county')) %>% 
  rename(region_2 = region) %>% 
  group_by(region_1, region_2) %>% 
  summarise(n_tot_workflows = sum(n_tot_workflows)) %>% 
  group_by(region_1) %>% 
  mutate(n_tot_workflows_1_x = sum(n_tot_workflows)) %>% 
  group_by(region_2) %>% 
  mutate(n_tot_workflows_x_2 = sum(n_tot_workflows)) %>% 
  ungroup() %>% 
  mutate(n_tot_workflows_x_x = sum(n_tot_workflows),
         RR = n_tot_workflows / n_tot_workflows_1_x / n_tot_workflows_x_2 * n_tot_workflows_x_x) 

#saveRDS(df_RR_mobility_complete, '../results/RR_mobility/RR_workflow_county_WA.rds')
#saveRDS(df_RR_mobility_complete_region, '../results/RR_mobility/RR_workflow_region_WA.rds')

## Load Safegraph (mobile phone) mobility data over the study period and by wave
df_visits <- read.csv('../data/mobility/safegraph_visits_march_2021_june_2022.csv', row.names = NULL) %>% 
  as_tibble()
df_visits_period_1 <- read.csv('../data/mobility/safegraph_visits_period_1.csv', row.names = NULL) %>% 
  as_tibble()
df_visits_period_2 <- read.csv('../data/mobility/safegraph_visits_period_2.csv', row.names = NULL) %>% 
  as_tibble()
df_visits_period_3 <- read.csv('../data/mobility/safegraph_visits_period_3.csv', row.names = NULL) %>% 
  as_tibble()
df_visits_period_4 <- read.csv('../data/mobility/safegraph_visits_period_4.csv', row.names = NULL) %>% 
  as_tibble()

get_RR_complete_from_df_visits <- function(df_visits){
  
  df_RR_safegraph_complete <- expand.grid(county_1 = unique(df_visits$county_origin),
                                          county_2 = unique(df_visits$county_destination))
  
  df_RR_safegraph_complete$n_tot_visits <- sapply(1:nrow(df_RR_safegraph_complete), FUN = function(i_row){
    
    curr_county_origin <- df_RR_safegraph_complete$county_1[i_row]
    curr_county_destination <- df_RR_safegraph_complete$county_2[i_row]
    
    workflow_1 <- df_visits$n_scaled_visits_county[
      df_visits$county_origin == curr_county_origin & 
        df_visits$county_destination == curr_county_destination
    ]
    workflow_1 <- ifelse(length(workflow_1) == 0., 0., workflow_1)
    
    workflow_2 <- df_visits$n_scaled_visits_county[
      df_visits$county_origin == curr_county_destination & 
        df_visits$county_destination == curr_county_origin
    ]
    workflow_2 <- ifelse(length(workflow_2) == 0., 0., workflow_2)
    
    workflow_1 + workflow_2
  })
  
  ## Compute RR of movements between places from mobile phone data
  df_RR_safegraph_complete <- df_RR_safegraph_complete %>% as_tibble() %>% 
    group_by(county_1) %>% 
    mutate(n_tot_visits_1_x = sum(n_tot_visits)) %>% 
    group_by(county_2) %>% 
    mutate(n_tot_visits_x_2 = sum(n_tot_visits)) %>% 
    ungroup() %>% 
    mutate(n_tot_visits_x_x = sum(n_tot_visits),
           RR = n_tot_visits / n_tot_visits_1_x / n_tot_visits_x_2 * n_tot_visits_x_x) 
  
  ## RR but aggregated at the regional level
  df_RR_safegraph_complete_region <- df_RR_safegraph_complete %>% 
    left_join(df_char_counties, by = c('county_1' = 'county')) %>% 
    rename(region_1 = region) %>% 
    left_join(df_char_counties, by = c('county_2' = 'county')) %>% 
    rename(region_2 = region) %>% 
    group_by(region_1, region_2) %>% 
    summarise(n_tot_visits = sum(n_tot_visits)) %>% 
    group_by(region_1) %>% 
    mutate(n_tot_visits_1_x = sum(n_tot_visits)) %>% 
    group_by(region_2) %>% 
    mutate(n_tot_visits_x_2 = sum(n_tot_visits)) %>% 
    ungroup() %>% 
    mutate(n_tot_visits_x_x = sum(n_tot_visits),
           RR = n_tot_visits / n_tot_visits_1_x / n_tot_visits_x_2 * n_tot_visits_x_x) 
  
  return(
    list(df_RR_safegraph_complete_county = df_RR_safegraph_complete,
         df_RR_safegraph_complete_region = df_RR_safegraph_complete_region)
  )
}

df_RR_safegraph_all_periods <- get_RR_complete_from_df_visits(df_visits)
df_RR_safegraph_period_1 <- get_RR_complete_from_df_visits(df_visits_period_1)
df_RR_safegraph_period_2 <- get_RR_complete_from_df_visits(df_visits_period_2)
df_RR_safegraph_period_3 <- get_RR_complete_from_df_visits(df_visits_period_3)
df_RR_safegraph_period_4 <- get_RR_complete_from_df_visits(df_visits_period_4)

saveRDS(df_RR_safegraph_all_periods$df_RR_safegraph_complete_county, '../results/RR_mobility/RR_mobile_phone_county_WA.rds')
saveRDS(df_RR_safegraph_all_periods$df_RR_safegraph_complete_region, '../results/RR_mobility/RR_mobile_phone_region_WA.rds')

saveRDS(df_RR_safegraph_period_1$df_RR_safegraph_complete_county, '../results/RR_mobility/RR_mobile_phone_county_WA_period_1.rds')
saveRDS(df_RR_safegraph_period_1$df_RR_safegraph_complete_region, '../results/RR_mobility/RR_mobile_phone_region_WA_period_1.rds')

saveRDS(df_RR_safegraph_period_2$df_RR_safegraph_complete_county, '../results/RR_mobility/RR_mobile_phone_county_WA_period_2.rds')
saveRDS(df_RR_safegraph_period_2$df_RR_safegraph_complete_region, '../results/RR_mobility/RR_mobile_phone_region_WA_period_2.rds')

saveRDS(df_RR_safegraph_period_3$df_RR_safegraph_complete_county, '../results/RR_mobility/RR_mobile_phone_county_WA_period_3.rds')
saveRDS(df_RR_safegraph_period_3$df_RR_safegraph_complete_region, '../results/RR_mobility/RR_mobile_phone_region_WA_period_3.rds')

saveRDS(df_RR_safegraph_period_4$df_RR_safegraph_complete_county, '../results/RR_mobility/RR_mobile_phone_county_WA_period_4.rds')
saveRDS(df_RR_safegraph_period_4$df_RR_safegraph_complete_region, '../results/RR_mobility/RR_mobile_phone_region_WA_period_4.rds')
