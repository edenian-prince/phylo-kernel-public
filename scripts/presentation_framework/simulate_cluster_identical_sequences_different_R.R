## This script simulates a branching process with mutations
## assuming a Poisson distributed offspring distribution. 
## This simulation approach follows that described in Tran-Kiem and Bedford 10.1073/pnas.2305299121.

## We then simulate two clusters with different reproduction numbers (high and low) and display them.

library(tidyverse)
library(purrr)
library(ggstar)

## Function to simulate a cluster of identical sequence
## with infector-infectee relationships using a branching process
simulate_epi_clusters <- function(prop_stay_within_group = 0.8, # Probability to stay within group
                                  R0 = 1.2, # Reproduction number
                                  p_trans_before_mut = 0.69, # Probability that an infectee has the same consensus sequence
                                  n_max_gen = 5, # Maximum number of generations to draw (The simulation ends either when the cluster gets extincts or when reaching n_max_gen)
                                  n_groups = 6 # Number of groups (e.g. counties, age groups...)
                                  ){
  
  mat_proba_spread_group <- matrix((1. - prop_stay_within_group)/(n_groups - 1), nrow = n_groups, ncol = n_groups)
  diag(mat_proba_spread_group) <- rep(prop_stay_within_group, n_groups)
  
  # Initialization 
  group_primary <- 1 # Draw the group of the primary infector
  id_primary <- 1
  vec_group_previous_gen <- c(group_primary)
  vec_id_previous_gen <- c(id_primary)
  max_group <- group_primary
  vec_group_new_gen <- NULL
  vec_id_new_gen <- NULL
  
  tot_cluster_size <- 1
  curr_pop_size <- 1
  i_gen <- 0
  vec_group_full_cluster <- c(group_primary)
  vec_id_full_cluster <- c(id_primary) # Vector to store the infectee ID
  vec_id_infector_full_cluster <- c(NA) # Vector to store the infector ID
  vec_gen_full_cluster <- c(i_gen) # Vector to store the generation of infection
  max_id <- 1
  # Simulate branching process
  while(curr_pop_size > 0 && i_gen < n_max_gen){
    
    # Loop over all infected individuals (previous generation)
    for(i_infected in 1:curr_pop_size){
      curr_group_infected <- vec_group_previous_gen[i_infected]
      curr_id_infected <- vec_id_previous_gen[i_infected]
      
      # Draw number of infected individuals
      n_new_infected_with_identical_sequences <- rpois(n = 1, lambda = R0)
      if(n_new_infected_with_identical_sequences > 0){
        
        id_draw <- (max_id + 1):(max_id + n_new_infected_with_identical_sequences)
        max_id <- max_id + n_new_infected_with_identical_sequences
        vec_group_draw <- rmultinom(n = 1, 
                                    size = n_new_infected_with_identical_sequences,
                                    prob = mat_proba_spread_group[curr_group_infected, ])
        group_draw <- Reduce('c', lapply(1:length(vec_group_draw), FUN = function(i_draw){
          rep(i_draw, vec_group_draw[i_draw])
        }))
        
        vec_group_full_cluster <- c(vec_group_full_cluster, group_draw)
        vec_id_full_cluster <- c(vec_id_full_cluster, id_draw)
        vec_gen_full_cluster <- c(vec_gen_full_cluster, rep(i_gen + 1, length(id_draw)))
        vec_id_infector_full_cluster <- c(vec_id_infector_full_cluster, rep(curr_id_infected, n_new_infected_with_identical_sequences))
        vec_group_new_gen <- c(vec_group_new_gen, group_draw)
        vec_id_new_gen <- c(vec_id_new_gen, id_draw)
        
      }
    }
    
    ## Update 
    vec_group_previous_gen <- vec_group_new_gen
    vec_id_previous_gen <- vec_id_new_gen
    curr_pop_size <- length(vec_group_previous_gen)
    vec_group_new_gen <- NULL
    vec_id_new_gen <- NULL
    i_gen <- i_gen + 1
  }
  
  # Draw the mutations probability at every step
  vec_has_mutated <- c(0, rbernoulli(n = length(vec_id_full_cluster) - 1, p = 1. - p_trans_before_mut))
  
  return(
    list('vec_group_full_cluster' = vec_group_full_cluster,
         'vec_id_full_cluster' = vec_id_full_cluster,
         'vec_id_infector_full_cluster' = vec_id_infector_full_cluster,
         'vec_has_mutated' = vec_has_mutated,
         'vec_gen_full_cluster' = vec_gen_full_cluster
    )
  )
}

## Get dataframe with infector-infectee pairs from an ouput of simulate_epi_clusters
get_infector_infectee_pairs_from_sim_epi_clusters <- function(list_sim){
  df_res <- tibble(infector = list_sim$vec_id_infector_full_cluster,
                   infectee = list_sim$vec_id_full_cluster,
                   group_infectee = list_sim$vec_group_full_cluster,
                   has_mutated = list_sim$vec_has_mutated,
                   gen_infectee = list_sim$vec_gen_full_cluster) %>% 
    arrange(gen_infectee)
  
  vec_n_mut_from_primary <- rep(NA, nrow(df_res))
  
  for(i_row in 1:nrow(df_res)){
    if(df_res[i_row, 'gen_infectee'] == 0){
      vec_n_mut_from_primary[i_row] <- 0
    } else{
      i_infector <- df_res[i_row, 'infector'] %>% as.numeric()
      vec_n_mut_from_primary[i_row] <- vec_n_mut_from_primary[i_infector] + as.numeric(df_res[i_row, 'has_mutated'])
    }
  }
  df_res$n_mut <- vec_n_mut_from_primary
  
  return(df_res)
}


## Perform simulations
### Lower reproduction number simulation (R0 = 1.2)
set.seed(91)
list_sim_1 <- simulate_epi_clusters(R0 = 1.2, 
                                    n_max_gen = 10,
                                    p_trans_before_mut = 0.69,
                                    prop_stay_within_group = 0.7,
                                    n_groups = 6)
df_pairs_1 <- get_infector_infectee_pairs_from_sim_epi_clusters(list_sim_1) %>% 
  filter(n_mut <= 1)

df_pairs_1

### Higher reproduction number simulation (R0 = 2.0)
set.seed(32)
list_sim_2 <- simulate_epi_clusters(R0 = 2.0, n_max_gen = 10,
                                    p_trans_before_mut = 0.69,
                                    prop_stay_within_group = 0.7,
                                    n_groups = 6)

df_pairs_2 <- get_infector_infectee_pairs_from_sim_epi_clusters(list_sim_2) %>% 
  filter(n_mut == 0 | (n_mut == 1 & has_mutated == T))
df_pairs_2

## Display results
### Reorder individuals for nicer plots
df_low_R <- df_pairs_1 %>% 
  filter(n_mut == 0 | (n_mut == 1 & has_mutated == T)) %>% 
  group_by(gen_infectee) %>%
  mutate(infectee_corr = rank(infectee)) %>% ungroup() %>% 
  left_join(df_pairs_1 %>% 
              filter(n_mut == 0 | (n_mut == 1 & has_mutated == T)) %>% 
              group_by(gen_infectee) %>% 
              mutate(infectee_corr = rank(infectee)) %>% ungroup() %>% 
              select(infectee_corr, gen_infectee, infectee) %>% 
              rename(infector = infectee, infector_corr = infectee_corr, gen_infector = gen_infectee),
            by = 'infector') %>% 
  mutate(scenario = 'Lower transmission intensity')

df_high_R <- df_pairs_2 %>% 
  filter(n_mut == 0 | (n_mut == 1 & has_mutated)) %>%
  group_by(gen_infectee) %>% 
  mutate(infectee_corr = rank(infectee)) %>% 
  left_join(df_pairs_2 %>% 
              group_by(gen_infectee) %>% 
              filter(n_mut == 0 | (n_mut == 1 & has_mutated)) %>%
              mutate(infectee_corr = rank(infectee)) %>% ungroup() %>% 
              select(infectee_corr, gen_infectee, infectee) %>% 
              rename(infector = infectee, infector_corr = infectee_corr, gen_infector = gen_infectee),
            by = 'infector') %>% 
  mutate(scenario = 'Higher transmission intensity')

### Plot the results
gen_max_plot <- 9 # Maximum number of generations to plot

plt_cluster_over_time <- bind_rows(df_low_R, df_high_R) %>% 
  filter(gen_infectee <= gen_max_plot) %>% 
  mutate(scenario = factor(scenario,
                           levels = c('Lower transmission intensity', 
                                      'Higher transmission intensity')),
         group_infectee_bis = group_infectee * as.numeric(n_mut == 0)) %>%
  ggplot(aes(x = gen_infectee, y = infectee_corr)) +
  geom_segment(aes(x = gen_infector, xend = gen_infectee,
                   y = infector_corr, yend = infectee_corr),
               color = 'gray30',
               arrow = arrow(length = unit(0.02, 'npc')),
               alpha = 0.5) +
  geom_point(aes(colour = as.factor(group_infectee_bis),
                 size = as.factor(n_mut == 0),
                 alpha = as.factor(n_mut == 0))) +
  geom_star(data = bind_rows(df_low_R, df_high_R) %>% 
              filter(has_mutated == T) %>% 
              filter(gen_infectee <= gen_max_plot),
            aes(x = (gen_infector + gen_infectee)/2,
                y = (infector_corr + infectee_corr)/2),
            color = 'firebrick', fill = 'firebrick',
            size = 2) +
  scale_x_continuous(name = 'Time (in generations)',
                     breaks = 0:10) +
  scale_size_manual(breaks = c(TRUE, FALSE), values = c(3., 1.)) +
  scale_alpha_manual(breaks = c(TRUE, FALSE), values = c(1., 0.)) +
  scale_colour_manual(name = 'Group',
                      breaks = c(0:6),
                      values = c('gray22', '#002a4b', '#b0a8b9', '#0088af', '#ff8066', '#467f18', '#7bb54d')
                      
  ) +
  facet_wrap(. ~ scenario) +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(),
        strip.text = element_text(size = 12),
        legend.position = 'none',
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12))

plot(plt_cluster_over_time)
