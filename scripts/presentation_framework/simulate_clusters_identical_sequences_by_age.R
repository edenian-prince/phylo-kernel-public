## This script simulates a branching process with mutations
## assuming a Poisson distributed offspring distribution. 
## This simulation approach follows that described in 
## Tran-Kiem and Bedford 10.1073/pnas.2305299121.
library(tidyverse)
source('../age_analyses/utils_contact_mat.R')

## Load contact matrix
contact_mat_WA <- readRDS('../../results/RR_contacts/contact_mat_decade_overall.rds')
rho_eigenval <- get_max_eigenval(contact_mat_WA)
contact_mat_WA_adj <- contact_mat_WA / rho_eigenval
vec_age_groups <- colnames(contact_mat_WA)

## Load RR of contact between groups
df_RR_overall <- readRDS('../../results/RR_contacts/df_RR_overall.rds')

## Function to simulate clusters of identical sequences with age
simulate_cluster_identical_sequences_with_age <- function(contact_mat_adj = contact_mat_WA_adj,
                                                          R0,
                                                          p_trans_before_mut,
                                                          p_seq,
                                                          n_max_gen = 20){
  
  n_age_groups <- nrow(contact_mat_adj)
  vec_R0_age <- apply(contact_mat_adj, 1, sum) * R0
  
  # Normalized contact matrix where sum of each row is equal to 1
  contact_mat_prop <- contact_mat_adj / 
    matrix(rep(apply(contact_mat_adj, 1, sum), n_age_groups), ncol = n_age_groups, nrow = n_age_groups, byrow = F)
  
  # Initialization 
  age_primary <- sample(x = n_age_groups, size = 1) # Draw the age of the primary infector
  vec_age_previous_gen <- c(age_primary)
  vec_age_new_gen <- NULL
  
  tot_cluster_size <- 1
  curr_pop_size <- 1
  i_gen <- 0
  vec_age_full_cluster <- c(age_primary)
  vec_age_sequenced_indiv <- NULL
  
  if(rbernoulli(n = 1, p = p_seq) == 1){
    vec_age_sequenced_indiv <- c(vec_age_sequenced_indiv, age_primary)
  }
  
  # Simulate branching process
  while(curr_pop_size > 0 && i_gen < n_max_gen){
    
    # Loop over all infected individuals (previous generation)
    for(i_infected in 1:curr_pop_size){
      curr_age_infected <- vec_age_previous_gen[i_infected]
      curr_R0 <- vec_R0_age[curr_age_infected]
      
      # Draw number of infected individuals
      n_new_infected_with_identical_sequences <- rpois(n = 1, lambda = curr_R0 * p_trans_before_mut)
      if(n_new_infected_with_identical_sequences > 0){
        age_draw <- rmultinom(n = 1, size = n_new_infected_with_identical_sequences, prob =  as.numeric(contact_mat_prop[curr_age_infected, ]))
        vec_age_infected <- Reduce('c', lapply(1:n_age_groups, FUN = function(i_age){
          rep(i_age, age_draw[i_age])
        }))
        vec_age_full_cluster <- c(vec_age_full_cluster, vec_age_infected)
        vec_age_new_gen <- c(vec_age_new_gen, vec_age_infected)
        # Are these individuals sequenced?
        vec_age_sequenced <- vec_age_infected[rbernoulli(n = n_new_infected_with_identical_sequences, p = p_seq)]
        vec_age_sequenced_indiv <- c(vec_age_sequenced_indiv, vec_age_sequenced)
      }
    }
    
    ## Update 
    vec_age_previous_gen <- vec_age_new_gen
    curr_pop_size <- length(vec_age_previous_gen)
    vec_age_new_gen <- NULL
  }
  
  return(
    list('vec_age_sequenced_indiv' = vec_age_sequenced_indiv,
         'vec_age_full_cluster' = vec_age_full_cluster
    )
  )
}

## Function to get pairwise distance from the output of simulate_cluster_identical_sequences_with_age
get_pairwise_distance_from_vec_age <- function(vec_age_sequenced_indiv){
  if(is.null(vec_age_sequenced_indiv)){
    return(NULL)
  } else if(length(vec_age_sequenced_indiv) <= 1) {
    return(NULL)
  } else {
    df_age_indiv <- tibble(id_indiv = 1:length(vec_age_sequenced_indiv),
                           age = vec_age_sequenced_indiv)
    
    df_pairs <- expand.grid(id_indiv_1 = 1:length(vec_age_sequenced_indiv),
                            id_indiv_2 = 1:length(vec_age_sequenced_indiv)) %>% 
      filter(id_indiv_1 != id_indiv_2) %>% 
      left_join(df_age_indiv, by = c('id_indiv_1' = 'id_indiv')) %>% 
      rename(age_1 = age) %>% 
      left_join(df_age_indiv, by = c('id_indiv_2' = 'id_indiv')) %>% 
      rename(age_2 = age) %>% 
      group_by(age_1, age_2) %>% 
      summarise(n_pairs = n()) %>% 
      ungroup()
    
    return(df_pairs)
  }
}

n_clusters <- 1e5. # This takes around 5 minutes for 1e5 clusters
R0 <- 1.2
p_trans_before_mut <- 0.7
p_seq <- 0.1

set.seed(87245)
t0 <- Sys.time()
df_pairs_id_seq <- Reduce('bind_rows', lapply(1:n_clusters, FUN = function(i_cluster){
  curr_sim <- simulate_cluster_identical_sequences_with_age(contact_mat_adj = contact_mat_WA_adj,
                                                            R0 = R0,
                                                            p_trans_before_mut = p_trans_before_mut,
                                                            p_seq = p_seq,
                                                            n_max_gen = 10)
  get_pairwise_distance_from_vec_age(curr_sim$vec_age_sequenced_indiv)
}))
t1 <- Sys.time()
print(t1 - t0)

## Compute RR of identical sequences between age groups
df_RR_id_seq <- df_pairs_id_seq %>% 
  group_by(age_1, age_2) %>%
  summarise(n_pairs_age_1_age_2 = sum(n_pairs)) %>%
  group_by(age_1) %>%
  mutate(n_pairs_age_1_age_x = sum(n_pairs_age_1_age_2)) %>%
  group_by(age_2) %>%
  mutate(n_pairs_age_x_age_2 = sum(n_pairs_age_1_age_2)) %>%
  ungroup() %>%
  mutate(n_pairs_age_x_age_x = sum(n_pairs_age_1_age_2)) %>%
  mutate(RR = n_pairs_age_1_age_2/n_pairs_age_1_age_x/n_pairs_age_x_age_2*n_pairs_age_x_age_x) %>%
  mutate(age_decade_1 = vec_age_groups[age_1], age_decade_2 = vec_age_groups[age_2]) 

## Display the relationship between the RR of contacts and the RR of observing identical sequences between groups
plt_comp_RR_id_seq_contacts <- df_RR_id_seq %>% 
  left_join(df_RR_overall, by = c('age_decade_1' = 'age_decade_i', 'age_decade_2' = 'age_decade_j')) %>% 
  ggplot(aes(x = RR_contacts, y = RR)) +
  geom_smooth(col = 'darkcyan', fill = 'darkcyan',
              method =  'loess') +
  geom_point() +
  scale_x_continuous(trans = 'log', breaks = c(0.5, 0.8, 1.0, 2.0, 3.0, 5., 10., 15.),
                     name = expression(RR['contacts']),
                     labels = c('0.5', '0.8', '1', '2', '3', '5', '10', '15')) +
  scale_y_continuous(trans = 'log', breaks = seq(0.1, 2.5, 0.1),
                     name = expression(RR['identical sequences'])) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 13))
  
plot(plt_comp_RR_id_seq_contacts)

pdf('../../figures/supplementary_figures/expected_relationship_RR_id_seq_contacts.pdf',
    height = 4, width = 5)
plot(plt_comp_RR_id_seq_contacts)
dev.off()
  
