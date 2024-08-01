# Function to compute the total number of contacts occurring between
# two age groups of the population (across all individuals)
get_df_contacts_from_mat <- function(contact_mat, age_dist){
  # contact_mat denotes the contact matrix and age_dist the number of individuals in two age groups. 
  df_contact <- contact_mat %>% 
    mutate(age_i = 0:(n() - 1)) %>% 
    pivot_longer(cols = -'age_i', names_prefix = 'V', names_to = 'age_j', values_to = 'contact_rate') %>% 
    mutate(age_i = as.character(age_i), age_j = as.character(as.numeric(age_j) - 1)) %>%
    left_join(age_dist %>% rename(n_indiv_i = n_indiv, age_decade_i = age_decade), by = c('age_i' = 'age')) %>% 
    left_join(age_dist %>% rename(n_indiv_j = n_indiv, age_decade_j = age_decade), by = c('age_j' = 'age'))
  
  return(df_contact)
}

# Function to compute the RR of contacts between two age groups
# from the total number of contacts occurring between these two groups
get_RR_contacts_from_df_contacts <- function(df_contacts){
  df_RR <- df_contacts %>% 
    mutate(n_contacts_ij = contact_rate * n_indiv_i) %>% 
    group_by(age_decade_i, age_decade_j) %>% 
    summarise(n_contacts_ij = sum(n_contacts_ij)) %>% 
    group_by(age_decade_i) %>% 
    mutate(n_contacts_ix = sum(n_contacts_ij)) %>% 
    group_by(age_decade_j) %>% 
    mutate(n_contacts_xj = sum(n_contacts_ij)) %>% 
    ungroup() %>% 
    mutate(n_contacts_xx = sum(n_contacts_ij),
           RR_contacts = n_contacts_ij/n_contacts_ix/n_contacts_xj*n_contacts_xx) 
  
  return(df_RR)
}

# Function to compute the contact matrix in decades from the
# number of contacts occurring between singe-year age groups
get_contact_mat_decade_from_df_contacts <- function(df_contacts){
  
  df_contact_mat <- df_contacts %>% 
    mutate(n_tot_contacts_ij = contact_rate * n_indiv_i) %>% 
    group_by(age_decade_i, age_decade_j) %>% 
    summarise(n_tot_contacts_ij = sum(n_tot_contacts_ij)) %>% 
    left_join(age_dist_decade_WA, by = c('age_decade_i' = 'age_decade')) %>% 
    rename(n_indiv_i = n_indiv) %>% 
    left_join(age_dist_decade_WA, by = c('age_decade_j' = 'age_decade')) %>% 
    rename(n_indiv_j = n_indiv) %>% 
    mutate(n_contact_per_day_ij = n_tot_contacts_ij/n_indiv_i) %>% 
    select(age_decade_i, age_decade_j, n_contact_per_day_ij) %>% 
    ungroup()
  
  contact_mat <- df_contact_mat %>% 
    pivot_wider(names_from = 'age_decade_j', values_from = 'n_contact_per_day_ij') %>% 
    arrange(age_decade_i) %>% 
    select(- age_decade_i) %>% 
    as.matrix()
  
  rownames(contact_mat) <- colnames(contact_mat)
  
  return(contact_mat)
}
