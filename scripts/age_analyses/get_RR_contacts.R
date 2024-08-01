## This script enables to compute the relative risk of contatcs of happening between two age groups
## from WA contact data estimated by Mistry et al. 10.1038/s41467-020-20544-y

library(tidyverse)
source('utils_contact_mat.R')

## Load WA age distribution from Mistry et al. 10.1038/s41467-020-20544-y
age_dist_WA <- read.csv('../data/contact_data/United_States_subnational_Washington_age_distribution_85.csv', header = F) %>% 
  as_tibble() %>% rename(age = V1, n_indiv = V2) %>% 
  mutate(age_decade = case_when(age <= 9 ~ '0-9y', age <= 19 ~ '10-19y',
                                age <= 29 ~ '20-29y', age <= 39 ~ '30-39y',
                                age <= 49 ~ '40-49y', age <= 59 ~ '50-59y',
                                age <= 69 ~ '60-69y', age <= 79 ~ '70-79y',
                                TRUE ~ '80y+')) %>% 
  mutate(age = as.character(age))

## Compute age distribution in decades
age_dist_decade_WA <- age_dist_WA %>% 
  group_by(age_decade) %>% 
  summarise(n_indiv = sum(n_indiv))

## Get contact matrices by setting for WA state for our age groups from Mistry et al. 10.1038/s41467-020-20544-y
# The contact rate represents the per capita probability of contact for an individual of age i (row) with individuals of age j (column)
contact_mat_overall_WA <- read.csv('../data/contact_data/United_States_subnational_Washington_M_overall_contact_matrix_85.csv', header = F)

## Dataframe with number of contacts occurring between two age groups (1-year age bin)
df_contact_overall <- get_df_contacts_from_mat(contact_mat_overall_WA, age_dist_WA)

## Contact matrix between age groups in decades
## (coefficients correspond to the average daily number of contacts that an indiv of age i has with indivs of age k)
contact_mat_decade_overall <- get_contact_mat_decade_from_df_contacts(df_contact_overall)

# Relative risk of contacts between two age groups
df_RR_overall <- get_RR_contacts_from_df_contacts(df_contact_overall)
saveRDS(df_RR_overall, '../results/RR_contacts/df_RR_overall.rds')
