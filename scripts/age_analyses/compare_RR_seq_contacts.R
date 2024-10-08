## This script compares the relative risk of observing identical sequences
## between two age groups and the relative risk of contacts of occurring between two groups.
## It also reproduces Figure 4A.

library(tidyverse)
library(vegan)
source('../utils_comp_RR.R')

## Load relative risk of observing identical sequences between ahe groups
df_RR_age <- read_csv('../../results/RR_age/df_RR_age_0_mut_away.csv') %>% rename(RR_seq = RR)


## Load results from subsampling to compute uncertainty intervals around RR of identical sequences
df_uncertainty_age <- readRDS('../../results/RR_age/df_RR_uncertainty_age_0_mut_away.rds') %>% 
  group_by(n_mutations, group_1, group_2) %>% 
  summarise(median_RR = median(RR), 
            lower_RR = quantile(RR, 0.025), 
            upper_RR = quantile(RR, 0.975)) %>% 
  ungroup()

## Load relative risk of contacts between two age groups
## This can be obtained using scripts/age_analyses/get_RR_contacts.R
df_RR_contacts <- readRDS( '../../results/RR_contacts/df_RR_overall.rds') %>% 
  select(age_decade_i, age_decade_j, RR_contacts)

## Join dataframes for comparison
df_RR_for_comparison <- df_RR_age %>% 
  left_join(df_RR_contacts, by = c('group_1' = 'age_decade_i', 'group_2' = 'age_decade_j')) %>% 
  left_join(df_uncertainty_age %>% select(- n_mutations), by = c('group_1', 'group_2'))

## Compute correlation coefficients
df_RR_for_comparison %>% filter(group_1 >= group_2) %>% 
  summarise(cor_pearson = cor(RR_seq, RR_contacts, method = 'pearson'),
            pval_pearson = cor.test(RR_seq, RR_contacts, method = 'pearson')$p.value,
            cor_pearson_log = cor(log(RR_seq), log(RR_contacts), method = 'pearson'),
            pval_pearson_log = cor.test(log(RR_seq), log(RR_contacts), method = 'pearson')$p.value,
            cor_spearman = cor(RR_seq, RR_contacts, method = 'spearman'),
            pval_spearman = cor.test(RR_seq, RR_contacts, method = 'spearman')$p.value)

cor.test(as.numeric(unlist(df_RR_for_comparison %>% filter(group_1 >= group_2) %>% select(RR_seq))),
         as.numeric(unlist(df_RR_for_comparison %>% filter(group_1 >= group_2) %>% select(RR_contacts))),
         method = 'spearman')


## Display comparison
plt_comp_RR_id_seq_contacts <- df_RR_for_comparison %>% 
  filter(group_1 >= group_2) %>% 
  ggplot(aes(x = RR_contacts, y = RR_seq)) +
  geom_point(aes(colour = (group_1 == group_2))) +
  geom_linerange(aes(colour = (group_1 == group_2), ymin = lower_RR, ymax = upper_RR)) +
  stat_cor(cor.coef.name = 'rho', method = 'spearman') +
  scale_x_continuous(trans = 'log', breaks = c(0.5, 1.0, 2.0, 5.0, 10.0),
                     name = expression(RR['contacts'])) +
  scale_y_continuous(trans = 'log', breaks = c(0.9, 1.0, 1.1, 1.3, 1.5, 1,7, 1.9),
                     name = expression(RR['identical sequences'])) +
  scale_colour_manual(breaks = c(T, F), labels = c('Yes', 'No'), name = 'Within age group',
                      values = c('firebrick2', 'gray22')) +
  theme_classic() +
  theme(legend.position = c(0.2, 0.5),
        legend.background = element_blank(),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 13))

plot(plt_comp_RR_id_seq_contacts)
