library(dplyr)
library(ggplot2)
library(RColorBrewer)

## Function to compute the probability for individuals being n_gen_away of having sequences at a given genetic distance
get_proba_m_mutations <- function(n_max_mutations, mutations_per_day,
                                  mean_gen_time, sd_gen_time,
                                  n_gen_away = 1){
  
  ## n_max_mutations: Upper bound for number of mutations for which this probability is computed
  ## mutations_per_day: pathogen mutation rate (in mutations per day)
  ## mean_gen_time: mean generation time (in days)
  ## sd_gen_time: SD for the generation time (in days)
  ## n_gen_away: number of generations separating two individuals
  
  ## Parameters of the Gamma distribution used for the generation time
  alpha_gen_time <- (mean_gen_time^2) / (sd_gen_time)^2
  beta_gen_time <- mean_gen_time / (sd_gen_time)^2
  
  ## Derive the probability density vector
  vec_m <- 0:n_max_mutations
  tmp_ratio <- beta_gen_time / (mutations_per_day + beta_gen_time)
  vec_density <- sapply(vec_m, FUN = function(nb_mutations_away_obs){
    dnbinom(x = nb_mutations_away_obs, size = alpha_gen_time*n_gen_away, prob = tmp_ratio)
  })
  
  vec_proba <- vec_density/sum(vec_density)
  
  df_res <- tibble(n_mutations_away = vec_m,
                   proba = vec_density)
  
  return(df_res)
}

## Parameters used for the simulation of a SARS-CoV-2 like virus
mutation_rate_per_day <- 32.765 / 365
1/mutation_rate_per_day
mean_gen_time <- 5.9
sd_gen_time <- 4.8

vec_nb_gen_away <- 1:6
df_proba <- Reduce('bind_rows', lapply(vec_nb_gen_away, FUN = function(curr_n_gen_away){
  get_proba_m_mutations(n_max_mutations = 40, mutation_rate_per_day,
                        mean_gen_time, sd_gen_time,
                        n_gen_away = curr_n_gen_away) %>% 
    mutate(g_gen = curr_n_gen_away)
}))

plt_relationship_proba_mutation <- df_proba %>% 
  group_by(g_gen) %>% arrange(n_mutations_away) %>% mutate(cum_proba = cumsum(proba)) %>% 
  filter(n_mutations_away <= 10) %>% 
  ggplot(aes(x = as.factor(n_mutations_away),
             y = cum_proba, group = g_gen, colour = as.factor(g_gen))) +
  geom_line(alpha = 0.8) +
  geom_point(alpha = 0.8) +
  scale_x_discrete(name = 'Genetic distance (in # of mutations)',
                   expand = expansion(mult = c(0.05, 0.05))) +
  scale_y_continuous(name = 'Probability', limits = c(0., 1.),
                     expand = expansion(mult = c(0., 0.05))) +
  scale_colour_manual(values = colorRampPalette(colors = brewer.pal(9, 'BuPu')[-(1:2)] %>% rev())(6),
                      name = 'Number of\ngenerations') +
  theme_classic() +
  theme(legend.position = c(0.8, 0.4),
        legend.background = element_blank(),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  guides(colour = guide_legend(ncol = 1))

plot(plt_relationship_proba_mutation)
