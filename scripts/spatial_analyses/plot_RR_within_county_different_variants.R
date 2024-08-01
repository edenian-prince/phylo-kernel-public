## This script evaluates how the RR of observing pairs of sequences within the same county varies
## as a function of genetic distance between infecting viruses for Delta and Omicron during the 6th wave
## of the pandemic in WA.

library(dplyr)
library(ggplot2)
library(viridis)

vec_variants <- c('Delta', 'Omicron')    
vec_n_mutations <- 0:4
## Load relative risk of observing sequences at a given genetic distance within
## the same county by period of interest
df_RR_counties_by_variant <- Reduce('bind_rows', lapply(vec_n_mutations, FUN = function(i_mut){
  Reduce('bind_rows', lapply(vec_variants, FUN = function(name_var){
    readRDS(paste0('../results/RR_county_by_variant/df_RR_county_', i_mut, '_mut_away_period_1_', name_var, '.rds')) %>% 
      mutate(variant = name_var)
  }))
})) %>% ungroup() %>% 
  filter(group_1 == group_2) 

df_RR_counties_by_variant %>% 
  filter(variant == 'Delta') %>% group_by(n_mutations) %>% 
  summarise(n_pairs = sum(n_pairs))

## Plot the relative risk of observing sequences within the same county
## as a function of the genetic distance separating them across periods
yaxis_zero_value <- min(df_RR_counties_by_variant$RR[df_RR_counties_by_variant$RR > 0.]) * 0.05
df_median <- df_RR_counties_by_variant %>% 
  group_by(variant, n_mutations) %>% 
  summarise(median_RR = median(RR),
            lower_IQR = quantile(RR, 0.75),
            upper_IQR = quantile(RR, 0.25))

plt_RR_within_county_by_variant <- df_median %>% 
  ggplot(aes(x = n_mutations, group = variant, colour = variant)) +
  geom_point(aes(y = median_RR), position = position_dodge(0.4)) +
  geom_linerange(aes(ymin = lower_IQR, ymax = upper_IQR), 
                 position = position_dodge(0.4)) +
  scale_x_continuous(name = 'Genetic distance\n(number of mutations)\n\n', breaks = 0:10) +
  scale_y_continuous(name = expression(RR["within county"]), trans = 'log',
                     breaks = c(yaxis_zero_value, 0.1, 1.0, 10., 100., 1e3, 1e4, 1e5, 1e6),
                     labels = c(0, expression(10^{-1}), expression(10^{0}),
                                expression(10^{1}), expression(10^{2}),
                                expression(10^{3}), expression(10^{4}),
                                expression(10^{5}), expression(10^{6}))) +
  scale_color_manual(breaks = c('Delta', 'Omicron'), values = c('darkslateblue', 'deeppink3'), 
                     name = 'Variant') +
  theme_classic() +
  guides(color = guide_legend(override.aes = list(linetype = 0),
                              ncol = 1)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        strip.background = element_blank(),
        strip.text = element_blank())


plot(plt_RR_within_county_by_variant)

# pdf('../plots/figure_RR_within_county/RR_within_county_by_variant.pdf',
#     height = 3., width = 4.)
plot(plt_RR_within_county_by_variant)
# dev.off()
# png('../plots/figure_RR_within_county/RR_within_county_by_variant.png',
#     height = 3., width = 4., units = 'in', res = 350)
# plot(plt_RR_within_county_by_variant)
# dev.off()
