## This script evaluates how the RR of observing pairs of sequences within the same county varies
## as a function of genetic distance between infecting viruses across pandemic waves in WA

library(dplyr)
library(ggplot2)
library(viridis)

vec_periods <- 1:4
label_periods <- c('Wave 4\n(Mar 21 - Jun 21)',
                   'Wave 5\n(Jul 21 - Nov 21)',
                   'Wave 6\n(Dec 21 - Feb 22)',
                   'Wave 7\n(Mar 22 - Aug 22)')           
  
vec_n_mutations <- 0:10
## Load relative risk of observing sequences at a given genetic distance within
## the same county by period of interest
df_RR_counties_by_period <- Reduce('bind_rows', lapply(vec_n_mutations, FUN = function(i_mut){
  Reduce('bind_rows', lapply(vec_periods, FUN = function(i_period){
    readRDS(paste0('../results/RR_county_by_period/df_RR_county_', i_mut, '_mut_away_period_', i_period, '.rds')) %>% 
      mutate(i_period = i_period)
    
    
  }))
})) %>% ungroup() %>% 
  filter(group_1 == group_2)


## Plot the relative risk of observing sequences within the same county
## as a function of the genetic distance separating them across periods
df_median <- df_RR_counties_by_period %>% 
  group_by(i_period, n_mutations) %>% 
  summarise(median_RR = median(RR))

yaxis_zero_value <- min(df_RR_counties_by_period$RR[df_RR_counties_by_period$RR > 0.]) * 0.15

plt_RR_within_county_by_period <- df_RR_counties_by_period %>% 
  mutate(RR = ifelse(RR == 0., yaxis_zero_value, RR)) %>% 
  ggplot(aes(x = as.factor(i_period), y = RR, colour = as.factor(n_mutations))) +
  geom_jitter(alpha = 0.1, width = 0.2, height = 0.) +
  geom_point(data = df_median %>% mutate(n_pairs = 1.), aes(y = median_RR), shape = 17) +
  geom_line(data = df_median %>% mutate(n_pairs = 1.), aes(y = median_RR, group = n_mutations)) +
  scale_x_discrete(name = '', breaks = vec_periods, labels = label_periods) +
  scale_y_continuous(name = expression(RR["within county"]), trans = 'log',
                     breaks = c(yaxis_zero_value, 0.1, 1.0, 10., 100., 1e3, 1e4, 1e5, 1e6),
                     labels = c(0, expression(10^{-1}), expression(10^{0}),
                                expression(10^{1}), expression(10^{2}),
                                expression(10^{3}), expression(10^{4}), 
                                expression(10^{5}), expression(10^{6})
                     ),
                     expand = expansion(add = c(0.2, 0.2), mult = c(0.1, 0.))) +
  scale_color_manual(name = 'Genetic distance\n(number of mutations)', 
                     values = viridis(n = 12, direction = -1, option = 'viridis')[-1]) +
  theme_classic() +
  guides(color = guide_legend(override.aes = list(linetype = 0),
                              ncol = 1)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1.)
  ) +
  facet_grid((n_pairs == 0) ~ ., scales = 'free', space = 'free_y')


# pdf('../plots/figure_RR_within_county/RR_within_county_by_period.pdf',
#     height = 5., width = 6.)
plot(plt_RR_within_county_by_period)
#dev.off()
# png('../plots/figure_RR_within_county/RR_within_county_by_period.png',
#     height = 5., width = 6., res = 350, units = 'in')
# plot(plt_RR_within_county_by_period)
# dev.off()
