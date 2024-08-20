## This script evaluates how the RR of observing pairs of sequences within the same county varies
## as a function of genetic distance between infecting viruses

library(dplyr)
library(ggplot2)
library(viridis)

## Load relative risk of observing sequences at a given genetic distance within
## the same county
df_RR_counties <- Reduce('bind_rows', lapply(0:10, FUN = function(curr_n_mut){
  read_csv(paste0('../../results/RR_county/df_RR_county_', curr_n_mut, '_mut_away.csv'))
})) %>% 
  filter(group_1 == group_2)

## Plot the relative risk of observing sequences within the same county
## as a function of the genetic distance separating them
df_median <- df_RR_counties %>% 
  group_by(n_mutations) %>% 
  summarise(median_RR = median(RR), lower_IQR = quantile(RR, 0.25), upper_IQR = quantile(RR, 0.75))

yaxis_zero_value <- min(df_RR_counties$RR[df_RR_counties$RR > 0.]) * 0.15

plt_RR_county_func_genetic_distance <- df_RR_counties %>% 
  mutate(RR = ifelse(RR == 0., yaxis_zero_value, RR)) %>% 
  ggplot(aes(x = n_mutations, y = RR)) +
  geom_jitter(alpha = 0.1, width = 0.2, height = 0.) +
  geom_point(data = df_median %>% mutate(n_pairs = 1.), color = 'orange2', aes(y = median_RR), shape = 17) +
  geom_line(data = df_median %>% mutate(n_pairs = 1.), color = 'orange2', aes(y = median_RR)) +
  scale_x_continuous(name = 'Genetic distance\n(number of mutations)\n\n', breaks = 0:10) +
  scale_y_continuous(name = expression(RR["within county"]), trans = 'log',
                     breaks = c(yaxis_zero_value, 0.1, 1.0, 10., 100., 1e3, 1e4),
                     labels = c(0, expression(10^{-1}), expression(10^{0}),
                                expression(10^{1}), expression(10^{2}),
                                expression(10^{3}), expression(10^{4})
                     ),
                     expand = expansion(add = c(0.2, 0.2), mult = c(0.14, 0.))) +
  scale_color_manual(name = 'Genetic distance\n(number of mutations)', 
                     values = viridis(n = 12, direction = -1, option = 'viridis')[-1]) +
  theme_classic() +
  guides(color = guide_legend(override.aes = list(linetype = 0),
                              ncol = 1)) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        strip.background = element_blank(),
        strip.text = element_blank()
        ) +
  facet_grid((n_pairs == 0) ~ ., scales = 'free', space = 'free_y')


# pdf('../plots/figure_RR_within_county/RR_within_county.pdf',
#     height = 4., width = 3.5)
plot(plt_RR_county_func_genetic_distance)
#dev.off()
# png('../plots/figure_RR_within_county/RR_within_county.png',
#     height = 4., width = 3.5, res = 350, units = 'in')
# plot(plt_RR_county_func_genetic_distance)
# dev.off()
