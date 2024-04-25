library(tidyverse)

## Load adjacency matrix
df_adj_county <- readRDS('../data/maps/df_adj_county.rds')
df_adj_zcta <- readRDS('../data/maps/df_adj_zcta.rds') %>% 
  as_tibble() %>% 
  mutate(zcta_1 = as.character(zcta_1), zcta_2 = as.character(zcta_2))

## Load characteristics of WA counties
df_char_counties <- read.csv('../data/maps/county_wa.csv') %>% as_tibble()
df_char_zctas <- read_csv('../data/maps/relationship_zcta_county_WA.csv', col_types = 'cc') %>% 
  left_join(df_char_counties %>% select(county, is_west), by = 'county')

## Load the relative risk of observing identical sequences between two counties
df_RR_counties <- readRDS('../results/RR_county/df_RR_county_0_mut_away.rds') %>% 
  left_join(df_adj_county, by = c('group_1' = 'county_1', 'group_2' = 'county_2')) %>% 
  mutate(is_same_county = (group_1 == group_2)) %>% 
  left_join(df_char_counties %>% select(county, is_west), by = c('group_1' = 'county')) %>% 
  rename(is_west_1 = is_west) %>% 
  left_join(df_char_counties %>% select(county, is_west), by = c('group_2' = 'county')) %>% 
  rename(is_west_2 = is_west) %>% 
  mutate(region = case_when(is_west_1 + is_west_2 == 2 ~ 'West - West',
                            is_west_1 + is_west_2 == 1 ~ 'West - East',
                            is_west_1 + is_west_2 == 0 ~ 'East - East'),
         region = factor(region, levels = c('West - West', 'West - East', 'East - East'))) %>% 
  mutate(is_adjacent_same_county = case_when(is_same_county == T ~ 'Same county',
                                             is_adjacent == T ~ 'Adjacent counties',
                                             TRUE ~ 'Non adjacent counties'),
         is_adjacent_same_county = factor(is_adjacent_same_county, levels = c('Non adjacent counties', 'Adjacent counties', 'Same county')))

df_RR_zcta <- readRDS('../results/RR_zcta/df_RR_zcta_0_mut_away.rds') %>%
  rename(group_1 = zcta_1, group_2 = zcta_2) %>% as_tibble() %>% 
  mutate(is_same_zcta = (group_1 == group_2)) %>% 
  left_join(df_adj_zcta, by = c('group_1' = 'zcta_1', 'group_2' = 'zcta_2')) %>% 
  left_join(df_char_zctas %>% select(county, zcta, is_west), by = c('group_1' = 'zcta')) %>% 
  rename(county_1 = county, is_west_1 = is_west) %>% 
  left_join(df_char_zctas %>% select(county, zcta, is_west), by = c('group_2' = 'zcta')) %>% 
  rename(county_2 = county, is_west_2 = is_west) %>% 
  mutate(is_west_1 = as.numeric(is_west_1), is_west_2 = as.numeric(is_west_2)) %>% 
  mutate(region = case_when(is_west_1 + is_west_2 == 2 ~ 'West - West',
                            is_west_1 + is_west_2 == 1 ~ 'West - East',
                            is_west_1 + is_west_2 == 0 ~ 'East - East'),
         region = factor(region, levels = c('West - West', 'West - East', 'East - East')),
         is_adjacent_same_zcta = case_when(is_same_zcta == T ~ 'Same ZCTA',
                                           is_adjacent == T ~ 'Adjacent ZCTAs',
                                           TRUE ~ 'Non adjacent ZCTAs'),
         is_adjacent_same_zcta = factor(is_adjacent_same_zcta, levels = c('Non adjacent ZCTAs', 'Adjacent ZCTAs', 'Same ZCTA'))) 

## At the county level
wilcox.test(x = df_RR_counties[df_RR_counties$region == 'West - East' & df_RR_counties$is_adjacent_same_county == 'Adjacent counties'& (df_RR_counties$group_1 > df_RR_counties$group_2), ]$RR,
            y = df_RR_counties[df_RR_counties$region == 'West - East' & df_RR_counties$is_adjacent_same_county == 'Non adjacent counties'& (df_RR_counties$group_1 > df_RR_counties$group_2), ]$RR)

wilcox.test(x = df_RR_counties[df_RR_counties$region == 'West - West' & df_RR_counties$is_adjacent_same_county == 'Adjacent counties'& (df_RR_counties$group_1 > df_RR_counties$group_2), ]$RR,
            y = df_RR_counties[df_RR_counties$region == 'West - West' & df_RR_counties$is_adjacent_same_county == 'Non adjacent counties'& (df_RR_counties$group_1 > df_RR_counties$group_2), ]$RR)

wilcox.test(x = df_RR_counties[df_RR_counties$region == 'West - West' & df_RR_counties$is_adjacent_same_county == 'Adjacent counties'& (df_RR_counties$group_1 > df_RR_counties$group_2), ]$RR,
            y = df_RR_counties[df_RR_counties$region == 'West - West' & df_RR_counties$is_adjacent_same_county == 'Same county', ]$RR)

wilcox.test(x = df_RR_counties[df_RR_counties$region == 'East - East' & df_RR_counties$is_adjacent_same_county == 'Adjacent counties'& (df_RR_counties$group_1 > df_RR_counties$group_2), ]$RR,
            y = df_RR_counties[df_RR_counties$region == 'East - East' & df_RR_counties$is_adjacent_same_county == 'Non adjacent counties'& (df_RR_counties$group_1 > df_RR_counties$group_2), ]$RR)

wilcox.test(x = df_RR_counties[df_RR_counties$region == 'East - East' & df_RR_counties$is_adjacent_same_county == 'Adjacent counties'& (df_RR_counties$group_1 > df_RR_counties$group_2), ]$RR,
            y = df_RR_counties[df_RR_counties$region == 'East - East' & df_RR_counties$is_adjacent_same_county == 'Same county', ]$RR)

# Check whether we still have significance when removing ties
vec_adjacent_EE <- df_RR_counties[df_RR_counties$region == 'East - East' & df_RR_counties$is_adjacent_same_county == 'Adjacent counties'& (df_RR_counties$group_1 > df_RR_counties$group_2), ]$RR
vec_same_county_EE <- df_RR_counties[df_RR_counties$region == 'East - East' & df_RR_counties$is_adjacent_same_county == 'Same county', ]$RR
wilcox.test(vec_adjacent_EE[vec_adjacent_EE > 0.], vec_same_county_EE[vec_same_county_EE > 0.])

## Plot RR by adjacency status and East / West region
col_west <- 'dodgerblue3'
col_east <- 'coral1'
col_east_west <- 'darkslateblue'

yaxis_zero_value <- min(df_RR_counties$RR[df_RR_counties$RR > 0.]) * 0.4

df_for_boxplot <- df_RR_counties %>% 
  group_by(is_adjacent_same_county, region) %>% 
  summarise(y05 = quantile(RR, 0.05),
            y25 = quantile(RR, 0.25),
            y50 = quantile(RR, 0.5),
            y75 = quantile(RR, 0.75),
            y95 = quantile(RR, 0.95)) %>% 
  mutate(y05_plot = ifelse(y05 == 0., yaxis_zero_value, y05),
         y25_plot = ifelse(y25 == 0., yaxis_zero_value, y25),
         y50_plot = ifelse(y50 == 0., yaxis_zero_value, y50),
         y75_plot = ifelse(y75 == 0., yaxis_zero_value, y75),
         y95_plot = ifelse(y95 == 0., yaxis_zero_value, y95))



df_signif <- bind_rows(
  tibble(x = c(0.875, 1.875), 
         xend = c(2.125, 3.125),
         y = c(1e3, 5e3),
         region = c('West - West', 'West - West') ,
         label = c("***", " ** ")),
  tibble(x = c(0.875, 1.875), 
         xend = c(2.125, 3.125),
         y = c(1e3, 5e3),
         region = c('East - East', 'East - East') ,
         label = c("***", " *** ")),
  tibble(x = c(0.875), 
         xend = c(2.125),
         y = c(1e3),
         region = c('West - East') ,
         label = c('ns'))
)

plt_RR_adj_east_west <- df_RR_counties  %>%
  mutate(RR = ifelse(RR == 0., yaxis_zero_value, RR)) %>% 
  ggplot(aes(x = is_adjacent_same_county,
             group = region, fill = region, color = region)) +
  geom_jitter(aes(y = RR), alpha = 0.25, height = 0., width = 0.3, color = 'darkgrey')  +
  geom_boxplot(data = df_for_boxplot %>% mutate(n_pairs = 1.), 
               aes(ymin = y05, lower = y25, middle = y50, upper = y75, ymax = y95, 
                   group = interaction(is_adjacent_same_county, region)),
               stat = "identity", fill = NA, width = 0.7) +
  geom_signif(stat = "identity",
              data = df_signif %>% mutate(n_pairs = 1.),
              aes(x = x, xend = xend,  y = y, yend = y, annotation = label)) +
  scale_x_discrete(name = '', 
                   breaks = c('Non adjacent counties', 'Adjacent counties', 'Same county'),
                   labels = c('Non adjacent', 'Adjacent', 'Within county')) +
  scale_y_continuous(trans = 'log', name = expression(RR["identical sequences"]),
                     expand = expansion(mult = c(0.05, 0.), add = c(0.1, 0.1)),
                     breaks = c(yaxis_zero_value, 0.1, 1, 10, 100, 1e3, 1e4),
                     labels = c(0, expression(10^{-1}), 
                                expression(10^{0}), expression(10^{1}), expression(10^{2}),
                                expression(10^{3}), expression(10^{4}))) +
  scale_colour_manual(name = '', 
                      values = c(col_west, col_east_west, col_east)) +
  scale_fill_manual(name = '', 
                    values = c(col_west, col_east_west, col_east)) +
  coord_flip() +
  facet_grid(region ~ (n_pairs != 0),  scales = 'free', space = "free") +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        axis.text = element_text(size = 12),
        strip.background.x = element_blank(),
        strip.text.x = element_blank(),
        strip.background.y = element_rect(fill = 'gray22'),
        strip.text.y = element_text(colour = 'white', size = 11),
        legend.position = 'none') 

#pdf('../plots/figure_space/RR_adjacent_west_east.pdf', height = 2.8, width = 5.5)
plot(plt_RR_adj_east_west)
#dev.off()


## Same thing but at the ZCTA level
df_RR_zcta <- df_RR_zcta %>% filter(RR > 0.)
wilcox.test(x = df_RR_zcta[df_RR_zcta$region == 'West - East' & df_RR_zcta$is_adjacent_same_zcta == 'Adjacent ZCTAs'& (df_RR_zcta$group_1 > df_RR_zcta$group_2), ]$RR,
            y = df_RR_zcta[df_RR_zcta$region == 'West - East' & df_RR_zcta$is_adjacent_same_zcta == 'Non adjacent ZCTAs'& (df_RR_zcta$group_1 > df_RR_zcta$group_2), ]$RR)

wilcox.test(x = df_RR_zcta[df_RR_zcta$region == 'West - West' & df_RR_zcta$is_adjacent_same_zcta == 'Adjacent ZCTAs'& (df_RR_zcta$group_1 > df_RR_zcta$group_2), ]$RR,
            y = df_RR_zcta[df_RR_zcta$region == 'West - West' & df_RR_zcta$is_adjacent_same_zcta == 'Non adjacent ZCTAs'& (df_RR_zcta$group_1 > df_RR_zcta$group_2), ]$RR)

wilcox.test(x = df_RR_zcta[df_RR_zcta$region == 'West - West' & df_RR_zcta$is_adjacent_same_zcta == 'Adjacent ZCTAs'& (df_RR_zcta$group_1 > df_RR_zcta$group_2), ]$RR,
            y = df_RR_zcta[df_RR_zcta$region == 'West - West' & df_RR_zcta$is_adjacent_same_zcta == 'Same ZCTA', ]$RR)

wilcox.test(x = df_RR_zcta[df_RR_zcta$region == 'East - East' & df_RR_zcta$is_adjacent_same_zcta == 'Adjacent ZCTAs'& (df_RR_zcta$group_1 > df_RR_zcta$group_2), ]$RR,
            y = df_RR_zcta[df_RR_zcta$region == 'East - East' & df_RR_zcta$is_adjacent_same_zcta == 'Non adjacent ZCTAs'& (df_RR_zcta$group_1 > df_RR_zcta$group_2), ]$RR)

wilcox.test(x = df_RR_zcta[df_RR_zcta$region == 'East - East' & df_RR_zcta$is_adjacent_same_zcta == 'Adjacent ZCTAs'& (df_RR_zcta$group_1 > df_RR_zcta$group_2), ]$RR,
            y = df_RR_zcta[df_RR_zcta$region == 'East - East' & df_RR_zcta$is_adjacent_same_zcta == 'Same ZCTA', ]$RR)

