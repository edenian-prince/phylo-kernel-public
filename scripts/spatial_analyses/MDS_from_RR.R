## This script reproduces the multidimensional scaling analysis to
## visualize similarity between counties from pairs of identical sequences.

library(tidyverse)
library(vegan)
library(ggrepel)

# Load relative risk of observing identical sequences between two counties
df_RR_counties <- read_csv('../../results/RR_county/df_RR_county_0_mut_away.csv') %>% 
  filter(n_pairs > 1)

# Load characteristics of WA counties
county_wa_regions <- read.csv('../../data/maps/county_wa.csv') %>% as_tibble()

# Vector of counties to be removed from the analysis (see methods)
vec_counties_to_remove <- c('Wahkiakum County', 'Garfield County', 'Skamania County', 
                            'Columbia County', 'Ferry County', 'Lincoln County', 
                            'San Juan County', 'Asotin County', 'Pend Oreille County', 
                            'Klickitat County')

# Function to get matrix of relative risk of observing identical sequences between counties
get_mat_from_df_RR <- function(df_RR_identical, vec_counties_to_remove){
  curr_df_RR <- df_RR_identical %>% 
    filter(! group_1 %in% vec_counties_to_remove,
           ! group_2 %in% vec_counties_to_remove)
  
  vec_counties <- unique(curr_df_RR$group_1)
  n_counties <- length(vec_counties)
  mat_RR <- matrix(0, ncol = n_counties, nrow = n_counties)
  for(i_county in 1:n_counties){
    for(j_county in 1:n_counties){
      mat_RR[i_county, j_county] <- curr_df_RR %>% ungroup() %>% 
        filter(group_1 == vec_counties[i_county], group_2 == vec_counties[j_county]) %>% 
        select(RR) %>% unlist() %>% as.numeric()
    }
  }
  colnames(mat_RR) <- rownames(mat_RR) <- vec_counties
  return(mat_RR)
}
mat_RR_identical_sequences <- get_mat_from_df_RR(df_RR_counties, vec_counties_to_remove)

# Generate matrix of distance between counties
mat_RR_MDS <- exp(-mat_RR_identical_sequences)
diag(mat_RR_MDS) <- rep(0., nrow(mat_RR_MDS))
meta_MDS <- metaMDS(comm = as.dist(mat_RR_MDS), k = 2)

# Plotting the NMDS by Eastern / Western WA
data_scores <- as.data.frame(scores(meta_MDS))
data_scores <- data_scores %>% mutate(county = rownames(data_scores)) %>% 
  left_join(county_wa_regions, by = 'county')

col_west <- 'dodgerblue3'
col_east <- 'coral1'

plt_map_NMDS_west <- data_scores %>% 
  ggplot(aes(y = -NMDS2, x = -NMDS1, label = county_short, colour = as.factor(is_west))) +
  geom_point() +
  geom_text_repel(size = 3) +
  theme_classic() +
  scale_y_continuous(name = '- Dimension 2') + scale_x_continuous(name = '- Dimension 1') +
  scale_colour_manual(name = 'Region', values = c(col_west, col_east),
                      labels = c('West', 'East'), breaks = c(T, F)) +
  theme(legend.position = 'none') +
  coord_fixed()

set.seed(1)
plot(plt_map_NMDS_west)

# set.seed(1)
# pdf('../plots/figure_space/MDS_west.pdf', height = 3.4, width = 4.5)
# plot(plt_map_NMDS_west)
# dev.off()
# set.seed(1)
# png('../plots/figure_space/MDS_west.png', height = 3.4, width = 4.5, res = 350, units = 'in')
# plot(plt_map_NMDS_west)
# dev.off()
