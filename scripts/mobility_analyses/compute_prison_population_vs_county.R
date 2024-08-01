## This script computes the ratio the population of each male WA prison
##  and the the population of the county that prison is in.


library(tidyverse)

## Load characteristics of WA counties
df_char_counties <- read.csv('../data/maps/county_wa.csv') %>% as_tibble() %>% 
  select(county, pop_size_2020)

## Load characteristics of WA ZCTAs
df_char_zctas <- read.csv('../data/maps/zcta_wa.csv') %>% as_tibble() %>% 
  select(zcta, pop_size)

## Load male prison characteristics
df_char_prisons <- read_csv('../data/maps/wa_prisons_characteristics.csv') %>% 
  filter(population_gender == 'male')

df_char_prisons %>% 
  left_join(df_char_counties) %>% 
  mutate(ratio_capacity_county_pop = capacity / pop_size_2020) %>% 
  select(facility_name, county, capacity, pop_size_2020, ratio_capacity_county_pop) %>% 
  arrange(-ratio_capacity_county_pop)
