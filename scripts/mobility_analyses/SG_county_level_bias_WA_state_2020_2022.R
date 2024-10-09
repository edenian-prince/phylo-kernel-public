########################################################################################################
## Exploring county-level bias in SafeGraph data for Washington state, 2020 - 2022
########################################################################################################
# sessionInfo()
# R version 4.4.0 (2024-04-24)
# Platform: x86_64-apple-darwin20
# Running under: macOS Sonoma 14.5

library(readr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(ggpubr)

########################################################################################################
## Load dataset
########################################################################################################
# County-level bias in SafeGraph dataset for WA state, plus county-specific U.S. Census metadata, 2020 - 2022

county_visits_and_pop <- read_csv("wa_county_safegraph_panel_vs_pop_size_2020_2022.csv",
  col_types = c("n", "c", "c", "c", "n", "n", "n", "n", "n", "n", "n", "n")
)

head(county_visits_and_pop)

## Key
# year: calendar year (2020, 2021, or 2022)
# county: county name
# fips: 5-digit FIPS code
# county_fips: county 3-digit FIPS code
# POPESTIMATE2020: county population size (2020 U.S. Census)
# POPPCT_RUR: percentage of county populations living in rural areas (2020 U.S. Census)
# prop_state: proportion of WA state population residing in each county (POPESTIMATE2020/sum(POPESTIMATE2020))
# mean_weekly_total: weekly number of devices tracked by SG in each county, averaged by calendar year (pre-calculated)
# prop_county: # devices tracked by SG in each county divided by county census population size (mean_weekly_total/POPESTIMATE2020)
# prop_SG: # devices tracked by SG in each county divided by # devices tracked in WA state (mean_weekly_total/sum(mean_weekly_total))
# bias: prop_SG - prop_state ("observed proportion" minus "expected proportion")

## Range for proportion of county population captured by SafeGraph panel
county_visits_and_pop %>%
  pull(prop_county) %>%
  range() %>%
  round(digits = 3)
# 0.019 0.049

########################################################################################################
## Figure S28. Comparisons between county census population sizes and SafeGraph panel sizes.
########################################################################################################

## A. County-level census population size vs SG panel size

a <- ggplot(county_visits_and_pop) +
  geom_point(aes(x = POPESTIMATE2020, y = mean_weekly_total, fill = as.factor(fips)), pch = 21, size = 3) +
  scale_y_continuous(transform = "log10") +
  scale_x_continuous(transform = "log10") +
  facet_wrap(~year) +
  theme_bw(base_size = 16) +
  xlab("County Census Population Size") +
  ylab("SG County Panel Size") +
  stat_cor(method = "spearman", cor.coef.name = "rho", p.digits = 1, aes(x = POPESTIMATE2020, y = mean_weekly_total)) +
  theme(legend.position = "none")
a

## B. County-level "Expected Proportion" vs "Observed Proportion"
## Expected proportion: Census county population size/Census state population size
## Observed proportion: SG county panel size/SG state panel size

b <- ggplot(county_visits_and_pop) +
  geom_point(aes(x = prop_state, y = prop_SG, fill = as.factor(fips)), pch = 21, size = 3) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") + # 1-to-1 line
  facet_wrap(~year) +
  theme_bw(base_size = 16) +
  xlab("County Expected Proportion (Census)") +
  ylab("County Observed Proportion (SG)") +
  stat_cor(method = "spearman", cor.coef.name = "rho", p.digits = 1, aes(x = prop_state / 100, y = prop_SG / 100)) +
  theme(legend.position = "none")
b

comb <- plot_grid(a, b, nrow = 2, align = "hv", labels = "AUTO")
comb

## Figure S28
# save_plot(comb,filename="county_census_pop_size_vs_SG_panel_size_WA_state.png",base_width = 12, base_height = 8)
save_plot(comb, filename = "county_census_pop_size_vs_SG_panel_size_WA_state.pdf", base_width = 12, base_height = 8)

########################################################################################################
## Explore annual estimates of bias in SafeGraph panel across WA counties
########################################################################################################
## Reference: "Quantifying Sampling Bias in SafeGraph Patterns" tutorial
## Link: https://colab.research.google.com/drive/1u15afRytJMsizySFqA2EPlXSh3KTmNTQ#sandboxMode=true&scrollTo=xsNNli6GTN6s

## We calculate county-level bias as:
## The proportion of SafeGraph's panel of devices in WA state residing in a particular county minus
## the proportion of WA state's population residing in that county based on 2020 U.S. Census data.
## bias = (SG county panel size/SG state panel size) - (Census county pop size/Census state pop size)

county_visits_and_pop %>%
  filter(year == 2020) %>%
  arrange(-bias) %>%
  print(n = 40)

county_visits_and_pop %>%
  filter(year == 2021) %>%
  arrange(-bias) %>%
  print(n = 40)

county_visits_and_pop %>%
  filter(year == 2022) %>%
  arrange(-bias) %>%
  print(n = 40)

### SG bias across all years, 2020 - 2022
county_visits_and_pop %>%
  pull(bias) %>%
  range() %>%
  round(digits = 3)
# -2.232  1.719

### SG bias for 3 counties that are the largest positive outliers (i.e., overrepresented in SG dataset)
county_visits_and_pop %>%
  filter(county %in% c("Clark County", "Pierce County", "Spokane County")) %>%
  pull(bias) %>%
  range() %>%
  round(digits = 3)
# 1.055 1.719

## SG bias for King County (largest negative outlier; i.e., underrepresented in SG dataset)
county_visits_and_pop %>%
  filter(county %in% c("King County")) %>%
  pull(bias) %>%
  range() %>%
  round(digits = 3)
# -2.232 -1.616

########################################################################################################
## Figure S29. County-level bias of SafeGraph data in Washington state vs.
## A. County census population size
## B. SafeGraph panel sizes in individual counties relative to WA state (“county observed proportion”)
## C. census urban-rural classification ("county percent rural")
########################################################################################################

# A. County census population size vs SG bias
a <- ggplot(county_visits_and_pop) +
  geom_point(aes(x = POPESTIMATE2020, y = bias, fill = as.factor(fips)), pch = 21, size = 3) +
  scale_x_continuous(transform = "log10") +
  geom_hline(yintercept = 0, lty = "dashed") +
  facet_wrap(~year) +
  theme_bw(base_size = 16) +
  xlab("County Census Population Size") +
  ylab("Bias") +
  stat_cor(method = "spearman", cor.coef.name = "rho", p.digits = 1, aes(x = POPPCT_RUR, y = bias)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-2.5, 2.5)) +
  theme(legend.position = "none")
a

# B. County SG "observed proportion" (SG county panel size/SG state panel size) vs SG bias
b <- ggplot(county_visits_and_pop) +
  geom_point(aes(x = prop_SG, y = bias, fill = as.factor(fips)), pch = 21, size = 3) +
  geom_hline(yintercept = 0, lty = "dashed") +
  facet_wrap(~year) +
  theme_bw(base_size = 16) +
  xlab("County Observed Proportion (SG)") +
  ylab("Bias") +
  stat_cor(method = "spearman", cor.coef.name = "rho", p.digits = 1, aes(x = prop_SG, y = bias)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-2.5, 2.5)) +
  theme(legend.position = "none")
b

# C. County percent rural vs SG bias
c <- ggplot(county_visits_and_pop) +
  geom_point(aes(x = POPPCT_RUR, y = bias, fill = as.factor(fips)), pch = 21, size = 3) +
  geom_hline(yintercept = 0, lty = "dashed") +
  facet_wrap(~year) +
  theme_bw(base_size = 16) +
  xlab("County Percent Rural") +
  ylab("Bias") +
  stat_cor(method = "spearman", cor.coef.name = "rho", p.digits = 1, aes(x = POPPCT_RUR, y = bias)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-2.5, 2.5)) +
  theme(legend.position = "none")
c

comb <- plot_grid(a, b, c, align = "hv", labels = "AUTO", nrow = 3)
comb

## Figure S29
# save_plot(comb,filename="county_bias_WA_state.png",base_width = 12, base_height = 12)
save_plot(comb, filename = "county_bias_WA_state.pdf", base_width = 12, base_height = 12)
