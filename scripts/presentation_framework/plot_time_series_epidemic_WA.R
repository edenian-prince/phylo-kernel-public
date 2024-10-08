## This script reproduces Figure S32 with the epidemic curves of cases
## in WA as a function of time and the definition of our 4 sub-periods (waves)
# Case data were obtained from: https://doh.wa.gov/data-and-statistical-reports/diseases-and-chronic-conditions/communicable-disease-surveillance-data/respiratory-illness-data-dashboard#downloads

library(tidyverse)

# Load epidemic curve
epicurve_WA <- read_csv('../../data/case_data/epicurve_WA.csv')

# Study period
date_beginning_study_period <- as.Date('2021-03-01')
date_end_study_period <- as.Date('2022-12-31')

# Define epidemic waves periods used in the study
vec_beginning_period <- as.Date(c('2021-03-01', '2021-07-01', '2021-12-01', '2022-03-01'))
vec_end_period <- as.Date(c('2021-07-01', '2021-12-01', '2022-03-01', '2022-09-01')) - 1

df_char_waves <- tibble(beginning_wave = vec_beginning_period,
                        end_wave = vec_end_period,
                        name_wave = paste0('Wave ', 4:7))

# Plot time series of cases
plt_wave_definition <- epicurve_WA %>% filter(County == 'Statewide') %>% 
  rename(date = `Earliest Specimen Collection Date`, total_cases = `Total Cases`) %>% 
  filter(date >= date_beginning_study_period, date <= date_end_study_period) %>% 
  ggplot() +
  geom_rect(data = df_char_waves, 
            aes(ymin = 0., ymax = 35, xmin = beginning_wave, xmax = end_wave, fill = name_wave), 
            alpha = 0.25) +
  geom_text(data = df_char_waves,
            aes(x = beginning_wave + (end_wave - beginning_wave)/2., y = 32, 
                label = name_wave, colour = name_wave)) +
  geom_bar(aes(x = date, y = total_cases/1000),  stat = 'identity', fill = 'black') +
  scale_x_date(date_breaks = '2 months', minor_breaks = '1 month',
               date_labels = "%b\n%y", name = 'Date', expand = expansion(mult = c(0.05, 0.05))) +
  scale_y_continuous(name = 'Daily cases (x 1,000)') +
  scale_colour_manual(values = brewer.pal(12, 'Paired')[c(2, 4, 6, 8)], name = '') +
  scale_fill_manual(values = brewer.pal(12, 'Paired')[c(2, 4, 6, 8)], name = '') +
  theme_bw() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
  )

#pdf('../plots/waves_definition.pdf', height = 2.5, width = 6.5)
plot(plt_wave_definition)
#dev.off()


