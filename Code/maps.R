#+ include = FALSE, warnings = FALSE, messages = FALSE
NORSData <- readxl::read_excel('~/Downloads/NationalOutbreakPublicDataTool.xlsx')

library(tidyverse)
library(readxl)

pre_2010_pop <- read_excel('DataRaw/st-est00int-01.xls', 
                                   skip = 3)

post_2010_pop <- read_csv("DataRaw/nst-est2018-alldata.csv", 
                          col_types = cols_only(NAME = col_character(), 
                                                POPESTIMATE2010 = col_double(), 
                                                POPESTIMATE2011 = col_double(), 
                                                POPESTIMATE2012 = col_double(), 
                                                POPESTIMATE2013 = col_double(), 
                                                POPESTIMATE2014 = col_double(), 
                                                POPESTIMATE2015 = col_double(), 
                                                POPESTIMATE2016 = col_double(), 
                                                POPESTIMATE2017 = col_double(), 
                                                POPESTIMATE2018 = col_double()
                                                )
                          )

pre_2010_pop %>% 
  rename(NAME = `...1`) %>% 
  rename_at(vars(-NAME), ~paste0('POPESTIMATE', .)) %>% 
  mutate(NAME = str_remove(NAME, '\\.')) %>% 
  left_join(post_2010_pop) %>% 
  pivot_longer(-NAME, names_to = 'year', values_to = 'population') %>% 
  mutate(year = as.numeric(str_remove(year, 'POPESTIMATE'))) %>% 
  rename(state = NAME) -> population_data

NORSData %>% janitor::clean_names() %>% 
  filter(primary_mode == 'Food') %>% 
  left_join(population_data) %>% 
  group_by(state) %>% 
  mutate(min = min(illnesses, na.rm = T), 
         mean = mean(illnesses, na.rm = T), 
         median = median(illnesses, na.rm = T)) %>% 
  group_by(state, year, population, min, mean, median) %>% 
  summarise(total_illness = sum(illnesses), 
         total_outbreaks = n()) %>% 
  group_by(state, min, mean, median) %>% 
  fill(population, .direction = 'up') %>% 
  summarise(illness_per_100000 = sum(total_illness)/sum(population)*100000, 
            outbreaks_per_100000 = sum(total_outbreaks/sum(population)*100000)
            ) -> outbreak_data
make_map <- function(fill_var, legend) {
  fill_var <- enquo(fill_var)
  ggplot(outbreak_data, aes(fill = !!fill_var)) + 
    geom_map(aes(map_id = str_to_lower(state)), map = maps) + 
    expand_limits(x = maps$long, y = maps$lat) + 
    theme(panel.grid = element_blank(), 
          panel.background = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          axis.line = element_blank(), 
          axis.title = element_blank()) + 
    labs(fill = legend, 
         title = 'Foodborne Outbreaks: 1998-2017', 
         subtitle = 'All Etiologies') -> map_plot
  return(map_plot)
}

#+ echo = FALSE, fig.width = 9

make_map(illness_per_100000, 'Illness Incidence per 100,000 people')
make_map(outbreaks_per_100000, 'Outbreak Incidence per 100,000 people')
make_map(min, 'Minimun Outbreak Size')
make_map(mean, 'Mean Outbreak Size') 
make_map(median, 'Median Outbreak Size')
