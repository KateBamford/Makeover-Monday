library(tidyverse)
library(data.world)
library(stringr)
library(ISOweek)
library(lubridate)
library(patchwork)
library(grid)

data_world <- "katebamford/makeover-monday"

gender_inequality_sql <- data.world::qry_sql("SELECT * FROM jan_2021_data_viz5_gender_inequality_and_hivaids")
gender_inequality_data <- data.world::query(gender_inequality_sql, data_world)


gender_inequality_data %>%
  mutate(rate = estimated_number_of_annual_aids_related_deaths) %>%
  ggplot(aes(x = year, y = rate)) + 
  geom_point() +
  facet_wrap(country ~ sex)

test <- 
  gender_inequality_data %>%
  mutate(rate = estimated_rate_of_annual_aids_related_deaths_per_100_000_population,
         num = estimated_number_of_people_living_with_hiv) %>%
  select(country, year, sex, num) %>%
    pivot_wider(names_from = "sex", values_from = num) %>%
    mutate(ratio = Female/Male) %>%
    ggplot(aes(x = year, y = ratio)) +
    geom_point(alpha=0.7)  

    
