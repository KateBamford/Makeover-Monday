library(tidyverse)
library(data.world)
library(stringr)
library(ISOweek)
library(lubridate)
library(patchwork)
library(grid)

data_world <- "katebamford/makeover-monday"

renewables_datasheet_sql <- data.world::qry_sql("SELECT * FROM data")
renewables_data <- data.world::query(renewables_datasheet_sql, data_world)

renewables_data %>%
  filter(is.na(share_of_production) == F) %>%
  select(year, area, variable, share_of_production) %>%
  filter(area == "Austria", year == "2000")
  mutate(renewable = case_when(variable == "Fossil "))

