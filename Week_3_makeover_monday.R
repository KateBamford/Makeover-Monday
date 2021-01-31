library(tidyverse)
library(data.world)
library(stringr)
library(ISOweek)
library(lubridate)
library(patchwork)
library(grid)

data_world <- "katebamford/makeover-monday"

weekly_data_sql <- data.world::qry_sql("SELECT * FROM global_temperature_anomalies")
weekly_data <- data.world::query(weekly_data_sql, data_world)