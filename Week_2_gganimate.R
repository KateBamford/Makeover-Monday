library(data.world)
library(tidyverse)
library(gganimate)
library(viridis)
library(hrbrthemes)
library(mapdata)
library(maptools)
library(broom)
library(mapproj)
library(transformr)
library(maps)
library(ggthemes)
library(haven)
library(ggpubr)
library(scales)
library(zoo)
library(gifski) #good rendering engine for gganimate!

library(curl)
library(readxl)
library(data.table)
library(tweenr)
library(rgeos)
library(devtools)
install_github("dgrtwo/gganimate", ref = "26ec501")
library(gganimate)

# Load HIV data
data_world <- "katebamford/makeover-monday"

gender_inequality_sql <- data.world::qry_sql("SELECT * FROM jan_2021_data_viz5_gender_inequality_and_hivaids")
gender_inequality_data <- data.world::query(gender_inequality_sql, data_world)

hiv_data <- 
  gender_inequality_data %>% 
  select(country, sex, year, number = estimated_number_of_people_living_with_hiv) %>%
  pivot_wider(names_from = c("sex", "year"), values_from = number)

# Get the shape file of Africa
data(wrld_simpl)
africa <- wrld_simpl[wrld_simpl$REGION==2,]

# Make it readable by ggplot

africa_map <- tidy(afr)

# Add HIV data to map

# add HIV data to data
africa_hiv <- 
  africa_map %>% 
  left_join(. , afr@data, by=c("id"="ISO3")) %>%
  left_join(hiv_data, by = c("NAME" = "country")) %>%
  replace(is.na(.), 0)


# Plot

ggplot() +
  geom_polygon(data = africa_hiv, aes(fill = Female_1990, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  theme_void() +
  scale_fill_viridis(name="Population (M)", breaks=c(0, 5000, 10000, 25000, 40000, 53000), 
                     guide = guide_legend(keyheight = unit(3, units = "mm"), 
                                          keywidth=unit(12, units = "mm"), 
                                          label.position = "bottom", 
                                          title.position = 'top', nrow=1)) +
  labs( title = "Gender ineqeualities" ) +
  ylim(-35,35) +
  theme(
    text = element_text(color = "#22211d"), 
    plot.background = element_rect(fill = "#f5f5f4", color = NA), 
    panel.background = element_rect(fill = "#f5f5f4", color = NA), 
    legend.background = element_rect(fill = "#f5f5f4", color = NA),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    legend.position = c(0.3, 0.0)
  ) +
  coord_map()


ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  # gganimate specific bits:
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

ani_africa_hiv <- 
  africa_hiv %>%
  pivot_longer(Female_1990:Male_2019, names_to = "Key", values_to = "number") %>%
  separate(Key, into = c("Sex", "Year")) %>%
  mutate(Year = as.Date(as.character(Year)))

map <- ggplot() +
  geom_polygon(data = ani_africa_hiv, aes(fill = number, x = long, y = lat, group = group, frame = Year), 
               size=0, alpha=0.9) +
  theme_void() +
  scale_fill_viridis(name="Population (M)", breaks=c(0, 5000, 10000, 25000, 40000, 53000), 
                     guide = guide_legend(keyheight = unit(3, units = "mm"), 
                                          keywidth=unit(12, units = "mm"), 
                                          label.position = "bottom", 
                                          title.position = 'top', nrow=1)) +
  labs( title = "Gender ineqeualities" ) +
  ylim(-35,35) +
  theme(
    text = element_text(color = "#22211d"), 
    plot.background = element_rect(fill = "#f5f5f4", color = NA), 
    panel.background = element_rect(fill = "#f5f5f4", color = NA), 
    legend.background = element_rect(fill = "#f5f5f4", color = NA),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    legend.position = c(0.3, 0.0)
  ) +
  coord_map()+
  facet_wrap(~ Sex) 
  # gganimate specific bits:
  labs(title = 'Year: {frame_time}') +
  transition_manual(frame = Year) +
  ease_aes('linear')

  library(gganimate)
  ani.options(interval = 0.2)
  install.packages("gganimate")
  