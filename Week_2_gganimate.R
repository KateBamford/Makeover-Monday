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
library(tweenr)
library(gifski) #good rendering engine for gganimate!


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
afr <- wrld_simpl[wrld_simpl$REGION==2,]

# Make it readable by ggplot

africa_map <- tidy(afr)

# Add HIV data to map

# add HIV data to data
africa_hiv <- 
  africa_map %>% 
  left_join(. , afr@data, by=c("id"="ISO3")) %>%
  left_join(hiv_data, by = c("NAME" = "country")) 

# Plot

  ggplot(data = africa_hiv) +
  geom_polygon(aes(fill = Female_1990, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  theme_void() +
  scale_fill_viridis(name="Population (M)", breaks=c(0, 5000, 10000, 25000, 40000, 53000), 
                     guide = guide_legend(keyheight = unit(3, units = "mm"), 
                                          keywidth=unit(12, units = "mm"), 
                                          label.position = "bottom", 
                                          title.position = 'top', nrow=1)) +
  labs(title = "Gender ineqeualities") +
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

ani_africa_hiv <- 
  africa_hiv %>%
  pivot_longer(Female_1990:Male_2019, names_to = "Key", values_to = "number") %>%
  separate(Key, into = c("Sex", "Year")) %>%
  mutate(Year = as.numeric(Year))

ani <- ggplot(data = ani_africa_hiv) +
  geom_polygon(aes(fill = number, x = long, y = lat, group = group), 
               size=0, alpha=0.9, colour = "black") +
  geom_path(aes(x = long, y = lat, group = group))+
  theme_void() +
  scale_fill_viridis(name="Number of people living with HIV", #breaks=c(0, 5000, 10000, 25000, 40000, 53000), 
                     option = "magma",
                     na.value = "white",
                     guide = guide_legend(keyheight = unit(3, units = "mm"), 
                                          keywidth=unit(12, units = "mm"), 
                                          label.position = "bottom", 
                                          title.position = 'top', nrow=1)) +
  labs( title = "Gender ineqeualities \n Year: {round(frame_time)}" ) +
  ylim(-50,40) +
  theme(
    text = element_text(color = "#22211d"), 
    plot.background = element_rect(fill = "#f5f5f4", color = NA), 
    panel.background = element_rect(fill = "#f5f5f4", color = NA), 
    legend.background = element_rect(fill = "#f5f5f4", color = NA),
    plot.title = element_text(size= 12, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    legend.position = c(0.3, 0.1)
  ) +
  coord_map() +
  facet_wrap(~ Sex) +
  transition_time(Year) +
  ease_aes('linear')

animate(ani, height = 4, width = 6, units = "in", res = 150)

anim_save("gender_inequalities_HIV.gif")




