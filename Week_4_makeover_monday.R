library(tidyverse)
library(data.world)
library(stringr)
library(ISOweek)
library(lubridate)
library(patchwork)
library(grid)

data_world <- "katebamford/makeover-monday"

mines_datasheet_sql <- data.world::qry_sql("SELECT * FROM mines_datasheet")
mines_datasheet <- data.world::query(mines_datasheet_sql, data_world)

citaion_copyright_sql <- data.world::qry_sql("SELECT * FROM citaion_copyright")
citaion_copyright <- data.world::query(citaion_copyright_sql, data_world)

citaion_copyright
mines_datasheet


mines_datasheet %>%
  group_by(district_name) %>%
  summarise(number_mines = n(), 
            total_coal_production_mt = sum(coal_lignite_production_mt_2019_2020)) %>%
   arrange(desc(total_coal_production_mt)) %>%
  filter(total_coal_production_mt > 20) %>%
  
  ggplot(aes(x = factor(district_name, levels = unique(district_name)), 
             y = total_coal_production_mt, fill = number_mines)) + 
             geom_bar(stat = 'identity') +
             geom_point(aes(y = number_mines), color = "#fac127", size = 2)  +   
             scale_fill_gradient2(name = "Number of mines",
                                  high = "#280b54", mid = "#9f8eb0", low = "#c7bed0", limits = c(0,70)) +
             scale_y_continuous(name = "Total coal production (MT)") +
             scale_x_discrete(name = "District") +
           theme_bw() +
           theme(
             axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
             #axis.title = element_blank(),
             panel.grid = element_blank(),
             legend.position = "bottom"
           ) +
  coord_flip()
           

# Get the shape file of India
data(wrld_simpl)
india <- wrld_simpl[wrld_simpl$NAME=="India",]

# Make it readable by ggplot
india_map <- tidy(india)

# Add mining data
mines <- 
mines_datasheet %>%
  group_by(state_ut_name) %>%
  mutate(number_mines = n(), 
         total_coal_production_mt = sum(coal_lignite_production_mt_2019_2020),
         longitude = mean(longitude),
         latitude = mean(latitude)) %>%
  arrange(desc(total_coal_production_mt)) %>%
  select(state = state_ut_name, 
         number_mines,
         total_coal_production_mt, 
         coal_lignite,
         longitude,
         latitude) %>%
  distinct()


ggplot(data = india_map) +
  geom_polygon(aes( x = long, y = lat, group = group), 
               size = 0.5, alpha = 0.5, fill = "#6b5286", colour = "white") + 
  geom_point(data = mines, aes(x = longitude, y = latitude, 
                               colour = total_coal_production_mt, size = number_mines)) +
  scale_color_viridis(name = "Total coal production (MT)", option = "B") +
  scale_size(name = "Number of mines") +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) + 
  coord_map()
  
