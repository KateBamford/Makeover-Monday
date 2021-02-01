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

background.colour <- "#f1f1f1"

chart <- mines_datasheet %>%
  group_by(district_name) %>%
  summarise(number_mines = n(), 
            total_coal_production_mt = sum(coal_lignite_production_mt_2019_2020)) %>%
   arrange(desc(total_coal_production_mt)) %>%
  filter(total_coal_production_mt > 20) %>%
  
  ggplot(aes(x = factor(district_name, levels = unique(district_name)), 
             y = total_coal_production_mt, fill = number_mines)) + 
             geom_bar(stat = 'identity') +
             geom_point(aes(y = number_mines, colour = ""), size = 2.5)  +  
             scale_colour_manual(values = "#fac127", name = "Number of mines") + 
             scale_fill_gradient2(name = "Number of mines",
                                  high = "#280b54", mid = "#9f8eb0", low = "#c7bed0", limits = c(0,70)) +
             scale_y_continuous(name = "Total coal production (MT)") +
             scale_x_discrete(name = "District") +
           theme_bw() +
           theme(
             #axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
             panel.border = element_blank(),
             axis.line = element_line(colour = "black"),
             panel.grid = element_blank(),
             legend.position = "bottom",
             plot.background = element_rect(fill = background.colour),
             panel.background = element_rect(fill= background.colour, color = background.colour),
             legend.background = element_blank(),
             legend.box.background = element_blank(),
             legend.key = element_rect(fill = "transparent", colour = "transparent"),
             axis.text = element_text(colour = "#280b54")) +
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


map <- ggplot(data = india_map) +
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
        axis.title = element_blank(),
        plot.background = element_rect(fill = background.colour),
        panel.background = element_rect(fill= background.colour, color = background.colour),
        legend.background = element_rect(fill= background.colour, color = background.colour),
        legend.position = "bottom",
        legend.key = element_rect(fill = "transparent", colour = "transparent")) + 
  coord_map()
  
# Patchwork 

layout <- c(
  area(t = 2, l = 1, b = 5, r = 4),
  area(t = 1, l = 4, b = 3, r = 6)
)

patchwork <- chart + map + plot_layout(guides = 'collect') +
  plot_layout(design = layout) 

final_plot <- 
  patchwork + plot_annotation(
  title = 'Indian Coal Mine Location and Production',
  caption = paste0('Data source:\nRSandeep Pai and Hisham Zerriffi.\nA novel dataset for analysing sub-national socioeconomic developments\nin the Indian coal industry, IOPSciNotes,\nhttps://doi.org/10.1088/2633-1357/abdbbb')
) & 
  theme(text = element_text('mono', colour = '#280b54'), 
        plot.title = element_text(size = 25),
        plot.background = element_rect(fill = background.colour, colour = background.colour))

ggsave(filename = "Outputs/IndiaCoal.png", plot = final_plot, width = 11.5, height = 8, dpi = 300)
