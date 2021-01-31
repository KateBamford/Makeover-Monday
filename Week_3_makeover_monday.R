library(tidyverse)
library(data.world)
library(stringr)
library(ISOweek)
library(lubridate)
library(patchwork)
library(grid)

data_world <- "katebamford/makeover-monday"

global_sql <- data.world::qry_sql("SELECT * FROM global_temperature_anomalies")
global_data <- data.world::query(global_sql, data_world)

# Select columns and turn into a long dataset
plot <- global_data %>%
  select(hemisphere:dec) %>%
  pivot_longer(cols = jan:dec, names_to = "month", values_to = "change") %>%
  mutate(month = factor(month, levels = c(unique(month)))) %>%
  
  ggplot(aes(year, month, fill = change))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="Change from 1980 baseline", option ="B", limits = c(-2,2)) +
  facet_grid(vars(hemisphere)) + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = background.colour),
        strip.background = element_blank(), 
        strip.text = element_text(family = "mono", colour = 'white', size = 15),
        legend.text = element_text(family = 'mono', colour = "white"),
        legend.title = element_text(family = 'mono', colour = 'white'),
        legend.position = 'bottom',
        legend.background = element_rect(fill = background.colour, colour = background.colour),
        plot.background = element_rect(fill = background.colour, colour = background.colour),
        plot.title = element_text(family = 'mono',color="white", size=20, face="bold"),
        plot.subtitle = element_text(family = 'mono',color="white", size=13),
        plot.caption = element_text(family = 'mono',color="white", size = 7)
        ) + 
  ggtitle("Watching the world burn",
          subtitle = "Global temperatures over the last 3 decades")+ 
  labs(caption = "Data source: NASA Goddard Institute for Space Studies\nVisulisation created by: @data_bam\nCREDITS: Eric Roston (@eroston) and Blacki Migliozzi (@blackili)")


ggsave(filename = "Outputs/GlobalWarming.png", plot = plot, width = 11.5, height = 8, dpi = 300)

