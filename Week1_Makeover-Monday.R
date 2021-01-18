library(tidyverse)
library(data.world)
library(stringr)
library(ISOweek)
library(lubridate)
library(patchwork)
library(grid)

data_world <- "katebamford/makeover-monday"

weekly_data_sql <- data.world::qry_sql("SELECT * FROM weekly_data")
weekly_data <- data.world::query(weekly_data_sql, data_world)

bike_sql <- data.world::qry_sql("SELECT * FROM bike_counts_14_counters")
bike_data <- data.world::query(bike_sql, data_world)

method_sql <- data.world::qry_sql("SELECT * FROM methodology")
methodology <- data.world::query(method_sql, data_world)


percent_incr <- 
  bike_data %>%
  select(week_no = timeframe, date = week_of, `2019` = `2019_counts_14_counters`, `2020` = `2020_counts_14_counters`) %>% 
  mutate(week_no = str_remove(week_no, "Week ") %>% as.numeric(),
         week_no = sprintf("%02d", week_no),
         date_2019 = ISOweek2date(paste0(2019, "-W", week_no, "-1")),
         date_2020 = ISOweek2date(paste0(2020, "-W", week_no, "-1")),
         month = format(date, "%B")) %>%
  group_by(month) %>%
  mutate(monthly_mean_2019 = mean(`2019`),
         percent_incr = (`2020` - monthly_mean_2019)/monthly_mean_2019*100)

bike_chart <-
bike_data %>%
  select(week_no = timeframe, date = week_of, `2019` = `2019_counts_14_counters`, `2020` = `2020_counts_14_counters`) %>% 
  mutate(week_no = str_remove(week_no, "Week ") %>% as.numeric(),
         week_no = sprintf("%02d", week_no)) %>%
  pivot_longer(cols = `2019`:`2020`, names_to = "year", values_to = "count") %>%
  mutate(isoweek = paste0(year, "-W", week_no, "-1"),
         isoweek = ISOweek2date(isoweek), 
         count = count/1000,
         month_year = format(isoweek, "%B-%y")) %>%
  group_by(month_year) %>%
  mutate(month_average = mean(count)) %>%
  arrange(isoweek)


background.colour <- '#2E2E2E'

line <- 
  ggplot(data = bike_chart, aes(x = date, y = count, group = year)) +
  geom_vline(xintercept = as.numeric(bike_chart$date[11]), size = 4, col = "lightgrey", alpha = 0.3) +
  geom_text(label = "Lockdown begins", x = as.numeric(bike_chart$date[11])-0.1, y = 120, 
            angle = 90, colour = "white", family = "mono", size = 3) +
  geom_line(aes(col = year), size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "month", name = "Month", 
               limits = c(ymd("2019-12-30"), ymd("2020-11-09")))+
  scale_y_continuous(name ="Trail use count (000s)", limits = c(0, 150)) +
  scale_colour_manual(values = c("#ffa600", "darkgreen"))+ 
  theme_classic()+
  theme(plot.background = element_rect(fill = background.colour),
        panel.background = element_rect(fill= background.colour),
        legend.background = element_rect(fill = background.colour))+
  labs(colour = "Year")

percent_incr_plot <- 
    ggplot(data = percent_incr, aes(x = date_2020, y = percent_incr)) +
    geom_vline(xintercept = as.numeric(percent_incr$date[11]), size = 4, col = "lightgrey", alpha = 0.3) +
    geom_bar(stat = "identity", fill = "#a9c2a0") +
    scale_x_date(date_labels = "%b", date_breaks = "month", name = "Month", 
                 limits = c(ymd("2019-12-30"), ymd("2020-11-10")))+
    scale_y_continuous(name ="Percentage increase", limits = c(-30, 205)) +
    theme_classic()+
    theme(plot.background = element_rect(fill = background.colour),
          panel.background = element_rect(fill= background.colour, color = background.colour),
          legend.background = element_rect(colour = NA))
  
bike_text <- paste0('Travel restrictions and stay-at-home orders in March 2020 ','were thought to increase leisure time \n',
                    'and result in increased bike use, and indeed one week in ','March saw a 200% increase compared to the March 2019 average. \n',
                    'However, bike use in the weeks before the pandemic started ','were already seeing a percentage increase of 100% \n',
                    'compared to the February and March 2019 monthly averages, ','closer to the percentage increase seen across the rest of 2020')


patchwork <- line / percent_incr_plot 

final_plot <- patchwork + plot_annotation(
  title = 'Did bike use really increase in 2020?',
  subtitle = bike_text,
  caption = paste0('Data source: Rails To Trails \n USA wide trail count data \n',
                   'In mid March 2020 states imposed travel restrictions and encouraged social distancing \n',
                   'Percentage increase is the increase in bike counts compared to the 2019 monthly average')
                           ) & 
  theme(text = element_text('mono', colour = 'White'), 
        plot.title = element_text(size = 20),
        plot.background = element_rect(fill = background.colour, colour = background.colour),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"))


520 x 320 

ggsave(filename = "Outputs/BikeBoom.png", plot = final_plot, width = 11.5, height = 8, dpi = 300)

