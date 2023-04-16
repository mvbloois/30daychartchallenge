library(tidyverse)
library(showtext)

## Fonts ----
font_add_google("Merriweather", "font")
showtext_auto()

df <-
  readxl::read_xlsx(
    "./2023/data/rail_tf_ns20_nl__custom_5802345_page_spreadsheet.xlsx",
    sheet = 3,
    skip = 9
  ) %>%
  rename(segment = 1,
         trains = 2) %>%
  mutate(segment = str_to_lower(segment)) %>%
  drop_na() %>%
  filter(is.numeric(trains)) %>%
  filter(!str_detect(segment, "hsl")) %>%
  filter(!str_detect(segment, "betuweroute")) %>%
  separate(segment, into = c("to", "from"),
           sep = "-") %>%
  mutate(across(to:from, str_squish))

stations <-
  read_csv("./2023/data/stations_nl.csv") %>% mutate(station = str_to_lower(station))


df_2 <- left_join(df,
                  stations %>% select(station, from_lon = lon, from_lat = lat)
                  ,
                  by = join_by(to == station)) %>%
  left_join(stations %>% select(station, to_lon = lon, to_lat = lat)
            ,
            by = join_by(from == station))

brk <- c(5000, 30e3, 55e3, 75e3, 100e3, 125e3, 150e3)
cols <-  rev(c('#0078FF', '#BD00FF', '#FF9A00', '#01FF1F', '#E3FF00'))
newcol <- colorRampPalette(cols)
ncols <- 7
cols2 <- newcol(ncols)
vmin <- min(df$values, na.rm = TRUE)
vmax <- max(df$values, na.rm = TRUE)
bg <- "#383E42"


ggplot(stations, aes(x = lon, y = lat)) +
  geom_segment(
    data = df_2,
    aes(
      x = to_lon,
      xend = from_lon,
      y = to_lat,
      yend = from_lat,
      colour = trains,
      size = trains
    )
  ) +
  geom_point(
    data = stations %>% filter(is_station == 1),
    aes(x = lon, y = lat),
    colour = "white",
    size = 1.5
  ) +
  scale_colour_gradientn(
    name = "# trains",
    colours = cols2,
    breaks = brk,
    labels = scales::label_number(),
    limits = c(min(brk), max(brk))
  ) +
  scale_size_continuous(range = c(0.2, 2.5)) +
  coord_equal() +
  labs(title = "Train traffic in the Netherlands",
       subtitle = "Number of passenger trains on part of the rail network (2020)",
       caption = "Data: Eurostat") +
  guides(size = "none",
         colour = guide_legend(
           direction = "horizontal",
           keyheight = unit(0.5, units = "mm"),
           keywidth = unit(14, units = "mm"),
           title.position = 'left',
           title.hjust = 0.5,
           title.vjust = 1,
           label.hjust = 0.5,
           label.vjust = 5,
           nrow = 1,
           byrow = TRUE,
           reverse = F,
           label.position = "bottom"
         )) +
  theme_void() +
  theme(text = element_text(family ="font",
                            size = 35,
                            colour = "white"),
        plot.background = element_rect(fill = bg),
        plot.title.position = "plot",
        plot.title = element_text(size = 92,
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 55,
                                     hjust = 0.5),
        plot.caption = element_text(size = 40,
                                    hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(b = 20, t = 20, r = 20, l = 20)
        )

## Saving ----
ggsave("./2023/17_networks.png",
       bg = bg,
       height = 9, width = 9)

system("open ./2023/17_networks.png")
