library(tidyverse)
library(jsonlite)
library(wesanderson)
library(showtext)

font_add_google("Ubuntu", "font")
showtext_auto()

df <- read_json("./2024/data/aantal-aardbevingen-20240328085006.json")

data <- df$data

plot_data <- as_tibble(data) |> 
  pivot_longer(cols = everything(), names_to = "year", values_to = "n") |> 
  mutate(magnitude = names(n)) |> 
  unnest(cols = n) |> 
  filter(n > 0 & year < 2024)

plot_data |> 
  ggplot(aes(x = year, y = magnitude, fill = n)) +
  geom_raster() +
  scale_x_discrete(breaks = seq(1991, 2023, 2)) +
  
  scale_fill_gradientn(colours = wes_palette("Zissou1", 100, type = "continuous")) +
  
  coord_fixed() +
  labs(x = NULL, y = NULL,
       title = "Earthquakes in Groningen",
       subtitle = str_wrap("Since the 1960s, the Netherlands has been extracting natural gas from the subsurface of North Netherlands. Gas extraction is responsible for nearly all earthquakes in the northern part of the country. The first earthquake occurred near Assen on December 26, 1986. The gas is extracted from a layer of sandstone at a depth of 3 kilometers. Due to gas extraction, the sandstone layer compacts. Along faults in this layer, a difference in stress builds up, eventually resulting in a sudden shift: an earthquake.", 80),
       caption = "Data: KNMI",
       fill = "# of earthquakes:") +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title.position = "top",
    text = element_text(family = "font",
                        size = 24),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 26,
                              hjust = 0),
    plot.title.position = "plot",
    plot.title = element_text(size = 62),
    plot.subtitle = element_text(size = 42,
                                 lineheight = 0.4),
    plot.caption = element_text(size = 26),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(
  "./2024/R/07_hazards.png",
  bg = "white",
  width = 8,
  height = 6,
)
system("open ./2024/R/07_hazards.png")



