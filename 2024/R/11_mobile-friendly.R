
library(tidyverse)
library(showtext)

font_add_google("Forum", "font")

showtext_auto(enable = TRUE)

data <- read_csv("./2024/data/mobile.zip")

world_map <-  map_data("world") |> 
  filter(!long > 180)

countries <- world_map %>%
  distinct(region) %>%
  rowid_to_column()

countries |>
  ggplot(aes(map_id = region)) +
  geom_map(
    map = world_map,
    colour = "darkblue",
    fill = "lightblue",
    linewidth = 0.2
  ) +
  geom_point(
    data = data,
    aes(x = longitude, y = latitude),
    size = 0.2,
    colour = "darkred",
    alpha = 0.7,
    inherit.aes = FALSE
  ) +
  labs(title = "MOBILE FRIENDLY",
       subtitle = "Cities with 5G coverage",
       caption = "Data: Kaggle") +
  expand_limits(x = world_map$long, y = world_map$lat) +
  coord_map("moll") +
  theme_void() +
  theme(
    text = element_text(family = "font"),
    plot.title = element_text(hjust = 0.5,
                              size = 56),
    plot.subtitle = element_text(hjust = 0.5,
                                 size = 38),
    plot.caption = element_text(hjust = 0.5,
                                size = 22),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(
  "./2024/R/11_mobile-friendly.png",
  bg = "white",
  width = 7,
  height = 5,
)

system("open ./2024/R/11_mobile-friendly.png")
