
# https://www.cbs.nl/nl-nl/visualisaties/dashboard-bevolking/leeftijd/bevolking

library(tidyverse)
library(showtext) 

font_add_google("Truculenta", "truculenta")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()

levs <- c("jonger dan 20 jaar", "20 tot 40 jaar", "40 tot 65 jaar", "65 tot 80 jaar", "80 jaar of ouder")

plot_data <- tibble::tribble(
  ~Leeftijd, ~jonger.dan.20.jaar, ~`20.tot.40.jaar`, ~`40.tot.65.jaar`, ~`65.tot.80.jaar`, ~`80.jaar.of.ouder`,
      2022L,                 21L,               26L,               33L,               15L,                  5L,
      2000L,                 24L,               30L,               32L,               10L,                  3L,
      1975L,                 34L,               30L,               26L,                9L,                  2L,
      1950L,                 37L,               29L,               26L,                7L,                  1L
  ) |> 
  pivot_longer(cols = -Leeftijd, names_to = "category", values_to = "percentage") |> 
  rename(year = Leeftijd) |> 
  mutate(category = str_replace_all(category, "\\.", " ")) |> 
  mutate(category = factor(category, levels = levs)) |> 
  mutate(year = as.character(year))

plot_data |> 
  ggplot(aes(x = year, y = category, fill = percentage)) +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(label = percentage),
            family = "robotoslab",
            colour = "white",
            fontface = "bold",
            size = 12) +
  scale_fill_viridis_c(direction = -1) +
  labs(x = NULL,
       y = NULL,
       title = "TILES",
       subtitle = "Population by age group in the Netherlands (percentage)",
       caption = "Source: CBS") +
  theme_minimal() +
  theme(
    text = element_text(family = "robotoslab",
                        size = 28),
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(family = "truculenta",
                              face = "bold",
                              size = 82),
    plot.subtitle = element_text(lineheight = 0.3,
                                 margin = margin(0,0,10,0))
    )

ggsave(
  "./2024/R/23_tiles.png",
  bg = "white",
  width = 5,
  height = 5,
)

system("open ./2024/R/23_tiles.png")
