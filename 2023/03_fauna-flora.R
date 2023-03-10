library(tidyverse)
library(glue)
library(ggimage)
library(ggtext)
library(showtext)
source("./2023/00_functions.R")

# https://brinsea.co.uk/latest/resource-centre/egg-sizes/

sigma <- 4
bg <- "#296D98"
txt <- "#DBD76A"

caption <- create_caption(txt, bg, "As of yet unknown")

showtext_auto()

egg_dimensions <-
  tibble(
    start_x = c(0, 40, 97),
    bird    = c("Quail", "Duck", "Goose"),
    diameter  = c(27, 44, 55),
    length = c(35, 65, 90),
    image = c("./resources/chicken_egg.jpg",
              "./resources/chicken_egg.jpg",
              "./resources/chicken_egg.jpg")
  ) 

egg_dimensions %>% 
  ggplot() +
  geom_segment(aes(x = start_x - sigma, xend = start_x - sigma,
               y = 0, yend = length),
               linewidth = 1.2) +
  # Ruler Height
  geom_segment(aes(x = start_x - sigma - 1.5, xend = start_x - sigma + 1.5,
                   y = 0, yend = 0),
               linewidth = 1.2) +
  geom_segment(aes(x = start_x - sigma - 1.5, xend = start_x - sigma + 1.5,
                   y = length, yend = length),
               linewidth = 1.2) +
  # Ruler Diameter
  geom_segment(aes(x = start_x, xend = start_x + diameter,
                   y = 0 - sigma, yend = 0 - sigma),
               linewidth = 1.2) +
  geom_segment(aes(x = start_x, xend = start_x,
                   y = 0 - sigma - 1.5, yend = 0 - sigma + 1.5),
               linewidth = 1.2) +
  geom_segment(aes(x = start_x + diameter, xend = start_x + diameter,
                   y = 0 - sigma - 1.5, yend = 0 - sigma + 1.5),
               linewidth = 1.2) +
  # Labels  
  geom_text(aes(x = start_x + diameter * 0.5, y = -15, label = bird),
            size = 10,
            colour = txt,
            fontface = "bold") +
  geom_text(aes(x = start_x + diameter * 0.5, y = -7, label = glue("~{diameter} mm")),
            size = 5,
            fontface = "bold",
            colour = txt) +
  geom_text(aes(x = start_x - sigma - 5 , y = length * 0.5, label = glue("~{length} mm")),
            size = 5,
            angle = 90,
            fontface = "bold",
            colour = txt) +
  # Images
  geom_image(aes(x = start_x + diameter * 0.5, y = length * 0.5,
                 image = image),
             size = 0.2, asp = 1.3) +
  ylim(c(-20, 100)) +
  coord_equal() +
  labs(title = "EGGS",
       subtitle = "subtitle",
       caption = caption) +
  theme_void() +
  theme(
    text = element_text(colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown()
  )
  
