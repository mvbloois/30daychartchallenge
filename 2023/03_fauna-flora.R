library(tidyverse)
library(glue)
library(ggimage)
library(ggtext)
library(showtext)
source("./2023/00_functions.R")

# https://brinsea.co.uk/latest/resource-centre/egg-sizes/

font_add_google("Fira Sans", "titlefont")
font_add_google("Fira Sans", "mainfont")
showtext_auto()

sigma <- 4
bg <- "#296D98"
txt <- "#DBD76A"

caption <- create_caption(txt, bg, "The Internet")

showtext_auto()

## Theme ----
theme_set(theme_void())

theme_update(
  text = element_text(family = "titlefont",
                      colour = txt,
                      size = 24, lineheight = 0.3),
  plot.background = element_rect(fill = bg,
                                 colour = bg),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.title = element_text(family = "titlefont",
                            face = "bold",
                            colour = txt,
                            margin = margin(t = 5, b = 5),
                            size = 92,
                            hjust = 0.5),
  plot.subtitle = element_text(family = "titlefont",
                               size = 56,
                               hjust = 0.5
  ),
  plot.caption = element_markdown(family = "titlefont",
                                  size = 56,
                                  colour = "grey15",
                                  hjust = 0.5,
                                  margin = margin(b = 5)),
  plot.margin = margin(b = 20, t = 20, r = 0, l = 0),
)

egg_dimensions <-
  tibble(
    start_x = c(0, 40, 97),
    bird    = c("Quail", "Duck", "Goose"),
    diameter  = c(27, 44, 55),
    length = c(35, 65, 90),
    image = c("./2023/images/quail.png",
              "./2023/images/duck.png",
              "./2023/images/goose_resized.png")
  ) 

egg_dimensions %>% 
  ggplot() +
  geom_segment(aes(x = start_x - sigma, xend = start_x - sigma,
               y = 0, yend = length),
               linewidth = 1.2,
               colour = txt) +
  # Ruler Height
  geom_segment(aes(x = start_x - sigma - 1.5, xend = start_x - sigma + 1.5,
                   y = 0, yend = 0),
               linewidth = 1.2,
               colour = txt) +
  geom_segment(aes(x = start_x - sigma - 1.5, xend = start_x - sigma + 1.5,
                   y = length, yend = length),
               linewidth = 1.2,
               colour = txt) +
  # Ruler Diameter
  geom_segment(aes(x = start_x, xend = start_x + diameter,
                   y = 0 - sigma, yend = 0 - sigma),
               linewidth = 1.2,
               colour = txt) +
  geom_segment(aes(x = start_x, xend = start_x,
                   y = 0 - sigma - 1.5, yend = 0 - sigma + 1.5),
               linewidth = 1.2,
               colour = txt) +
  geom_segment(aes(x = start_x + diameter, xend = start_x + diameter,
                   y = 0 - sigma - 1.5, yend = 0 - sigma + 1.5),
               linewidth = 1.2,
               colour = txt) +
  # Labels  
  geom_text(aes(x = start_x + diameter * 0.5, y = -15, label = bird),
            size = 36,
            colour = txt,
            fontface = "bold") +
  geom_text(aes(x = start_x + diameter * 0.5, y = -7, label = glue("~{diameter} mm")),
            size = 15,
            fontface = "bold",
            colour = txt) +
  geom_text(aes(x = start_x - sigma - 5 , y = length * 0.5, label = glue("~{length} mm")),
            size = 15,
            angle = 90,
            fontface = "bold",
            colour = txt) +
  # Images
  geom_image(aes(x = start_x + diameter * 0.5, y = length * 0.5,
                 image = image),
             size = 0.3, asp = 1.2) +
  ylim(c(-20, 100)) +
  coord_equal() +
  labs(title = "THREE SIZES OF EGGS",
       caption = caption)

## Saving ----
ggsave("./2023/03_fauna-flora.png",
       bg = bg,
       height = 9, width = 12)

system("open ./2023/03_fauna-flora.png")
