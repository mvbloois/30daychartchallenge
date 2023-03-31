library(tidyverse)
library(waffle)
library(ggtext)
library(showtext)
source("./2023/00_functions.R")

# https://www.24kitchen.nl/recepten/stroopwafels
# https://liamgilbey.github.io/ggwaffle/index.html

## Text and colours ----
txt <- "#BCD2E8"
bg <- "#152238"

pal <- c("butter" = "#FD0100",
         "creme" = "#F76915",
         "egg" = "#EEDE04",
         "flower" = "#A0D636",
         "sirup" = "#2FA236",
         "sugar" = "#333ED4")

source <- "24Kitchen.nl"
    
title <- str_to_upper("Sirup waffles from Gouda")
    
caption <- create_caption(txt, bg, source)
    
## Fonts ----
font_add_google("Noto Serif", "font")

showtext_auto()

## Data ----

ingredients <-
  tibble(
part        =c(rep("waffles", 4), rep("sirup", 4)),
ingredient = c("flower", "sugar", "butter", "egg", "sugar", "creme", "sirup", "butter"),
grams      = c(240, 95, 80, 60, 500, 82, 75, 225)
) 

## Theme ----
theme_set(theme_void())

theme_update(
  text = element_text(family = "font",
                      colour = txt,
                      size = 62, lineheight = 0.3),
  plot.background = element_rect(fill = bg,
                                 colour = bg),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.title = element_text(family = "font",
                            face = "bold",
                            colour = txt,
                            margin = margin(t = 5, b = 5),
                            size = 82,
                            hjust = 0.5),
  plot.subtitle = element_text(family = "font",
                               size = 55,
                               hjust = 0.5,
                               margin = margin(t = 5, b = 5)
  ),
  plot.caption = element_markdown(family = "font",
                                  size = 32,
                                  colour = txt,
                                  hjust = 1,
                                  margin = margin(t = 25)),
  plot.margin = margin(b = 20, t = 20, r = 20, l = 20),
  legend.position = "bottom",
  panel.grid.major.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  strip.text.x = element_text(vjust = 1, hjust = 0.5)
)

ggplot(ingredients, aes(fill = ingredient, values = grams)) +
  geom_waffle(colour = bg, n_rows = 25, flip = TRUE) + 
  facet_wrap(~part, nrow = 1, strip.position = "bottom") +
  scale_fill_manual(values = pal) +
  coord_equal() +
  labs(
    title = title,
    subtitle = "Ingredients for 16 sirup waffles, add pinches of salt and cinnamon powder",
    caption = caption,
    fill = "one gram of:"
  )

## Saving ----
ggsave("./2023/02_waffle.png",
       bg = bg,
       height = 9, width = 12)

system("open ./2023/02_waffle.png")

