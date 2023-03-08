## Packages ----
library(tidyverse)
library(ggtext)
library(showtext)
source("./2023/00_functions.R")

## Text and colours ----
txt <- "darkgreen"
bg  <- "#C5C5C5"

source <- "Whatever by Whomever"

title <- "PART-TO-WHOLE"

subtitle <- paste0(
  "Sentiment analysis based on 111.000+ tweets in 14 African languages shows<br>",
  "some interesting differences in ",
  "<span style = 'color:#319400'>positive</span>, ",
  "<span style = 'color:#F7C608'>neutral</span> and ",
  "<span style = 'color:#952038'>negative</span> sentiments."
)

caption <- create_caption(txt, bg, source)

## Fonts ----
font_add_google("Playfair Display", "titlefont")
font_add_google("Fira Sans", "mainfont")
showtext_auto()

## Theme ----
theme_set(theme_void())

theme_update(
  text = element_text(family = "mainfont",
                      size = 62, lineheight = 0.3),
  plot.background = element_rect(fill = bg,
                                 colour = bg),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.title = element_text(family = "titlefont",
                            face = "bold",
                            margin = margin(t = 5, b = 5),
                            size = 102,
                            hjust = 0.5),
  plot.subtitle = element_markdown(
                                   size = 62,
                                   hjust = 0.5
                                   ),
  plot.caption = element_markdown(family = "mainfont",
                                  size = 62,
                                  colour = "grey15",
                                  hjust = 0.5,
                                  margin = margin(b = 5)),
  plot.margin = margin(b = 20, t = 20, r = 0, l = 0),
)

## Data ----
df <- tibble::tibble(
  var1 = c("A", "A", "A", "B", "B"),
  var2 = c("X", "Y", "Z", "Y", "Z"),
  var3 = c(33, 33, 34, 50, 50)
)

## Plot ----

df %>% 
  mutate(var1 = as.factor(var1),
         var2 = as.factor(var2)) %>% 
  ggplot(aes(x = as.numeric(var1), y = var3, fill = var2)) +
  geom_col(linewidth = 1, position = position_stack()) +
  xlim(c(-1,3)) +
  coord_polar(theta = "y") +
  labs(title = title,
       subtitle = subtitle,
       caption = caption,
       fill = "Colours") 

## Saving ----
ggsave("./2023/01_part-to-whole.png",
       bg = bg,
       height = 12, width = 12)

system("open ./2023/01_part-to-whole.png")
