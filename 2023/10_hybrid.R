library(tidyverse)
library(ggforce)
library(showtext)
library(PrettyCols)

## Fonts ----
font_add_google("Fira Code", "font")
showtext_auto()
showtext_opts(dpi = 300)

bg <- "white"
pal <- prettycols("Dark")

## Theme ----
theme_set(theme_void())

theme_update(
  text = element_text(family = "font",
                      size = 62,
                      colour = stone),
  plot.background = element_rect(colour = bg,
                                 fill = bg),
  plot.caption = element_text(size = 24)
)

df <- tibble(
  x =     c(0, 3, 6, 0, 2.75, 6, 7.7, 7),
  y =     c(0, 6, 0, 3.75, -1.5, 3.6, -3.7, -3),
  angle = c(300, 0, 60, 335, 90, 25, 45, 45),
  label_en = c("zebra", "horse", "donkey", "zorse", "zonkey",
               "mule/hinny", "Syrian\nwild ass", "kunga"),
  size = c(1, 1, 1, 132, 132, 132, 132, 132)
)

ggplot() +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = 5, b = 3, angle = 225),
               fill = pal[1], colour = pal[1],  alpha = 0.5) +
  geom_ellipse(aes(x0 = 6, y0 = 0, a = 5, b = 3, angle = 45),
               fill = pal[2], colour = pal[2], alpha = 0.5) +
  geom_ellipse(aes(x0 = 3, y0 = 5.5, a = 5, b = 3, angle = 0),
               fill = pal[3], colour = pal[3], alpha = 0.5) +
  geom_circle(aes(x0 = 7.5, y0 = -3.5, r = 1.2),
              fill = pal[4], colour = pal[4], alpha = 0.5) +
  geom_text(data = filter(df, size == 1),
            aes(x = x, y = y, label = label_en, angle = angle),
            colour = "white", size = 28) +
  geom_text(data = filter(df, size > 1),
            aes(x = x, y = y, label = label_en, angle = angle),
            colour = "white", size = 6) +  
  guides(size = "none") +
  labs(title = str_to_upper("Equid Hybrids"),
    caption = "Data: Wikipedia") +
  coord_equal() 

## Saving ----
ggsave("./2023/10_hybrid.png",
       bg = bg,
       height = 9, width = 12)

system("open ./2023/10_hybrid.png")
