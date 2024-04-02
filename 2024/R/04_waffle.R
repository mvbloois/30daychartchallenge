
library(tidyverse)
library(janitor)
library(waffle)
library(MetBrewer)
library(showtext)


font_add_google("Playfair Display", "font")
showtext_auto()

bg <- "#EFEFEF"
cols <- met.brewer("Derain")

data <- tibble(
  use = c("Agricultural", "Natural", "Buildup area", "Recreational", "Infrastructural", "Semi buildup area"),
  area = c(223.6317, 49.8956, 36.1526, 10.5418, 11.5563, 4.9318)
) |> 
  arrange(desc(area))

sum(data$area)

data |> 
  ggplot(aes(fill = use, values = area)) +
  geom_waffle(n_rows = 20, flip = TRUE,
              colour = bg) +
  coord_fixed() +
  labs(title = "Land use of the Netherlands",
       caption = "Data: Centraal Bureau voor de Statistiek",
       fill = "Land use:") +
  scale_fill_manual(values = cols) +
  theme_void() +
  theme(
    text = element_text(family = "font"),
    plot.background = element_rect(fill = bg, colour = bg),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 21),
    plot.title = element_text(size = 88,
                              hjust = 0.5),
    plot.subtitle = element_text(size = 54,
                                 hjust = 0),
    plot.caption = element_text(size = 38,
                                hjust = 0.5,
                                margin = margin(t = 15)),
    plot.margin = margin(10, 10, 10, 10)
  )

## Saving ----
ggsave("./2024/R/04_waffle.png",
       bg = bg,
       width = 8,
       height = 6,)

system("open ./2024/R/04_waffle.png") 

