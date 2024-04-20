library(tidyverse)
library(showtext) 

font_add_google("Truculenta", "truculenta")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()

cols <- PrettyCols::prettycols("Bold")

bg <- cols[3]
fg <- cols[4]
time <- cols[5]

plot_data <- tibble(
  place = c(2,1),
  item = c("Age of dinosaurs", " Age of man"),
  start = c(-240, -0.3),
  end = c(-66, 0)
) 

plot_data |> 
  ggplot() +
  geom_segment(aes(x = start, xend = end, y = place),
               linewidth = 14,
               colour = time) +
  geom_text(aes(x = (start+end)/2, y = place + 0.4, label = item),
            size = 14,
            family = "robotoslab",
            colour = fg) +
  scale_x_continuous(limits = c(-255, 19), breaks = c(-250,-200,-150,-100,-50,0)) +
  scale_y_continuous(limits = c(0,4)) +
  labs(x = "Million Years Ago",
       y = NULL,
       title = "DINOSAURS",
       caption = "Source: Wikipedia") +
  theme_minimal() +
  theme(
    text = element_text(family = "robotoslab",
                        colour = cols[1],
                        size = 28),
    plot.background = element_rect(fill = alpha(bg, 0.3),
                                   colour = alpha(bg, 0.3)),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.2,
                                      alpha(bg, 0.5)),
    axis.text.x = element_text(colour = cols[1]),
    axis.text.y = element_blank(),
    plot.title = element_text(family = "truculenta",
                              face = "bold",
                              size = 82),
    plot.subtitle = element_text(lineheight = 0.3,
                                 margin = margin(0,0,10,0)),
    plot.margin = margin(20,20,20,20)
    )

ggsave(
  "./2024/R/19_dinosaurs.png",
  bg = "white",
  width = 5,
  height = 5,
)

system("open ./2024/R/19_dinosaurs.png")
