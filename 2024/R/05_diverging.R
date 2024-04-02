

library(tidyverse)
library(geomtextpath)
library(showtext)

font_add_google("Lato", "lato")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()

#https://www.npdata.be/Data/NIS/Bevolking/Bevolking-1831-2018-c.xls
cols <- c("#FF0F21", "#FF7F00")


subtitle <- str_wrap("Since 1900, the population dynamics of the Netherlands and Belgium have taken an interesting turn. Initially, Belgium held a population much larger as that of the Netherlands. Over time, the Netherlands has experienced consistent and rapid growth, surpassing Belgium in population size.", 60)


plot_data <- read_csv("./2024/data/05_diverging.csv")

plot_segment <- plot_data |> 
  filter(Jaar %in% seq(1900, 2020, by = 10)) |> 
  pivot_wider(names_from = Land, values_from = Bevolking) |> 
  mutate(delta = abs(België - Nederland),
         Land = if_else(België > Nederland, "België", "Nederland"))

plot_data |> 
  ggplot(aes(x = Jaar, y = Bevolking)) +
  geom_line(aes(group = Land, colour = Land),
            linewidth = 1.5) +
  geom_segment(data = plot_segment,
               aes(x = Jaar, xend = Jaar,
                   y = België, yend = Nederland,
                   colour = Land),
               size = .8, linetype = 2) +
  annotate(geom = "text", x = 1940, y = 12e6, label = "Netherlands",
           colour = "#FF7F00", size = 16) +
  annotate(geom = "curve", x = 1952, xend = 1955, y = 12e6, yend = 10.5e6,
           colour = "#FF7F00",curvature = -0.5) +
  annotate(geom = "text", x = 1960, y = 7e6, label = "Belgium",
           colour = "#FF0F21", size = 16) +
  annotate(geom = "curve", x = 1968, xend = 1972, y = 7e6, yend = 9.7e6,
           colour = "#FF0F21",curvature = 0.5) +
  scale_x_continuous(breaks = seq(1900, 2020, 10)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::number_format()) +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  labs(x = NULL,
       y = NULL,
       colour = NULL,
       title = "DIVERGING",
       subtitle = subtitle,
      caption = "Data: CBS, Wikipedia, npdata.be") +
  theme_minimal() +
  theme(
    text = element_text(family = "lato",
                        colour = "grey15",
                        size = 52),
    legend.position = "none",
    axis.text = element_text(family = "lato",
                             size = 32),
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(family = "lato",
                              face = "bold",
                              size = 92,
                              hjust = 0),
    plot.subtitle = element_text(family = "lato",
                                 size = 52,
                                 colour = "grey25",
                                 lineheight = 0.3,
                                 hjust = 0,
                                 margin = margin(t = 5, b = 15)),
    plot.caption = element_text(size = 22,
                                hjust = 1,
                                margin = margin(t = 15)),
    plot.margin = margin(20,20,20,20)
  )
 
ggsave(
  "./2024/R/04_diverging.png",
  bg = "white",
  width = 8,
  height = 6,
)
system("open ./2024/R/04_diverging.png")

