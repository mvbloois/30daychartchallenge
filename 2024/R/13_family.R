library(tidyverse)
library(ggtext)
library(showtext) 

font_add_google("Merriweather", "title")
font_add_google("Merriweather", "font")
showtext_auto()

bg <- "#D7C2A8"

st <- str_wrap("Traditionally, Dutch families perpetuated first names across generations. For instance, a child might be named after a grandparent, ensuring that these names endure for centuries. The most common example is naming a child after its grandparent.", 60)

tribble(
  ~gen, ~name,
  1, "Servaas van Duijf (-1713) x **Agneta** van Diesen (1640-)",
  2, "Johannes van Duijf (1675-1713) x Sara Nieuwenhuijzen",
  3, "Bernardus Cantzelaar (1713-1783) x **Angenieta** van Duijf (1713-1783)",
  4, "Johannes Canzelaer (1742-1815) x Jannetje Hekkers (1737-1818)",
  5, "Arie van der Kolk (1777-1844) x **Angenitha** Cantzelaar (1776-1859)",
  6, "Johannes van der Kolk (1799-1847) x Jacoba Roodbol (1804-1853)",
  7, "Adrianus Both (1826-1907) x Elizabeth van der Kolk (1826-1896)",
  8, "Arie Boer (1866-1962) x **Angenietha** Both (1867-1933)",
  9, "Jacobus van Krimpen (1887-1975) x Jannigje Boer (1885-1960)",
  10, "**Angenietha** van Krimpen (1915-2006)"
) |> 
  ggplot(aes(x = 0, y = gen, label = name)) +
  geom_segment(aes(x = 0, xend = 0, y = 1, yend = 10),
               colour = "grey15") +
  geom_richtext(#family = "font",
                size = 10,
                colour = "grey20",
                fill = colorspace::darken(bg, 0.01),
                label.color = NA,
                label.padding = unit(9, "pt")) +
  labs(title = "FAMILY",
       subtitle = st,
       caption = "Source: M. van Bloois") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(family = "title",
                              face = "bold",
                              size = 68),
    plot.subtitle = element_text(family = "title",
                                size = 42,
                                lineheight = 0.35),
    plot.caption = element_text(size = 24),
    plot.margin = margin(10, 20, 10, 20)
  )

ggsave(
  "./2024/R/13_family.png",
  bg = "white",
  width = 6,
  height = 7)

system("open ./2024/R/13_family.png")
