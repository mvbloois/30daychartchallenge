library(tidyverse)
library(ggpattern)
library(showtext)

## Fonts ----
font_add_google("Old Standard TT", "font")
showtext_auto()

txt <- "grey25"
bg <- "#E0D3AF"
  
df <- read_csv2("./2023/data/historical.csv") %>% 
  mutate(price = pound * 240 + shilling * 12 + pence)

## Theme ----
theme_set(theme_minimal())

theme_update(
  text = element_text(family = "font",
                      size = 62, lineheight = 0.3),
  panel.grid = element_line(colour = "grey80"),
  plot.background = element_rect(fill = bg,
                                 colour = bg),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.title = element_text(family = "font",
                            face = "bold",
                            colour = txt,
                            margin = margin(t = 5, b = 5),
                            size = 62,
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
                                  margin = margin(t = 15)),
  plot.margin = margin(b = 20, t = 20, r = 20, l = 20),
  legend.position = "none",
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank()
)

df %>% 
ggplot(aes(x = year, y = price)) +
  geom_col_pattern(pattern_spacing = 0.01) +
  scale_y_continuous(breaks = 1:4*240,
                     labels = c("£1", "£2", "£3", "£4")) +
  labs(title = str_wrap("PRICES OF THE QUARTER OF NINE BUSHELS OF THE BEST OR HIGHEST PRICED WHEAT AT WINDSOR MARKET, ON LADY-DAY AND MICHAELMAS, FROM 1595 TO 1764, BOTH INCLUSIVE; THE PRICE OF EACH YEAR BEING THE MEDIUM BETWEEN THE HIGHEST PRICES OF THOSE TWO MARKET-DAYS.", 60),
       caption = "Data: Smith, Adam (1776), An Inquiry into the Nature and Causes of the Wealth of Nations<br>
       Correspondence: Mr. M. van Bloois, Gouda, HOL-LAND",
       x = NULL,
       y = NULL) 
## Saving ----
ggsave("./2023/04_historical.png",
       bg = bg,
       height = 9, width = 12)

system("open ./2023/04_historical.png")

