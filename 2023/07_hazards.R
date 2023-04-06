library(tidyverse)
library(rvest)
library(showtext)
library(ggtext)

## Fonts ----
font_add_google("Ubuntu", "font")
font_add_google("Roboto Condensed", "subfont")
showtext_auto()

## Text and colours ----
txt <- "#282C2F"
bg <- "#EEFFFF"

subtitle <- paste0(
"Between 1958 and 2023 there were 2,900 confirmed ",
"<span style = 'color:#006994'>**unprovoked shark attacks**</span> ",
"around the world,<br> of which 633 were <span style = 'color:#880808;font-weight:bold;'>**fatal**</span>. ",
"In the same period the human population grew by 5 billion.<br>The U.S.A. and Hawaii have by far the lowest fatality to attack ratio."
)

url <- "https://en.wikipedia.org/wiki/Shark_attack"

tbl <- read_html(url) %>% 
   rvest::html_nodes("table.wikitable") %>% 
   rvest::html_table(fill = TRUE)

table <- tbl[[1]]

plot_data <- table %>% 
  head(14) %>% 
  mutate(Totalattacks = str_remove(Totalattacks, "\\[.+") %>% parse_integer(.),
         Fatalattacks = str_remove(Fatalattacks, "\\[.+") %>% parse_integer(.),
         Lastfatality = str_remove(Lastfatality, "\\[.+") %>% parse_integer(.),
         Region = str_replace(Region, "\\(", " \\("),
         Region = str_remove(Region, "Pacific Islands / "),
         perc = Fatalattacks / Totalattacks)

  
## Theme ----
theme_set(theme_minimal())

theme_update(
  text = element_text(family = "font",
                      size = 62, lineheight = 0.3),
  plot.background = element_rect(fill = bg,
                                 colour = bg),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.title = element_text(family = "font",
                            face = "bold",
                            colour = txt,
                            margin = margin(t = 5, b = 5),
                            size = 92,
                            hjust = 0.5),
  plot.subtitle = element_markdown(family = "subfont",
                               size = 55,
                               hjust = 0.5,
                               margin = margin(t = 5, b = 5)
  ),
  plot.caption = element_text(family = "font",
                                  size = 32,
                                  colour = txt,
                                  hjust = 1,
                                  margin = margin(t = 15)),
  plot.margin = margin(b = 20, t = 20, r = 20, l = 20),
  legend.position = "none",
  panel.grid.major.y = element_blank(),
  panel.grid.minor.x = element_blank()
)


plot_data %>% 
  ggplot(aes(y = fct_reorder(Region, Totalattacks))) +
  geom_col(aes(x = Totalattacks), fill = "#006994") +
  geom_col(aes(x = Fatalattacks), fill = "#880808") +
  geom_text(aes(x = -50, label = scales::percent_format(accuracy = 1L)(perc)),
            family = "font",
            size = 16,
            alpha = 0.7) +
  labs(title = str_to_upper("Confirmed unprovoked shark attacks"),
       subtitle = subtitle,
       caption = "Data: en.wikipedia.org/wiki/Shark_attack",
       x = "Number of Attacks",
       y = NULL)

## Saving ----
ggsave("./2023/07_hazards.png",
       bg = bg,
       height = 9, width = 12)

system("open ./2023/07_hazards.png")

