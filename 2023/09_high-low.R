library(tidyverse)
library(tidyquant)
library(patchwork)
library(scales)
library(showtext)

bg <- "#293133"
stone <- "#918E85"
red <- "#D2222D"
green <- "#238823"

## Fonts ----
font_add_google("Fira Code", "font")
showtext_auto()

## Theme ----
theme_set(theme_minimal())

theme_update(
    text = element_text(family = "font",
                        size = 62,
                        colour = stone),
    axis.text = element_text(colour = stone),
    axis.text.x = element_text(size = 32),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = stone),
    plot.background = element_rect(colour = bg,
                                   fill = bg)
  )

df <- getSymbols("ASML", from = "2022-10-01", to = "2023-03-31", auto.assign = FALSE) %>%
  broom::tidy() %>% #https://stackoverflow.com/questions/54888828/what-is-the-best-way-to-transform-an-xts-into-a-tibble
  mutate(series = str_remove(series, "ASML\\.")) %>% 
  pivot_wider(names_from = series, values_from = value) %>%
  mutate(colour = ifelse(Close > lag(Close), green, red)) %>%
  drop_na()

price <- df %>% 
  ggplot(aes(x = index)) +
  geom_segment(aes(
    x = index,
    xend = index,
    y = High,
    yend = Low
  ),
  colour = "white") +
  geom_rect(aes(
    xmin = index - 0.3,
    xmax = index + 0.3,
    ymin = Close,
    ymax = Open,
    fill = colour
  )
  ) +
  scale_y_continuous(labels = number_format(big.mark = ",", prefix = "$ ")) +
  scale_fill_identity() +
  labs(x = NULL,
       y = "price") +
  theme(
    axis.text.x = element_blank()
  )


vol <- df %>% 
  ggplot(aes(x = index, y = Volume, fill = colour)) +
  geom_col() +
  scale_x_date(date_breaks = "month") +
  scale_y_continuous(labels = number_format(scale = 1/1e6,
                                            big.mark = ",", suffix = "M")) +
  scale_fill_identity() +
  labs(x = NULL,
       y = "volume")

layout <- "
AAAAAA
AAAAAA
AAAAAA
BBBBBB
"

price / vol +
  plot_layout(design = layout) +
  plot_annotation(
    title = "ASML",
    subtitle = "October 1, 2022 - March 31, 2023",
    caption = "Data: Yahoo"
  ) &
  theme(
    plot.title = element_text(colour = "white",
                              size = 82),
    plot.subtitle = element_text(colour = "white",
                                 size = 55),
    plot.caption = element_text(size = 32)
  )

## Saving ----
ggsave("./2023/09_high-low.png",
       bg = bg,
       height = 9, width = 12)

system("open ./2023/09_high-low.png")
