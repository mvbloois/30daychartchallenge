library(tidyverse)
library(showtext)

font_add_google("Truculenta", "truculenta")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()

coffee_prices <-
  read_csv("./2024/data/coffee-prices-historical-data.csv") |>
  drop_na() |>
  mutate(date = ymd(date))

coups <- tibble(date = c("1976-11-10", "1987-09-03", "1996-07-25", "2015-05-14")) |>
  mutate(date = ymd(date))

coffee_prices |>
  ggplot(aes(x = date, y = value, group = 1)) +
  geom_line(colour = "darkgreen",
            linewidth = 0.4) +
  geom_vline(xintercept = coups$date,
             colour = "darkred") +
  geom_text(
    data = coups,
    aes(
      x = date + months(3),
      y = -0.1,
      label = date
    ),
    size = 12,
    colour = "darkred",
    hjust = 0
  ) +
  annotate(
    "text",
    x = ymd("1976-03-01"),
    y = -0.1,
    label = "Coups:",
    size = 12,
    colour = "darkred",
    hjust = 1
  ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    x = NULL,
    y = "Coffee Price ($ per pound)",
    title = "HISTORICAL",
    subtitle = str_wrap("Burundi's primary exports are coffee and tea, which account for 90% of foreign exchange earnings. In this chart historical coffee prices are juxtaposed with coups d'état in Burundi.", 60),
    caption = "Source: Kaggle, Wikipedia"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "robotoslab",
                        size = 42),
    legend.position = "none",
    axis.title = element_text(colour = "darkgreen"),
    plot.title.position = "plot",
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      family = "truculenta",
      face = "bold",
      size = 88,
      hjust = 0
    ),
    plot.subtitle = element_text(size = 54,
                                 lineheight = 0.35,
                                 hjust = 0),
    plot.caption = element_text(size = 42,
                                hjust = 1),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(
  "./2024/R/15_historical.png",
  bg = "white",
  width = 8,
  height = 6,
)

system("open ./2024/R/15_historical.png")

