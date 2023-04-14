library(tidyverse)
library(ggforce)
library(janitor)
library(data.table)
library(showtext)
library(patchwork)

bg <- "#DBCECA"
iceblue <- "darkblue"
hotred <- "#B7121F"
mercury <- "grey40"

## Fonts ----
font_add_google("Merriweather", "font")
showtext_auto()

theme_set(theme_minimal())

theme_update(
  text = element_text(family = "font",
                      size = 40),
  plot.background = element_rect(fill = bg,
                                 colour = bg),
  plot.title.position = "plot",
  plot.title = element_text(size = 92,
                            hjust = 0.5),
  plot.subtitle = element_text(size = 48,
                               hjust = 0.5),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  axis.text.x = element_blank(),
  plot.margin = margin(b = 20, t = 20, r = 20, l = 20)
)

db_raw <-
  read_delim("./2023/data/etmgeg_260.zip", delim = ",", skip = 50) %>%
  clean_names()

cold_streaks <- db_raw %>% select(
  date = yyyymmdd,
  temp = tg
) %>%
  mutate(
    date = lubridate::ymd(date),
    temp = parse_number(temp),
    temp = temp / 10,
    decade = glue::glue("{year(date) %/% 10}0's"),
    freezing = temp < -5 
  ) %>% 
  group_by(decade, freezing, streak = rleid(freezing)) %>% 
  summarise(streak_length = n(),
            .groups = "drop") %>% 
  select(-streak) %>% 
  group_by(decade, freezing) %>% 
  filter(streak_length == max(streak_length) & freezing) %>% 
  distinct() %>% 
  mutate(type = "Cold streak (< 5 C)")

warm_streaks <- db_raw %>% select(
  date = yyyymmdd,
  temp = tg
) %>%
  mutate(
    date = lubridate::ymd(date),
    temp = parse_number(temp),
    temp = temp / 10,
    decade = glue::glue("{year(date) %/% 10}0's"),
    warm = temp > 20 
  ) %>% 
  group_by(decade, warm, streak = rleid(warm)) %>% 
  summarise(streak_length = n(),
            .groups = "drop") %>% 
  select(-streak) %>% 
  group_by(decade, warm) %>% 
  filter(streak_length == max(streak_length) & warm) %>% 
  distinct()%>% 
  mutate(type = "Warm streak (> 20 C)")


plot_data <- db_raw %>% select(
    date = yyyymmdd,
    temp = tg
  ) %>%
  mutate(
    date = lubridate::ymd(date),
    temp = parse_number(temp),
    temp = temp / 10,
    decade = year(date) %/% 10
  ) %>% 
  group_by(decade) %>% 
  summarise(min = min(temp),
            avg = mean(temp),
            max = max(temp),
            .groups = "drop") %>% 
  mutate(idx = 0,
         decade = glue::glue("{decade}0's"))

centre <- -20

p1 <- plot_data %>% 
  ggplot() +
  geom_circle(aes(x0 = idx, y0 = centre, r = 2),
              fill = mercury, colour = mercury) +
  geom_rect(aes(xmin = idx - 0.8, xmax = idx + 0.8,
                ymin = centre, ymax = 35),
            fill = mercury,
            colour = mercury) +
  geom_link(aes(x = idx, xend = idx,
                y = min, yend = max,
                colour = after_stat(y)),
            size = 3) +
  geom_segment(aes(x = idx - 0.8, xend = idx + 0.8,
                y = avg, yend = avg)) +
facet_wrap(~decade, nrow = 1) +
  coord_equal() +
  scale_y_continuous(breaks = seq(from = -20, to = 30, by = 10),
                     labels = scales::label_number(suffix = " C")) +
  scale_colour_gradient2(low = iceblue, mid = "white", high = hotred, midpoint = 0) +
  labs(
    title = "Ice skating or swimming?",
    subtitle = "Minimum and maximum daily mean temperatures per decade in De Bilt",
    caption = "Data: Koninklijk Nederlands Meteorologisch Instituut",
    x = NULL, y = NULL) +
  guides(colour = "none")
 
## Saving ----
ggsave("./2023/15_positive-negative.png",
       bg = bg,
       height = 9, width = 9)

system("open ./2023/15_positive-negative.png")

