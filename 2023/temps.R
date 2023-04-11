library(tidyverse)
library(ggforce)

mercury <- "#DBCECA"

theme_set(theme_minimal())

theme_update(
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  axis.text.x = element_blank()
)

db_raw <-
  read_delim("./2023/data/etmgeg_260.zip", delim = ",", skip = 50) %>%
  clean_names()

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

plot_data %>% 
  ggplot() +
  geom_circle(aes(x0 = idx, y0 = centre, r = 2),
              fill = mercury, colour = mercury) +
  geom_rect(aes(xmin = idx - 0.8, xmax = idx + 0.8,
                   ymin = centre, ymax = 35),
            fill = mercury,
            colour = mercury) +
  geom_rect(aes(xmin = idx - 0.3, xmax = idx + 0.3,
                ymin = min, ymax = max)) +
  geom_segment(aes(x = idx - 0.8, xend = idx + 0.8,
                   y = avg, yend = avg)) +
  facet_wrap(~decade, nrow = 1) +
  scale_y_continuous(breaks = seq(from = -20, to = 30, by = 10),
                     labels = scales::label_number(suffix = "°C")) +
  labs(x = NULL, y = NULL) +
  coord_equal()

