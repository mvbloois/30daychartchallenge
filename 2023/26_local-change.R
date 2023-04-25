library(tidyverse)
library(janitor)
library(colorspace)
library(patchwork)
library(showtext)

font_add_google("Roboto Condensed", "condensed")
font_add_google("Roboto Serif", "robotos")
showtext_auto()

blue <- "#0981D1"
yellow <- "#FDB813"
hot <- "darkred"
cold <- "#6E9ABC"

df <- read_delim("./2023/data/etmgeg_260.zip",
                 delim = ",",
                 skip = 50) %>% 
  clean_names() %>%
  mutate(date = ymd(yyyymmdd),
         rain = parse_number(rh) / 10,
         sun  = parse_number(sq) / 10,
         mintemp = parse_number(tn) / 10,
         maxtemp = parse_number(tx) / 10) %>% 
  select(date, rain, sun, mintemp, maxtemp) %>% 
  filter(year(date) < 2023) %>% 
  filter(year(date) > 1905) %>% 
  group_by(year = year(date)) %>% 
  summarise(sum_rain = sum(rain, na.rm = TRUE),
            sum_sun = sum(sun, na.rm = TRUE),
            max_temp = max(maxtemp, na.rm = TRUE),
            min_temp = min(mintemp, na.rm = TRUE)) %>% 
  ungroup() 


theme_set(theme_minimal())

theme_update(
  text = element_text(family = "robotos",
                      size = 40),
  panel.grid.minor.x = element_blank(),
  plot.margin = margin(20,20,20,20)
)

plt_sun <- df %>% 
  ggplot(aes(x = year, y = sum_sun)) +
  geom_text(aes(x = 1903, y = 2000, label = "SUNSHINE"),
            family = "condensed",
            color = "grey80",
            fontface = "bold",
            hjust = "left",
            size  = 30) +
  geom_smooth(colour = darken(yellow, 0.3)) +
  geom_point(aes(x = year, y = sum_sun),
             colour = yellow) +
  scale_x_continuous(position="top") +
  scale_y_continuous(limits = c(1200, 2400),
                     labels = scales::label_number(suffix = " hrs")) +
  labs(
    x = NULL,
    y = NULL
    )

plt_rain <- df  %>% 
  ggplot(aes(x = year, y = sum_rain)) +
  geom_text(aes(x = 1903, y = 1200, label = "RAINFALL"),
            family = "condensed",
            color = "grey80",
            fontface = "bold",
            hjust = "left",
            size  = 30) +
  geom_smooth(colour = darken(blue, 0.3)) +
  geom_point(aes(x = year, y = sum_rain), 
             colour = blue,
             size = 1) +
  scale_y_continuous(limits = c(400, 1400),
                     labels = scales::label_number(suffix = " mm")) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme(
    axis.text.x = element_blank()
  )

plt_temp <- df  %>% 
  ggplot() +
  geom_text(aes(x = 1903, y = 10, label = "TEMPERATURES"),
            family = "condensed",
            color = "grey80",
            fontface = "bold",
            hjust = "left",
            size  = 30) +
  geom_smooth(aes(x = year, y = max_temp), colour = darken(hot, 0.3)) +
  geom_point(aes(x = year, y = max_temp),
             colour = hot) +
  geom_smooth(aes(x = year, y = min_temp), colour = darken(cold, 0.3)) +  
  geom_point(aes(x = year, y = min_temp),
             colour = cold) +
  scale_y_continuous(limits = c(-30, 40),
                     labels = scales::label_number(suffix = " °C")) +
  labs(
    x = NULL,
    y = NULL
  )

plt_sun / plt_rain / plt_temp +
  plot_annotation(
    title = "SLOWLY BUT SURELY",
    subtitle = str_wrap("Annual amount of sunshine and rainfall, and the yearly minimum and maximum temperatures in De Bilt (Netherlands) 1906-2022", 87),
    caption = "Data: KNMI",
    theme = theme(
      plot.title = element_text(size = 90),
      plot.subtitle = element_text(size = 50,
                                   lineheight = 0.4)
    )
  )

## Saving ----
ggsave("./2023/26_local-change.png",
       bg = "white",
       height = 9, width = 12)

system("open ./2023/26_local-change.png")  
