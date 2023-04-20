library(tidyverse)
library(janitor)
library(patchwork)
library(showtext)

font_add_google("Roboto Slab", "font")
showtext_auto()

df <- read_delim("./2023/data/etmgeg_260.zip",
                 delim = ",",
                 skip = 50) %>% 
  clean_names() %>%
  mutate(date = ymd(yyyymmdd),
         rain = parse_number(rh) / 10,
         sun  = parse_number(sq) / 10,
         temp = parse_number(tg) / 10) %>% 
  select(date, rain, sun, temp) %>% 
  group_by(year = year(date), month = month(date)) %>% 
  summarise(rain = sum(rain, na.rm = TRUE),
            sun  = sum(sun,  na.rm = TRUE),
            temp = mean(temp,  na.rm = TRUE)) %>% 
  filter(year < 2023) %>% 
  filter(year > 1922)

rain <- df %>% 
  ggplot(aes(x = year, y = month)) +
  geom_tile(aes(fill = rain),
            colour = "white") +
  scale_fill_gradient(low = alpha("#0981D1", 0.2), high = "#0981D1") +
  scale_y_reverse() +
  coord_equal() +
  labs(
    title = "One hunderd years of rain, sun and temperatures",
    subtitle = "The top left tile of each bar is January 1923, the bottom right tile is December 2022",
    ) +
  guides(fill = "none") +
  theme_void() +
  theme(
    plot.title = element_text(family = "font",
                              size = 80,
                              colour = "#023020",
                              hjust = 0.5,
                              vjust = 20),
    plot.subtitle = element_text(family = "font",
                                 size = 40,
                                 colour = "#023020",
                                 hjust = 0.5,
                                 vjust = 35),
  )
  
sun <- df %>% 
  ggplot(aes(x = year, y = month)) +
  geom_tile(aes(fill = sun),
            colour = "white") +
  scale_fill_gradient(low = alpha("#FDB813", 0.2), high = "#FDB813") +
  scale_y_reverse() +
  coord_equal() +
  guides(fill = "none") +
  theme_void()

temp <- df %>% 
  ggplot(aes(x = year, y = month)) +
  geom_tile(aes(fill = temp),
            colour = "white") +
  scale_fill_gradient(low = alpha("#E31919", 0.1), high = "#E31919") +
  scale_y_reverse() +
  coord_equal() +
  labs(caption = "Data: KNMI") +
  guides(fill = "none") +
  theme_void() +
  theme(
    plot.caption = element_text(family = "font",
                                size = 27,
                                colour = "#023020",
                                hjust = 0.5,
                                vjust = -60)
  )

rain / sun /temp 

## Saving ----
ggsave("./2023/23_tiles.png",
       bg = "white",
       height = 9, width = 12)

system("open ./2023/23_tiles.png")  
