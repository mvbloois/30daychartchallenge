library(tidyverse)
library(ggimage)
library(showtext)
library(rvest)


## Fonts ----
font_add_google("Atomic Age", "font")
font_add_google("Fira Sans", "subfont")
showtext_auto()

## https://www.svgrepo.com/
image <- "./2023/data/nuclear-bomb-svgrepo-com.svg"
image_txt <- paste(readLines(image), collapse = "\n")

df <-
  read_html("https://www.atomicarchive.com/almanac/test-sites/testing-chronology.html") %>%
  html_element("table") %>%
  html_table() %>%
  select(-Total) %>%
  filter(Country != "Totals") %>%
  pivot_longer(
    cols = -Country,
    names_to = "period",
    names_transform = list(period = as.character),
    values_to = "tests"
  ) %>%
  mutate(
    Country = str_replace(Country, "\u00A0", " "),
    country = factor(
      Country,
      levels = c(
        "Pakistan",
        "India",
        "France",
        "United States",
        "Russia/USSR",
        "United Kingdom",
        "China",
        "North Korea"
      )
    ),
    period = factor(period)
  ) 

periods <- as.character(unique(df$period))

df_2 <- df %>% 
  group_by(country) %>% 
  summarise(total_tests = sum(tests),
            .groups = "drop") %>% 
  mutate(label = glue::glue("{total_tests} tests"))

df %>% 
  filter(tests > 0) %>% 
  ggplot(aes(x = as.numeric(period), y = as.numeric(country))) +
  geom_image(aes(y = as.numeric(country) - 0.1, image = image, size = tests)) +
  geom_text(aes(x = 0, y = as.numeric(country), label = country),
            family = "font",
            size = 14,
            hjust = "right",
            fontface = "bold",
            colour = "#EE4B2B") +
  geom_text(data = df_2,
            aes(x = 0, y = as.numeric(country) - 0.3, label = label),
            family = "font",
            size = 12,
            hjust = "right",
            colour = "#EE4B2B") +
  scale_size_continuous(range = c(0.02, 0.1), guide = 'none') +
  scale_x_continuous(limits = c(-1, NA),
                     breaks = 1:8,
                     labels = periods) +
  scale_y_continuous(limits = c(0.5, NA)) +
  labs(
    title = str_to_upper("Nuclear Testing"),
    subtitle = str_wrap("Since the first nuclear test explosion on July 16, 1945, at least eight nations have detonated 2,056 nuclear test explosions at dozens of test sites, including Lop Nor in China, the atolls of the Pacific, Nevada, Algeria where France conducted its first nuclear device, western Australia where the U.K. exploded nuclear weapons, the South Atlantic, Semipalatinsk in Kazakhstan, across Russia, and elsewhere.", 100),
    caption = "Data: https://www.atomicarchive.com/",
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = "font",
                        size = 40,
                        colour = "#EE4B2B"),
    plot.background = element_rect(fill = "#343231"),
    plot.title = element_text(size = 102),
    plot.subtitle = element_text(family = "subfont",
                                 size = 45,
                                 lineheight = 0.3),
    axis.text.x = element_text(),
    plot.margin = margin(b = 20, t = 20, r = 20, l = 20)
  )
  
## Saving ----
ggsave("./2023/19_anthropocene.png",
       bg = "#343231",
       height = 9, width = 11)

system("open ./2023/19_anthropocene.png")
