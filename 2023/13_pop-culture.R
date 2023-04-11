## https://www.kaggle.com/datasets/sberj127/kpop-hits-through-the-years?resource=download

library(tidyverse)
library(fs)
library(janitor)
library(patchwork)
library(ggtext)
library(showtext)

font_add(family = "titlefont", "./resources/distortion.ttf")

font_add(family = "kai", "./resources/abeatbyKaiRegular.otf")
showtext_auto()

read_kpop <- function(filename) {
  read_csv(filename) %>% mutate(year = str_remove_all(filename, "./2023/data/kpop/KPopHits|.csv"))
}

df <- map(dir_ls(path = "./2023/data/kpop"), ~read_kpop(.)) %>% 
  reduce(bind_rows) %>%
  clean_names() 

keys <- c("A", "Bb", "B", "C","C#", "D", "Eb", "E", "F", "F#", "G", "G#")

kpop <- df %>% 
  mutate(key = case_when(key == 0 ~ "C",
                         key == 1 ~ "C#",
                         key == 2 ~ "D",
                         key == 3 ~ "Eb",
                         key == 4 ~ "E",
                         key == 5 ~ "F",
                         key == 6 ~ "F#",
                         key == 7 ~ "G",
                         key == 8 ~ "G#",
                         key == 9 ~ "A",
                         key == 10 ~ "Bb",
                         key == 11 ~ "B"),
         key = factor(key, levels = keys),
         year = ifelse(year == "90s", "1990s", year) %>% factor(),
         time = format(as.POSIXct(Sys.Date()) + duration_ms / 1000, "%M:%S"),
         mode = ifelse(mode == 1, "major", "minor"))

theme_set(theme_minimal())

theme_update(
    text = element_text(family = "kai",
                        size = 30),
    plot.background = element_rect(fill = cosmiclatte,
                                   colour = cosmiclatte),
    plot.title.position = "plot",
    plot.title = element_text(family = "kai",
                              size = 50)
    )

burntred <- "#AB0033"
slowblue <- "#42B4E6"
cosmiclatte <- "#FFF8E7"
omg1 <- "#FEAC5E"
omg2 <- "#C779D0"
omg3 <- "#4BC0C8"

outliers <- kpop %>% 
  filter(duration_ms == min(duration_ms) | duration_ms == max(duration_ms)) %>% 
  select(title, artist_s, duration_ms, year) %>% 
  mutate(duration = format(as.POSIXct(Sys.Date()) + duration_ms / 1000, "%M:%S"),
         label = glue::glue("{title} by {artist_s}"),
         x = ifelse(duration_ms == min(duration_ms), "2003", "2016"),
         y = ifelse(duration_ms == min(duration_ms), duration_ms + 25000, duration_ms - 25000),
         )

plt_1 <- kpop %>% 
  ggplot(aes(x = year, y = duration_ms)) +
  geom_point(position = position_jitter(0.1),
             colour = burntred, alpha = 0.5) +
  geom_smooth(aes(x = as.numeric(year), y = duration_ms),
              colour = slowblue, se = FALSE) +
  geom_text(data = outliers,
            aes(x = x, y = y, label = label),
            colour = "black",
            size = 8,
            family = "kai") +
  scale_x_discrete(breaks = unique(kpop$year)[seq(1, length(unique(kpop$year)), by = 2)]) +
  scale_y_continuous(breaks = seq(from = 6e4, to = 6e05, by = 6e4),
                     labels = ~format(as.POSIXct(Sys.Date()) + seq(from = 6e4, to = 6e05, by = 6e4) / 1000, "%M:%S")) +
  labs(title = "Songs significantly shorter over time",
       x = NULL,
       y = "duration") 


st2 <- glue::glue("Both <span style=\"color:{omg1}\">major</span> and <span style=\"color:{omg2}\">minor</span> keys are used")

plt_2 <- kpop %>% 
  ggplot(aes(x = key, y = danceability)) +
  geom_violin(colour = omg3, fill = omg3, alpha = 0.3) +
  geom_point(aes(colour = mode)) +
  scale_color_manual(values = c("major" = omg1, "minor" = omg2)) +
  ylim(c(0,1)) +
  labs(title = "No clear preference for a particular key",
       subtitle = st2,
       x = NULL) +
  guides(colour = "none") +
  theme(
    plot.subtitle = element_markdown()
  )

plt_3 <- kpop %>% 
  ggplot(aes(x = valence, y = danceability)) +
  geom_point(colour = burntred, alpha = 0.5) +
  geom_smooth(colour = slowblue, se = FALSE) +
  labs(x = "positivity conveyed by track") +
  ylim(c(0,1)) +
  labs(title = "Positive songs are often more danceable") +
  guides(colour = "none")

plt_4 <- kpop %>% 
  ggplot(aes(x = speechiness, y = danceability)) +
  geom_point(colour = burntred, alpha = 0.5) +
  geom_smooth(colour = slowblue, se = FALSE) +
  scale_x_log10() +
  labs(x = "speechiness of track") +
  ylim(c(0,1)) +
  labs(title = "Danceability and speechiness go hand in hand") +
  guides(colour = "none")

(plt_3 + plt_4) / (plt_1 + plt_2) +
  plot_layout(widths = c(2, 2)) +
  plot_annotation(
    title = "Hits of K-Pop through the years",
    caption = "Data: Kaggle, maintained by Sandra Angela Berjamin",
    theme = theme(
    plot.title = element_text(family = "titlefont",
                              size = 200,
                              colour = burntred)
    )
  ) &
  theme(plot.background = element_rect(fill = cosmiclatte,
                                       colour = cosmiclatte),
        plot.margin = margin(b = 20, t = 20, r = 30, l = 30)
        )

## Saving ----
ggsave("./2023/13_pop-culture.png",
       bg = cosmiclatte,
       height = 9, width = 12)

system("open ./2023/13_pop-culture.png")
