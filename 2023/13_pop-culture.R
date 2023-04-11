## https://www.kaggle.com/datasets/sberj127/kpop-hits-through-the-years?resource=download

library(tidyverse)
library(fs)
library(janitor)
library(patchwork)
library(showtext)

font_add(family = "North Carossela", "./resources/North Carossela.otf")
showtext_auto()


list_of_fonts <- as.data.frame(font_files())
read_kpop <- function(filename) {
  read_csv(filename) %>% mutate(year = str_remove_all(filename, "./2023/data/kpop/KPopHits|.csv"))
}

df <- map(dir_ls(path = "./2023/data/kpop"), ~read_kpop(.)) %>% 
  reduce(bind_rows) %>%
  clean_names() 

kpop <- df %>% 
  rename(is_major = mode) %>%  
  mutate(key = case_when(key == 0 ~ "C",
                         key == 1 ~ "C_sharp",
                         key == 2 ~ "D",
                         key == 3 ~ "E_flat",
                         key == 4 ~ "E",
                         key == 5 ~ "F",
                         key == 6 ~ "F_sharp",
                         key == 7 ~ "G",
                         key == 8 ~ "G_sharp",
                         key == 9 ~ "A",
                         key == 10 ~ "B_flat",
                         key == 11 ~ "B"),
         year = ifelse(year == "90s", "1990s", year) %>% factor(),
         time = format(as.POSIXct(Sys.Date()) + duration_ms / 1000, "%M:%S"))
burntred <- "#AB0033"
slowblue <- "#42B4E6"


kpop %>% 
  count(artist_s, sort = TRUE)

kpop %>% 
  ggplot(aes(x = year, y = duration_ms)) +
  geom_point(position = position_jitter(0.1),
             colour = burntred, alpha = 0.5) +
  geom_smooth(aes(x = as.numeric(year), y = duration_ms),
              colour = slowblue) +
  scale_y_continuous(breaks = seq(from = 6e4, to = 6e05, by = 6e4),
                     labels = ~format(as.POSIXct(Sys.Date()) + seq(from = 6e4, to = 6e05, by = 6e4) / 1000, "%M:%S")) +
  labs(title = "Songs significantly shorter over time",
       y = "duration") +
  theme_minimal() +
  theme(text = element_text(family = "North Carossela"))
