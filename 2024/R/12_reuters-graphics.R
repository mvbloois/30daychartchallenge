library(tidyverse)
library(janitor)
library(scales)
library(showtext) 

font_add_google("Source Sans 3", "font")
showtext_auto()


df <- read_delim("./2024/data/tour_dem_tttot_tabular.tsv.gz") %>% 
  clean_names() %>% 
  rename(col_1 = 1) %>% 
  separate_wider_delim(cols = col_1, delim = ",", names = c("freq", "c_dest", "purpose", "duration", "unit", "geo")) %>% 
  mutate(across(starts_with("x"), ~str_replace(., ":", "NA")),
         across(starts_with("x"), ~str_remove_all(., " |[a-z]")),
         across(starts_with("x"), parse_integer)) %>%
  pivot_longer(cols = starts_with("x"), 
               names_to = "year",
               names_prefix = "x", 
               values_to = "trips") 

plot_data <- df %>% 
  filter(geo == "NL") %>% 
  filter(c_dest != "WORLD") %>% 
  filter(purpose == "PER") %>%
  filter(duration == "N_GE4") %>% 
  filter(year >= "2016") %>% 
  mutate(c_dest = case_when(c_dest == "DOM" ~ "Domestic",
                            c_dest == "FOR" ~ "International",
                            TRUE ~ "xx"))

labs_y <- tibble(
  x = rep("2015", 4),
  y = c(0 + 3e5, 5e6+3e5, 10e6+3e5, 15e6+3e5),
  lbl = c("0", "5", "10", "15 milion")
)

plot_data %>% 
  ggplot(aes(x = year, y =trips, group = c_dest)) +
  geom_line(aes(colour = c_dest), linewidth = 1.5) +
  scale_colour_manual(values = c("#308ecb", "#8b6fab")) +
  scale_x_discrete(breaks = c(2017, 2018, 2019, 2020, 2021, 2022)) +
  scale_y_continuous(limits = c(0,NA), 
                     breaks = c(0, 5e6, 10e6, 15e6),
                     labels = number_format(scale = 1/1e6,
                                            suffix = "m")) +
  geom_text(data = labs_y,
            aes(x = x, y = y, label = lbl),
            hjust = 0,
            size = 11,
            colour = "grey30",
            inherit.aes = FALSE) +
  labs(x = NULL, y = NULL,
       title = "Dutch trips: domestic and abroad",
       caption = "Source: Eurostat") +
  coord_cartesian(expand = FALSE) +
  theme(
    text = element_text(family = "font",
                        size = 36),
    plot.background = element_rect(fill = "white",
                                   colour = "white"),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    legend.text = element_text(size = 56),
    panel.background = element_rect(fill = "white",
                                    colour = "white"),
    panel.grid.major.y = element_line(color="#cbcbcb"), 
    panel.grid.major.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_line(linewidth = 0.3),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(colour = "grey30"),
    plot.title = element_text(size = 62,
                              colour = "grey30"),
    plot.caption = element_text(size = 32,
                                colour = "grey30",
                                hjust = 0),
    plot.margin = margin(20, 20, 20, 20)
  )

## Saving ----
ggsave("./2024/R/12_reuters-graphics.png",
       bg = "white",
       height = 6, width = 7)

system("open ./2024/R/12_reuters-graphics.png")      

