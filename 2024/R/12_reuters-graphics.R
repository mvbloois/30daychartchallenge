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

plot_data %>% 
  ggplot(aes(x = year, y =trips, group = c_dest)) +
  geom_line(aes(colour = c_dest), linewidth = 1.2) +
  #geom_hline(yintercept = 0, linewidth = 0.8, colour="#333333") +
  scale_colour_manual(values = c("#308ecb", "#8b6fab")) +
  scale_x_discrete(breaks = c(2017, 2018, 2019, 2020, 2021, 2022)) +
  scale_y_continuous(limits = c(0,NA), 
                     breaks = c(0, 5e6, 10e6, 15e6),
                     labels = number_format(scale = 1/1e6,
                                            suffix = "m")) +

  labs(x = NULL, y = NULL,
       title = "Dutch trips: domestic and abroad",
       caption = "Source: Eurostat") +
  theme(
    text = element_text(family = "font",
                        size = 32),
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
    #axis.text.x = element_text(vjust = 1.5),
    axis.text.y = element_text(vjust = -1.5,
                               hjust = 4),
    #axis.ticks.length.x = unit(5, "pt"),
    axis.ticks.x = element_line(linewidth = 0.3),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 62,
                              colour = "grey30"),
    plot.caption = element_text(size = 28,
                                colour = "grey30",
                                hjust = 0),
    plot.margin = margin(10, 40, 10, 0)
  )

## Saving ----
ggsave("./2024/R/12_reuters-graphics.png",
       bg = "white",
       height = 6, width = 7)

system("open ./2024/R/12_reuters-graphics.png")      
