library(tidyverse)
library(janitor)
library(scales)
# install.packages('devtools')
# devtools::install_github('bbc/bbplot')
library(bbplot)

df <- read_delim("./2023/data/tour_dem_tttot_tabular.tsv.gz") %>% 
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
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  geom_text(aes(x = "2020", y = 3.4e6,
                 label = "In 2020 travel restrictions caused a fall\nof the number of trips abroad of 65%"),
             size = 7.5,
             colour = "#FAAB18"
             ) +
  geom_text(aes(x = "2021", y = 11e6,
                label = "Domestic trips\nare up by 26%"),
            size = 7.5,
            colour = "#1380A1"
  ) +
  scale_colour_manual(values = c("#1380A1", "#FAAB18")) +
  scale_y_continuous(limits = c(0,NA), 
                     breaks = c(5e6, 10e6, 15e6),
                     labels = number_format(scale = 1/1e6,
                                            suffix = "m")) +
  bbc_style() +
  labs(title = "Going Dutch in times of covid-19",
       subtitle = "Personal domestic and international trips over 3 nights 2016-2021",
       caption = "Source: Eurostat") +
  theme(axis.text.y = element_text(vjust = -0.1),
        panel.grid.major.y = element_line(color="#cbcbcb"), 
        panel.grid.major.x =element_blank(),
        plot.caption = element_text(size = 14,
                                    hjust = 0)
        )

## Saving ----
ggsave("./2023/12_bbc-news.png",
       bg = "white",
       height = 9, width = 12)

system("open ./2023/12_bbc-news.png")      
