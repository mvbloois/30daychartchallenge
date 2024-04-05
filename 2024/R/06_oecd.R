
library(tidyverse)
library(gghighlight)
library(showtext)

font_add(family = "font", "./resources/OpenDyslexic3-Regular.ttf")
font_add(family = "bold", "./resources/OpenDyslexic3-Bold.ttf")
showtext_auto()

plot_data <- read_tsv("./2024/data/06_oecd.tsv") |> 
  janitor::clean_names() |> 
  fill(year_study, .direction =  "down") |> 
  select(-starts_with("x")) |> 
  mutate(average = as.numeric(average)) |> 
  drop_na() |> 
  group_by(jurisdiction) |> 
  mutate(min = min(average),
         max = max(average)) |> 
  ungroup()

hl <- c("Iceland", "Finland", "Sweden", "Denmark", "Norway", "Netherlands")

plot_data |> 
  ggplot(aes(x = year_study, y = average,
             colour = jurisdiction)) +
  geom_line(aes(group = jurisdiction)) +
  gghighlight(jurisdiction %in% hl,
              use_direct_label = FALSE) +
  facet_wrap(~jurisdiction) +
  scale_y_continuous(limits = c(0, 600),
                     breaks = c(0,100,200,300,400,500,600)) +
  scale_colour_manual(values = rep("red",6)) +
  labs(x = NULL,
       y = NULL,
       title = "PISA Reading Scales",
       subtitle = str_wrap("The Programme for International Student Assessment, conducted by the OECD, evaluates the abilities of 15-year-olds in reading, mathematics, and science to tackle real-world challenges. Several countries in Europe see a notable decline in reading skills.", 70),
       caption = "Data: OECD, PISA") +
  theme_minimal() +
  theme(
    text = element_text(family = "font"),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    strip.text = element_text(hjust = 0),
    plot.title.position = "plot",
    plot.title = element_text(family = "bold",
                              size = 32),
    plot.margin = margin(10, 10, 10, 10)
  )

