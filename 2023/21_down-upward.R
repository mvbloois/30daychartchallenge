library(tidyverse)
library(ggthemes)
library(showtext)

font_add(family = "font", "./resources/milo-subset-md.woff.ttf")
showtext_auto()

blue <- "#0057b7"
yellow <- alpha("#ffd700", 0.2)

oeso <- c("AUS", "BEL", "CAN", "CHL", "DNK", "DEU", "EST", "FIN", "FRA", "GRC" ,"HUN", "IRL", "ISL", "ISR", "ITA",
          "JPN", "KOR", "LUX", "MEX", "NLD", "NZL", "NOR", "AUT", "POL", "PRT", "SVK", "ESP",
          "CZE", "TUR", "GBR", "USA", "SWE", "CHE") 

pal <- c("#FF0032", "#439200", "#00A0D2", "#6F68FF")

df <- read_csv("./2023/data/annual-working-hours-vs-gdp-per-capita-pwt.csv") %>% 
  janitor::clean_names() %>% 
  filter(code %in% oeso)%>% 
  filter(year > 1949) %>% 
  select(country = entity, code, year, annual_working_hours_per_worker,
         gdp_per_capita_output_multiple_price_benchmarks) %>% 
  drop_na() %>% 
  arrange(country, year) %>% 
  group_by(country)

endpoints <- df %>% 
  group_by(code) %>% 
  filter(year == max(year))

ggplot() +
  geom_path(data = df,
            aes(x = gdp_per_capita_output_multiple_price_benchmarks,
                y = annual_working_hours_per_worker,
                group = country),
            colour = "grey80") +
  geom_point(data = endpoints,
             aes(x = gdp_per_capita_output_multiple_price_benchmarks,
                 y = annual_working_hours_per_worker),
             colour = "grey80") +
  geom_path(data = df %>% filter(code %in% c("BEL", "USA", "KOR", "CHL")),
            aes(x = gdp_per_capita_output_multiple_price_benchmarks,
                y = annual_working_hours_per_worker,
                colour = country),
            size = 0.8
            ) +
  geom_point(data = endpoints %>% filter(code %in% c("BEL", "USA", "KOR", "CHL")),
             aes(x = gdp_per_capita_output_multiple_price_benchmarks,
                 y = annual_working_hours_per_worker,
                 colour = country),
             size = 2
             ) +
  scale_x_continuous(labels = scales::label_dollar()) +
  scale_y_continuous(limits = c(1000, 3000), 
                     labels = scales::label_comma(suffix = " h")) + 
  scale_colour_manual(name = "Country", values = pal) +
  labs(
    title = str_to_upper("As per capita GDP goes up, working hours go down"),
    subtitle = "",
    caption = "Data: Our World In Data",
    x = "GDP per capita",
    y = "Annual working hours per worker"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "font",
                        colour = blue,
                        size = 50),
    plot.background = element_rect(fill = yellow, colour = yellow),
    plot.title.position = "plot",
    plot.title = element_text(size = 92,
                              hjust = 0.5),
    plot.subtitle = element_text(size = 56,
                                 margin = margin(t = 10, b = 10),
                                 hjust = 0),
    plot.caption = element_text(margin = margin(t = 15)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(colour = blue),
    axis.line = element_line(colour = blue),
    axis.ticks = element_line(colour = blue),
    axis.ticks.length=unit(.20, "cm"),
    legend.position = c(.75, .75),
    legend.text = element_text(size = 60),
    legend.title = element_text(size = 65,
                                vjust = -5),
    plot.margin = margin(b = 20, t = 20, r = 30, l = 30)
  )

## Saving ----
ggsave("./2023/21_down-upward.png",
       bg = "white",
       height = 9, width = 12)

system("open ./2023/21_down-upward.png")

