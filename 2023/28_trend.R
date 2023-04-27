library(tidyverse)
library(readxl)
library(janitor)
library(tsibble)
library(fable)
library(scales)
library(ggtext)
library(PrettyCols)
library(showtext)
library(colorspace)

font_add("title", "./resources/BrunoAceSC-Regular.ttf")
font_add_google("Lato", "font")
showtext_auto()

download.file("https://www.eurocontrol.int/performance/data/download/xls/Airport_Traffic.xlsx",
              "./2023/data/Airport_Traffic.xlsx")

airports <- read_xlsx("./2023/data/Airport_Traffic.xlsx",
          sheet = "DATA") %>% 
  clean_names()

pc <- "#203354"
bg <- "#EFFFFF"

airports %>% 
  group_by(apt_name, apt_icao) %>% 
  summarise(flights = sum(flt_tot_1),
            .groups = "drop") %>% 
  arrange(desc(flights))

data <- airports %>% 
  filter(apt_icao %in% c("EDDF", "EHAM", "LEMD", "LFPG", "EGLL", "EDDM")) %>%  
  group_by(apt_name, month = yearmonth(flt_date)) %>% 
  summarise(flights = sum(flt_tot_1),
            .groups = "drop") %>% 
  as_tsibble(key = apt_name, index = month)


forecast <- data %>% 
  filter(month <= yearmonth("2020-02-01")) %>% 
  model(
    ets = ETS(flights)
  ) %>% 
  forecast(h = "3 years")


ggplot() +
  autolayer(forecast,
            level = c(95)) +
  geom_line(data = data,
            aes(x = month, y = flights),
            colour = pc) +
  scale_x_yearmonth(labels = scales::label_date(format = "%Y")) +
  scale_y_continuous(labels = scales::label_number()) +
  scale_colour_pretty_d("Neon") +
  scale_fill_pretty_d("Neon") +
  labs(
    x = NULL,
    y = NULL,
    title = "Will covid-19 have lasting effects?",
    subtitle = "Monthly number of flights on six major European airports: actuals numbers against pre-covid forecasts",
    caption = "Data: EUROCONTROL"
  ) +
  facet_wrap(~apt_name) +
  guides(
    colour = "none",
    fill = "none"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "font",
                        size = 40,
                        colour = pc),
    strip.text = element_text(family = "font",
                              face = "bold",
                              size = 45,
                              hjust = 0,
                              colour = pc),
    axis.text = element_text(family = "font",
                             size = 40,
                             colour = darken(pc, .3)),
    axis.text.x = element_text(hjust = -0.1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = darken(pc, 0.5),
                                    linetype = "dotted"),
    panel.background = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(family = "title",
                              size = 92,
                              colour = pc),
    plot.subtitle = element_text(family = "font",
                              size = 52,
                              lineheight = 0.7,
                              colour = pc,
                              margin = margin(t = 5, b = 10)),
    plot.margin = margin(20,20,20,20)
  )


## Saving ----
ggsave("./2023/28_trend.png",
       bg = bg,
       height = 9, width = 12)

system("open ./2023/28_trend.png")

fs::file_delete("./2023/data/Airport_Traffic.xlsx")
