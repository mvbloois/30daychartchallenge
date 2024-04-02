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

font_add_google("Oswald", "title")
font_add_google("Open Sans", "font")
showtext_auto()

# download.file("https://www.eurocontrol.int/performance/data/download/xls/Airport_Traffic.xlsx",
#               "./2024/data/Airport_Traffic.xlsx")

airports <- read_xlsx("./2024/data/Airport_Traffic.xlsx",
                      sheet = "DATA") %>% 
  clean_names()
	
bg <- lighten("#0056B9", 0.3)
pc <- "#FFD800"

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
  forecast(h = "4 years")


ggplot() +
  autolayer(forecast,
            level = c(95)) +
  geom_line(data = filter(data, month >= yearmonth("2017 jan")),
            aes(x = month, y = flights),
            colour = pc) +
  scale_x_yearmonth(breaks = c("2017-01-01", "2019-01-01", "2021-01-01", "2023-01-01") |> ymd(),
                    labels = scales::label_date(format = "%Y")) +
  scale_y_continuous(labels = scales::label_number()) +
  scale_fill_manual(values = rep(lighten(bg, 0.3), 6)) +
  scale_colour_manual(values = rep(lighten(bg, 0.1), 6)) +
  labs(
    x = NULL,
    y = NULL,
    title = "Major European airport still have fewer passengers than before covid-19",
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
                              hjust = 0.5,
                              colour = pc),
    axis.text = element_text(family = "font",
                             size = 40,
                             colour = lighten(pc, .3)),
    axis.text.x = element_text(hjust = -0.1),
    axis.text.y = element_text(vjust = -0.2),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = bg),
    panel.background = element_rect(fill = "#0056B9"),
    legend.position = "none",
    plot.title.position = "plot",
    plot.title = element_text(family = "title",
                              size = 92,
                              hjust = 0.5,
                              colour = pc),
    plot.subtitle = element_text(family = "font",
                                 size = 52,
                                 lineheight = 0.7,
                                 hjust = 0.5,
                                 colour = pc,
                                 margin = margin(t = 5, b = 15)),
    plot.caption = element_text(hjust = 0.5,
                                margin = margin(t = 15)),
    plot.margin = margin(20,20,20,20)
  )


## Saving ----
ggsave("./2024/R/03_makeover.png",
       bg = bg,
       height = 9, width = 12)

system("open ./2024/R/03_makeover.png")

# fs::file_delete("./2024/data/Airport_Traffic.xlsx")
