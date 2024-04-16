library(tidyverse)
library(janitor)
library(showtext) 

font_add_google("Truculenta", "truculenta")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()

nl_map <- map_data(map = "world", region = c("Netherlands"))

stations <- tibble(
  station = c("De Kooy", "Maastricht"),
  long = c(4.7869, 5.7713),
  lat = c(52.9223, 50.9136)
)

map <- ggplot()+
  geom_polygon(data = nl_map,
               aes(x=long, y=lat, subgroup=group), 
               fill = "darkgreen",
               colour = "white",
               size=0.2,
               alpha=0.4) +
  geom_point(data = stations,
             aes(x=long, y=lat),
             size = 1) +
  geom_text(data = stations,
            aes(x=long, y=lat, label = station),
            family = "robotoslab",
            hjust = 1.2) +
  theme_void()


# Download and read file
download.file(
  "https://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/daggegevens/etmgeg_235.zip",
  "./2024/data/etmgeg_235.zip"
)

download.file(
  "https://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/daggegevens/etmgeg_380.zip",
  "./2024/data/etmgeg_380.zip"
)



kooy <-
  read_delim("./2024/data/etmgeg_235.zip", delim = ",", skip = 50) |>
  clean_names() |> 
  mutate(station = "De Kooy") 

maas <-
  read_delim("./2024/data/etmgeg_380.zip", delim = ",", skip = 50) |>
  clean_names()|> 
  mutate(station = "Maastricht")

fs::file_delete("./2024/data/etmgeg_235.zip")
fs::file_delete("./2024/data/etmgeg_380.zip")

plot_data <- bind_rows(
  kooy,
  maas
) |> 
  mutate(date = ymd(yyyymmdd),
         rh = if_else(rh == "   -1", "    0" , rh),
         rain = parse_number(rh) / 10) |> 
  select(station, date, rain) |> 
  group_by(station) |> 
  mutate(yr_tot = slider::slide_dbl(rain, .f = sum, .before = 359, .complete = TRUE)) |> 
  ungroup() |> 
  drop_na() 

p <- plot_data |> 
  filter(day(date) == 1) |> 
  ggplot(aes(x = date, y = yr_tot, colour = station)) +
  geom_line(alpha = 0.8) +
  scale_y_continuous(limits = c(0,NA), label = scales::number_format(suffix = " mm")) +
  scale_colour_manual(values = c("darkred", "darkgreen"))+
  labs(x = NULL,
       y = NULL,
       title = "WEATHER",
       subtitle = "Yearly total rainfall in two places in the Netherlands (moving 360-day total).",
       caption = "Source: KNMI",
       colour = NULL) +
  theme_minimal() +
  theme(
    text = element_text(family = "robotoslab",
                        size = 28),
    plot.title.position = "plot",
    plot.title = element_text(family = "truculenta",
                              face = "bold",
                              size = 62),
    legend.position = "top",
    legend.justification = "left"
  )

p + patchwork::inset_element(map, 0.9, 0.7, 1, 1, align_to = "full")
 
ggsave(
  "./2024/R/16_weather.png",
  bg = "white",
  width = 8,
  height = 4,
)

system("open ./2024/R/16_weather.png") 
