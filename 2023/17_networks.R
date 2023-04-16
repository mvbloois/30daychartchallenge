library(tidyverse)

df <- readxl::read_xlsx("./2023/data/rail_tf_ns_nl_page_spreadsheet.xlsx",
                  sheet = 3,
                  skip = 9) %>% 
  rename(segment = 1,
         passengers = 2) %>%
  mutate(passengers = parse_number(passengers)) %>% 
  drop_na() %>%  
  filter(is.numeric(passengers)) %>% 
  separate(segment, into = c("to", "from"),
           sep = "-") %>% 
  mutate(across(to:from, str_squish))

stations <- tibble(
  station = unique(df$to, df$from)
) 
  filter(!str_detect(station, "Aansluiting")) %>% 
  head(10) %>% 
  mutate(x = tmaptools::geocode_OSM(paste("station", station))$coords[1],
         y = tmaptools::geocode_OSM(paste("station", station))$coords[2])

  
dd <- map(stations, ~tmaptools::geocode_OSM(paste("station", .x)))
 
stats <- dd[[1]] %>% 
  select(query, lat, lon) %>% 
  bind_rows(
    tribble(
      ~query, ~lat, ~lon,
      "Moordrecht Aansluiting", 52.0109, 4.6655,
      "Utrecht Lunetten Aansluiting", 52.0722, 5.1365,
      "Harmelen Aansluiting", 52.1046, 4.9586,
      "Amersfoort Schothorst", 52.1739, 5.4036,
      "Kijfhoek", 51.8324, 4.5964,
      "Venserpolder Aansluiting", 52.32667, 4.94728,
      "Singelgracht Aansluiting", 52.3858, 4.8861,
      "Weesp", 52.31265, 5.04362,
      "Amsterdam Riekerpolder", 52.33844, 4.82615,
      "Amsterdam Riekerpolder Aansluiting", 52.33844, 4.82615,
      "Gaasperdammerweg Aansluiting", 52.3340, 4.9968,
      "Hemtunnel Aansluiting", 52.4464, 4.8120,
      "Vught Aansluiting", 51.6611, 5.2935,
      "Hilversum", 52.22619, 5.18268,
      "Geleen Lutterade Dsm Aansluiting", 50.97587, 5.82530,
      "Onnen", 53.1572, 6.6340,
      "Roosendaal Grens", 51.4695, 4.4524,
      "S Heer Arendskerke Aansluiting", 51.49143, 3.83558,
      "Waterhuizen Aansluiting", 53.1858, 6.6436,
      "Eijsden Grens", 50.7591, 5.7064,
      "Venlo Grens", 51.34095, 6.19452,
   #   "Haanrade",
      "Haanrade Grens", 50.88077, 6.08548,
      "Zevenaar Grens",51.9012, 6.1411,
   "Nieuweschans Grens", 53.1877, 7.2181,
   "Oldenzaal Grens", 52.3091, 7.0440
    )
  ) 

df_2 <- left_join(
  df,
  stats %>% 
    mutate(station = str_remove(query, "station ")) %>% 
    select(station, to_lat = lat, to_lon = lon)
  , by = join_by(to == station)
) %>% 
  left_join(
    stats %>% 
      mutate(station = str_remove(query, "station ")) %>% 
      select(station, from_lat = lat, from_lon = lon)
    , by = join_by(from == station)
  )

ggplot(stats, aes(x = lon, y = lat)) +
  geom_point(colour = "darkred",
             size = 1.5) +
  geom_segment(data = df_2,
               aes(x = to_lon, xend = from_lon,
                   y = to_lat, yend = from_lat,
                   colour = passengers),
               size = 1.2
               ) +
  scale_colour_gradient(low = "blue", high = "red") +
  coord_equal() +
  theme_minimal()

