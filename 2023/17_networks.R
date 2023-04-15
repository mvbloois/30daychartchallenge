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
 
stats <- dd[1]

stats

network <- df %>% 
  mutate(to_coords = list(tmaptools::geocode_OSM(paste("station", to))$coords),
         from_coords = list(tmaptools::geocode_OSM(paste("station", from))$coords))

ifelse(tmaptools::geocode_OSM(paste("station", "Gouda"))$coords[1], 0, 1)
