# https://data.ipu.org/women-ranking?month=3&year=2023
# https://www.riinu.me/2022/02/world-map-ggplot2/
library(tidyverse)
library(ggthemes)
library(gghalves)
library(classInt)
library(patchwork)

## HDI

df_2003 <- readxl::read_xlsx("./2023/data/stand_2003.xlsx", skip = 1) %>% 
  janitor::clean_names() %>% 
  select(country = 2,
         seats = seats_4,
         women = women_5) %>% 
  mutate(country = str_remove_all(country, "\\*"),
         women = parse_number(women),
         share_2003 = women / seats) %>% 
  select(country, share_2003) %>% 
  mutate(country = case_when(country == "Dem. Rep. of East Timor" ~ "Timor-Leste",
                             country == "United Rep. of Tanzania" ~"Tanzania",
                             country == "Lao People's Democratic Rep." ~ "Laos",
                             country == "The f.Y.R. of Macedonia" ~ "North Macedonia",
                             country == "Saint Vincent & the Grenadines" ~ "Saint Vincent and the Grenadines",
                             country == "Dem. People's Rep. of Korea" ~ "North Korea",
                             country == "Iran (Islamic Rep. of)" ~ "Iran",
                             country == "Micronesia (Fed. States of)" ~ "Micronesia",
                             country == "Cote d'Ivoire" ~ "Ivory Coast",
                             
                             TRUE ~ country))

df_2023 <- read_csv("./2023/data/chamber--current_women_percent.csv",
         skip = 5) %>% 
  select(country = 2,
         elections = 3,
         seats = 4,
         women = 5) %>% 
  mutate(seats = parse_number(seats),
         women = parse_number(women),
         share_2023 = women / seats) %>% 
  drop_na() %>% 
  select(country, share_2023) %>% 
  mutate(country = case_when(str_detect(country, "Bolivia") ~ "Bolivia",
                             country == "Cabo Verde" ~ "Cape Verde",
                             country == "United Republic of Tanzania" ~"Tanzania",
                             country == "Lao People's Democratic Republic" ~ "Laos",
                             country == "Democratic People's Republic of Korea" ~ "North Korea",
                             country == "Iran (Islamic Republic of)" ~ "Iran",
                             country == "Eswatini" ~ "Swaziland",
                             country == "Micronesia (Federated States of)" ~ "Micronesia",
                             country == "Côte d'Ivoire" ~ "Ivory Coast",
                             country == "Gambia (The)" ~ "Gambia",
                             country == "Türkiye" ~ "Turkey",
                             TRUE ~ country))


df_2023 %>% 
  anti_join(
    df_2003,
    by = join_by(country)
  )

df_2003 %>% 
  anti_join(
    df_2023,
    by = join_by(country)
  )

plot_data <- df_2023 %>% 
  left_join(
    df_2003,
    by = join_by(country)
  ) %>% 
  mutate(direction = ifelse(share_2023 >= share_2003, "Y", "N")) %>% 
  pivot_longer(cols = starts_with("share"), names_to = "year", names_prefix = "share_",
               values_to = "share")

plot_data %>% 
  ggplot(aes(x = year, y = share)) +
  geom_point(aes(colour = direction),
             size = 7,
             shape = "-") +
  geom_line(aes(group = country,
                colour = direction),
            alpha = 0.4) +
  geom_half_violin(data = plot_data %>% filter(year == 2003),
                   nudge = 0.1,
                   colour = "pink",
                   fill = "pink") +
  geom_half_violin(data = plot_data %>% filter(year == 2023),
                   nudge = 0.1,
                   side = "r",
                   colour = "pink",
                   fill = "pink") +
  scale_y_continuous(limits = c(0,.75),
                     breaks = c(0,.25,.50,.75),
                     labels = scales::label_percent()) +
  scale_colour_manual(values = c("Y" = "#19486A", "N" = "darkred")) +
  labs(
    x = NULL,
    y = NULL
  ) +
  guides(
    colour = "none"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )


df_2023 %>% 
  mutate(country = case_when(str_detect(country, "Bolivia") ~ "Bolivia",
                             country == "Democratic People's Republic of Korea" ~ "North Korea",
                             country == "Russian Federation" ~ "Russia",
                             country == "United States of America" ~ "USA",
                             country == "United Kingdom" ~ "UK",
                             country == "Republic of Moldova" ~ "Moldova",
                             country == "United Republic of Tanzania" ~"Tanzania",
                             country == "Viet Nam" ~ "Vietnam",
                             country == "Lao People's Democratic Republic" ~ "Laos",
                             country == "Republic of Korea" ~ "South Korea",
                             country == "Eswatini" ~ "Swaziland",
                             country == "Syrian Arab Republic" ~ "Syria",
                             country == "Congo" ~ "Republic of Congo",
                             country == "Antigua and Barbuda" ~ "Antigua",
                             country == "Cabo Verde" ~ "Cape Verde",
                             country == "Saint Kitts and Nevis" ~ "Saint Kitts",
                             country == "Trinidad and Tobago" ~ "Trinidad",
                             country == "Saint Vincent and the Grenadines" ~ "Saint Vincent",
                             country == "Brunei Darussalam" ~ "Brunei",
                             TRUE ~ country))


world_map <-  map_data("world") %>% 
  filter(! long > 180)

countries <-  world_map %>% 
  distinct(region) %>% 
  rowid_to_column() %>% 
  left_join(df_2023,
            by = join_by(region == country))

countries%>% 
  anti_join(df_2023,
            by = join_by(region == country))

df_2023 %>% 
  anti_join(countries,
            by = join_by(country == region))

map <- countries %>% 
  ggplot(aes(fill = share_2023, map_id = region)) +
  geom_map(map = world_map,
           colour = "white",
           linewidth = 0.1) +
  scale_fill_gradient(
    low = "#19486A",
    high = "pink",
    na.value = "grey65"
  ) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  coord_map("moll") +
  theme_map() +
  theme(
    plot.background = element_rect(fill = "white",
                                   colour = "white")
  )



plot + inset_element(map, 0.6, 0.6, 1, 1, align_to = "full")

## Saving ----
ggsave("./2023/24_un-women.png",
       bg = "white",
       height = 9, width = 12)

system("open ./2023/24_un-women.png")  
