library(tidyverse)
library(janitor)
library(classInt)
library(showtext)

font_add_google("Forum", "font")
font_add_google("Open Sans", "labels")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

txt <- "#444444"

food <-
  read_csv("./2023/data/b0b1dc1b-2c1f-4c20-8dd0-7fa776da80d5_Data.csv") %>%
  clean_names() %>%
  filter(!is.na(country_code)) %>%
  filter(
    !country_code %in% c(
      "SSF",
      "WLD",
      "UMC",
      "LMC",
      "LIC",
      "SAS",
      "MEA",
      "LCN",
      "EAS",
      "ECS",
      "HIC",
      "NAC"
    )
  ) %>%
  rename(
    people_without_healthy_diet = millions_of_people_who_cannot_afford_a_healthy_diet_co_hd_unafford_n,
    percent_people_without_healthy_diet = percent_of_the_population_who_cannot_afford_a_healthy_diet_co_hd_headcount
  ) %>%
  mutate(
    country_name = case_when(
      country_code == "TUR" ~ "Turkey",
      country_code == "CIV" ~ "Ivory Coast",
      country_code == "BHS" ~ "Bahamas",
      country_code == "COD" ~ "Democratic Republic of the Congo",
      country_code == "COG" ~ "Republic of Congo",
      country_code == "CPV" ~ "Cape Verde",
      country_code == "LCA" ~ "Saint Lucia",
      country_code == "TTO" ~ "Trinidad",
      country_code == "EGY" ~ "Egypt",
      country_code == "GBR" ~ "UK",
      country_code == "GMB" ~ "Gambia",
      country_code == "IRN" ~ "Iran",
      country_code == "KGZ" ~ "Kyrgyzstan",
      country_code == "LAO" ~ "Laos",
      country_code == "PSE" ~ "Palestine",
      country_code == "KOR" ~ "South Korea",
      country_code == "RUS" ~ "Russia",
      country_code == "SVK" ~ "Slovakia",
      country_code == "SYR" ~ "Syria",
      country_code == "SWZ" ~ "Swaziland",
      country_code == "TWN" ~ "Taiwan",
      country_code == "USA" ~ "USA",
      TRUE ~ country_name
    ),
    people_without_healthy_diet = ifelse(
      people_without_healthy_diet == "..",
      NA_real_,
      people_without_healthy_diet
    ),
    population_pop = ifelse(population_pop == "..", NA_real_, population_pop)
  )



food_percent <- food %>%
  filter(percent_people_without_healthy_diet != "..") %>%
  group_by(country_name) %>%
  filter(time == max(time)) %>%
  mutate(
    percent_people_without_healthy_diet = as.numeric(percent_people_without_healthy_diet),
    percent_people_without_healthy_diet = percent_people_without_healthy_diet / 100,
    people_without_healthy_diet = as.numeric(people_without_healthy_diet)
  ) %>%
  select(
    country = country_name,
    country_code,
    time,
    percent_people_without_healthy_diet,
    people_without_healthy_diet
  )

sum(food_percent$people_without_healthy_diet)

world_map <-  map_data("world") %>%
  filter(!long > 180)


countries <- world_map %>%
  distinct(region) %>%
  rowid_to_column() %>%
  left_join(food_percent,
            by = join_by(region == country))

brk <-
  round(
    classIntervals(
      food_percent$percent_people_without_healthy_diet,
      n = 6,
      style = 'equal'
    )$brks,
    3
  )

cols <- PrettyCols::prettycols("Relax", direction = -1)
newcol <- colorRampPalette(cols)
ncols <- 7
cols2 <- newcol(ncols)
vmin <-
  min(food_percent$percent_people_without_healthy_diet, na.rm = TRUE)
vmax <-
  max(food_percent$percent_people_without_healthy_diet, na.rm = TRUE)

ann_seg <-
  tribble( ~ x,
           ~ xend,
           ~ y,
           ~ yend,
           48,
           60,-20,-25,
           78,
           63,
           19,
           7,
           -71,-69,
           19,
           25,
           107,
           131,
           34,
           28)

annotations <-
  tribble(
    ~ x,
    ~ y,
    ~ label,
    62,-26,
    "97% of Malagasy population has no healthy food",
    52,
    5,
    "India: 988 million people without healthy food",
    -68,
    27,
    "88% of Haitians have no healthy food\ncompared to 22% of the Dominicans",
    132,
    28,
    "China: 200 million people without healthy food"
  )

countries %>%
  ggplot(aes(fill = percent_people_without_healthy_diet, map_id = region)) +
  geom_map(map = world_map,
           colour = "white",
           linewidth = 0.2) +
  geom_segment(
    data = ann_seg,
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    linewidth = 0.1,
    colour = txt,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = annotations,
    aes(x = x, y = y, label = label),
    family = "labels",
    size = 1.5,
    colour = txt,
    lineheight = 1,
    hjust = "left",
    inherit.aes = FALSE
  ) +
  scale_fill_gradientn(
    name = "% of population without healthy food",
    colours = cols2,
    breaks = brk,
    labels = scales::label_percent(accuracy = 1L)(brk),
    limits = c(min(brk), max(brk))
  ) +
  labs(
    title = "Three billion people cannot afford a healthy diet",
    subtitle = str_wrap(
      "A healthy diet meets nutritional standards set by dietary guidelines, with sufficient diversity and quantity within and between food groups to achieve nutrient adequacy and protect against diet-related diseases.",
      90
    ),
    caption = "Data: Worldbank"
  ) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  coord_map("moll") +
  guides(
    fill = guide_legend(
      direction = "vertical",
      title.position = 'top',
      label.hjust = 0.5,
      nrow = 9,
      byrow = T,
      reverse = T,
      label.position = "right"
    )
  ) +
  theme_void() +
  theme(
    text = element_text(family = "font",
                        size = 10,
                        colour = txt),
    plot.background = element_rect(fill = "#F0F8FF",
                                   colour = "#F0F8FF"),
    plot.title.position = "plot",
    plot.title = element_text(
      family = "font",
      size = 32,
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 16,
      lineheight = 1,
      hjust = 0.5
    ),
    legend.background = element_rect(fill = "#F0F8FF",
                                     colour = "#F0F8FF"),
    legend.title = element_text(
      lineheight = 0.3,
      angle = 90,
      hjust = 151,
      vjust = 1.8
    ),
    legend.position = c(.15, .7),
    legend.spacing.x = unit(1, "mm"),
    plot.margin = margin(20, 20, 20, 20)
  )

## Saving ----
ggsave(
  "./2023/30_worldbank.png",
  bg = "#F0F8FF",
  dpi = 300,
  height = 9,
  width = 12
)

system("open ./2023/30_worldbank.png")
