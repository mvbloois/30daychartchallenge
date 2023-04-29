library(tidyverse)
library(janitor)
library(classInt)
library(showtext)

font_add_google("Open Sans", "font")
showtext_auto()

food <- read_csv("./2023/data/b0b1dc1b-2c1f-4c20-8dd0-7fa776da80d5_Data.csv") %>% 
  clean_names() %>% 
  filter(! is.na(country_code)) %>% 
  filter(! country_code %in% c("SSF", "WLD", "UMC", "LMC", "LIC", "SAS", "MEA", "LCN",
                               "EAS", "ECS", "HIC", "NAC")) %>% 
  rename(people_without_healthy_diet = millions_of_people_who_cannot_afford_a_healthy_diet_co_hd_unafford_n,
         percent_people_without_healthy_diet = percent_of_the_population_who_cannot_afford_a_healthy_diet_co_hd_headcount) %>% 
  mutate(country_name = case_when(country_code == "TUR" ~ "Turkey",
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
                                  TRUE ~ country_name),
         people_without_healthy_diet = ifelse(people_without_healthy_diet == "..", NA_real_, people_without_healthy_diet),
         population_pop = ifelse(population_pop == "..", NA_real_, population_pop))
  


food_percent <- food %>% 
  filter(percent_people_without_healthy_diet != "..") %>% 
  group_by(country_name) %>% 
  filter(time == max(time)) %>% 
  mutate(percent_people_without_healthy_diet = as.numeric(percent_people_without_healthy_diet),
         percent_people_without_healthy_diet = percent_people_without_healthy_diet / 100,
         people_without_healthy_diet = as.numeric(people_without_healthy_diet)) %>% 
  select(country = country_name, country_code, time, percent_people_without_healthy_diet,
         people_without_healthy_diet)

sum(food_percent$people_without_healthy_diet)
  
world_map <-  map_data("world") %>%
  filter(!long > 180)


countries <- world_map %>%
  distinct(region) %>%
  rowid_to_column() %>%
  left_join(food_percent,
            by = join_by(region == country))

brk <- round(classIntervals(food_percent$percent_people_without_healthy_diet,
                            n = 6,
                            style = 'equal')$brks, 3)

cols <- PrettyCols::prettycols("Relax", direction = -1)
newcol <- colorRampPalette(cols)
ncols <- 7
cols2 <- newcol(ncols)
vmin <- min(food_percent$percent_people_without_healthy_diet, na.rm = TRUE)
vmax <- max(food_percent$percent_people_without_healthy_diet, na.rm = TRUE)


countries %>%
  ggplot(aes(fill = percent_people_without_healthy_diet, map_id = region)) +
  geom_map(map = world_map,
           colour = "white",
           linewidth = 0.2) +
  annotate("segment",
           x = 50, y = -20,
           xend = 60, yend = -25,
           linewidth = .1,
           colour = "black") +
  annotate("text",
           x = 62, y =-27,
           label = "97% of Malagasy population has no healthy food",
           hjust = "left") +
  annotate("segment",
           x = 73, y = 15,
           xend = 69, yend = 8,
           linewidth = .1,
           colour = "black") +
  annotate("text",
           x = 58, y = 5,
           label = "India has 988 million people without healthy food",
           hjust = "left") +
  scale_fill_gradientn(
    name = "% of population without healthy food",
    colours = cols2,
    breaks = brk,
    labels = scales::label_percent(accuracy = 1L)(brk),
    limits = c(min(brk), max(brk))
  ) +
  labs(
    title = "Honger",
    subtitle = "in de wereld",
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
                        size = 40),
    plot.background = element_rect(fill = "#F0F8FF",
                                   colour = "#F0F8FF"),
    legend.background = element_rect(fill = "#F0F8FF",
                                     colour = "#F0F8FF"),
    legend.title = element_text(lineheight = 0.3,
                                angle = 90,
                                hjust = -140,
                                vjust = 1.8),
    legend.position = c(.15, .7),
    legend.spacing.x = unit(1, "mm"),
    plot.margin = margin(20,20,20,20)
  )

## Saving ----
ggsave("./2023/30_worldbank.png",
       bg = "#F0F8FF",
       height = 9,
       width = 12)

system("open ./2023/30_worldbank.png")

