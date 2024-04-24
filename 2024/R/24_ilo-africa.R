
library(tidyverse)
library(countrycode)
library(showtext) 

font_add_google("Truculenta", "truculenta")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()

pal <- wesanderson::wes_palette("Zissou1", 21, type = "continuous")

df <- read_csv("./2024/data/EMP_NIFL_SEX_RT_A-full-2024-04-23.csv") |> 
  janitor::clean_names() |> 
  filter(sex_label == "Sex: Total") |> 
  filter(time == max(time),
         .by = ref_area_label) |> 
  select(ref_area_label, time, obs_value) 

world_map <- map_data("world") |> 
  mutate(continent = countrycode::countryname(region, "continent")) |> 
  filter(continent == "Africa") |> 
  filter(lat > -35)

countries <- world_map |> 
  distinct(region) |>  
  rowid_to_column() |> 
  left_join(
    df,
    by = join_by(region == ref_area_label)
  ) 

countries %>% 
  ggplot(aes(fill = obs_value, map_id = region)) +
  geom_map(map = world_map,
           colour = "grey10",
           linewidth = 0.1) +
  scale_fill_gradientn(colours = pal,
                       na.value = "grey80") +
  expand_limits(x = world_map$long, y = world_map$lat) +
  labs(
    title = "ILO Region for Africa",
    subtitle = str_wrap("\"Nearly 83% of employment in Africa and 85% in Sub-Saharan Africa is informal, absorbing many of the continent’s young employment seekers. In the past, policy narratives in Africa tended to either neglect informal economies or even viewed them as potentially threatening to formal economies – therefore needing elimination and control rather than support and investment for inclusive structural economic transformation.\"", 60),
    caption = "Source: ILOSTAT",
    fill = "% informal sector") +
  coord_map("moll") +
  theme_void() +
  theme(
    legend.position = c(0.2, 0.3),
    legend.title.position = "left",
    legend.title = element_text(angle = 90),
    text = element_text(family = "robotoslab",
                        size = 28),
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(family = "truculenta",
                              face = "bold",
                              size = 82),
    plot.subtitle = element_text(lineheight = 0.3,
                                 margin = margin(0,0,10,0))
  )

ggsave(
  "./2024/R/24_ilo-africa.png",
  bg = "white",
  width = 4,
  height = 6,
)

system("open ./2024/R/24_ilo-africa.png")
