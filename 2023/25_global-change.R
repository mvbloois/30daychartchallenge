library(tidyverse)
library(readxl)
library(janitor)
library(countrycode)
library(ungeviz)
library(broom)
library(emmeans)
library(patchwork)
library(showtext)

font_add_google("DM Sans", "font")
showtext_auto()

countrycode('Turkey', origin = 'country.name', destination = 'continent')

population <- read_xlsx("./2023/data/P_Data_Extract_From_World_Development_Indicators.xlsx") %>% 
  clean_names() %>% 
  select(country = country_name,
         population_1990 = x1990_yr1990,
         population_2021 = x2021_yr2021) %>% 
  mutate(across(starts_with("population"), as.numeric)) %>% 
  mutate(country = case_when(country == "Hong Kong SAR, China" ~ "Hong Kong, China (SAR)",
                             country == "Korea, Rep." ~ "Korea (Republic of)",
                             country == "Slovak Republic" ~ "Slovakia",
                             country == "Turkiye" ~  "Turkey",
                             country == "Bahamas, The" ~ "Bahamas",
                             country == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
                             country == "Iran, Islamic Rep." ~ "Iran (Islamic Republic of)",
                             country == "Moldova" ~ "Moldova (Republic of)",
                             country == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                             country == "Egypt, Arab Rep." ~ "Egypt",
                             country == "St. Lucia" ~ "Saint Lucia",
                             country == "Vietnam" ~ "Viet Nam",
                             country == "Bolivia" ~ "Bolivia (Plurinational State of)",
                             country == "Venezuela, RB" ~ "Venezuela (Bolivarian Republic of)",
                             country == "West Bank and Gaza" ~ "Palestine, State of",
                             country == "Gambia, The" ~ "Gambia",
                             country == "Eswatini" ~ "Eswatini (Kingdom of)",
                             country == "Kyrgyz Republic" ~ "Kyrgyzstan",
                             country == "Micronesia, Fed. Sts." ~ "Micronesia (Federated States of)",
                             country == "Lao PDR" ~ "Lao People's Democratic Republic",  
                             country == "Congo, Rep." ~ "Congo",                             
                             country == "Cote d'Ivoire" ~ "Côte d'Ivoire",
                             country == "Tanzania" ~ "Tanzania (United Republic of)",
                             country == "Congo, Dem. Rep." ~ "Congo (Democratic Republic of the)",
                             country == "Yemen, Rep." ~ "Yemen",
                             
                             TRUE ~ country)) %>% 
  pivot_longer(
    cols = starts_with("population"),
    names_to = "year",
    names_prefix = "population_",
    values_to = "population"
  ) 
  

hdi <-
  read_xlsx("./2023/data/HDR21-22_Statistical_Annex_HDI_Trends_Table.xlsx",
            skip = 4) %>%
  clean_names() %>%
  select(country, x1990, x2000, x2010, x2015, x2018, x2019, x2020, x2021) %>%
  slice(2:200) %>%
  pivot_longer(
    cols = starts_with("x"),
    names_to = "year",
    names_prefix = "x",
    values_to = "hdi"
  ) %>%
  mutate(hdi = parse_number(hdi)) %>%
  drop_na() %>%
  group_by(country) %>%
  filter(year == min(year) | year == max(year)) %>%
  ungroup() %>%
  mutate(
    country = ifelse(country == "Türkiye", "Turkey", country),
    continent = countrycode(country, origin = 'country.name', destination = 'continent')
  ) %>% 
  inner_join(
    population,
    by = join_by(country, year)
  )

hdi_old <-
  lm(hdi ~ continent, data = filter(hdi, year == min(year)),
     weights = population) %>%
  emmeans("continent") %>%
  tidy() %>%
  mutate(
    continent = factor(continent),
    continent = fct_reorder(continent, 1:5),
    year = 1990
  )

hdi_new <-
  lm(hdi ~ continent, data = filter(hdi, year == max(year)),
     weights = population) %>%
  emmeans("continent") %>%
  tidy() %>%
  mutate(
    continent = factor(continent),
    continent = fct_reorder(continent, 1:5),
    year = 2021
  )

plot_data <- bind_rows(hdi_new,
                       hdi_old) %>%
  mutate(year = factor(year))

txt <- "#0076BE"
cornflower <- "#95D8EB"
oceangreen <- "#48BF91"
bg <- "grey95"

ggplot(plot_data,
       aes(x = estimate, moe = std.error, y = year)) +
  stat_confidence_density(aes(group = year, fill = year),
                          height = 0.6,
                          confidence = 0.68) +
  geom_point(aes(x = estimate),
             size = 3) +
  geom_errorbarh(aes(xmin = estimate - std.error,
                     xmax = estimate + std.error),
                 height = 0.3) +
  scale_fill_manual(values = c(oceangreen, txt)) +
  xlim(0.3, 1.0) +
  facet_wrap( ~ continent ~ ., ncol = 1, switch = "y") +
  labs(title = "Human Development Index per continent 1990-2021",
       subtitle = str_wrap("The Human Development Index (HDI) is a summary measure of average achievement in key dimensions of human development: a long and healthy life, being knowledgeable and having a decent standard of living.", 70),
       caption = "Data: hdr.undp.org/data-center/",
       y = NULL) +
  theme_minimal() +
  theme(
    text = element_text(family = "font", 
                        colour = txt,
                        size = 40),
    plot.background = element_rect(fill = bg,
                                   colour = bg),
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5,
                              size = 90),
    plot.subtitle = element_text(hjust = 0.5,
                                 size = 50,
                                 lineheight = 0.3),
    axis.text = element_text(colour = txt),
    panel.grid.major = element_line(colour = "grey70"),
    panel.grid.minor.x = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(
      size = 45,
      colour = txt,
      face = "bold",
      angle = 0
    ),
    panel.spacing = unit(2, "lines"),
    plot.margin = margin(10,10,10,10)
  )

## Saving ----
ggsave(
  "./2023/25_global-change.png",
  bg = bg,
  height = 9,
  width = 12
)

system("open ./2023/25_global-change.png")

