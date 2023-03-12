devtools::install_github('rensa/ggflags')

# Max Roser and Esteban Ortiz-Ospina (2016) - "Global Education". Published online at OurWorldInData.org. Retrieved from: 'https://ourworldindata.org/global-education' [Online Resource]
# Barro, Robert and Jong-Wha Lee, 2013, "A New Data Set of Educational Attainment in the World, 1950-2010." Journal of Development Economics, vol 104, pp.184-198.
# United Nations Development Programme, Human Development Report (2018 Statistical Update).

library(tidyverse)
library(geomtextpath)
library(ggflags)
library(countrycode)

pal <- 
  c(Peru = "#C8102E",
    Kyrgyzstan = "#EF3340",
    Thailand = "#00205B",
    Guatemala = "#00A3E0",
    Mali = "#009639")

df <- read_csv("./2023/data/mean-years-of-schooling-long-run.csv") %>% 
  rename(country = Entity,
         code = Code,
         year = Year,
         mean_schooling = 4) %>% 
  #filter(code %in% c("COL", "VEN", "PER", "BRA", "ARG", "CHL", "PRY", "URY", "ECU", "BOL")) %>% 
  filter(code %in% c("MLI", "PER", "THA", "KGZ", "GTM")) %>%
  filter(year %in% c(1980, 2000, 2017)) %>% 
  mutate(year = glue::glue("Y{year}"),
         iso2c = str_to_lower(countrycode(country, "country.name", "iso2c")))

df_wide <- df %>% 
  spread(year, mean_schooling)

df %>% 
  ggplot(aes(x = year, y = mean_schooling, colour = country)) +
  geom_textsegment(data = df_wide,
                   aes(x = "Y1980", xend = "Y2000",
                       y = `Y1980`, yend = `Y2000`,
                       colour = country,
                       label = country)) +
  geom_textsegment(data = df_wide,
                   aes(x = "Y2000", xend = "Y2017",
                       y = `Y2000`, yend = `Y2017`,
                       colour = country,
                       label = country)) +
  geom_flag(aes(country = iso2c), size = 12) +
  scale_colour_manual(values = pal) +
  scale_y_continuous(labels = scales::number_format(suffix = " years")) +
  labs(title = "Average Total Years of Schooling for Adult Population",
       subtitle = "OWID",
       caption = "Data: Barro-Lee (2018) and UNDP HDR (2018)",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

