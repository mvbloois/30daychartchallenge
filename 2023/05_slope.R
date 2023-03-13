# devtools::install_github('rensa/ggflags')

# Max Roser and Esteban Ortiz-Ospina (2016) - "Global Education". Published online at OurWorldInData.org. Retrieved from: 'https://ourworldindata.org/global-education' [Online Resource]
# Barro, Robert and Jong-Wha Lee, 2013, "A New Data Set of Educational Attainment in the World, 1950-2010." Journal of Development Economics, vol 104, pp.184-198.
# United Nations Development Programme, Human Development Report (2018 Statistical Update).

library(tidyverse)
library(geomtextpath)
library(ggflags)
library(countrycode)
library(ggtext)
library(showtext)
source("./2023/00_functions.R")

## Text and colours ----
txt <- "grey15"
bg <- "grey85"
pal <- 
  c(Peru = "#C8102E",
    Kyrgyzstan = "#EF3340",
    Thailand = "#00205B",
    Guatemala = "#00A3E0",
    Mali = "#009639")

source <- " Max Roser and Esteban Ortiz-Ospina (2016) - 'Global Education'. Published online at OurWorldInData.org<br>
Barro and Lee, 2013, 'A New Data Set of Educational Attainment in the World, 1950-2010.' Journal of Development Economics, vol 104, pp.184-198<br>
United Nations Development Programme, Human Development Report (2018 Statistical Update)"

title <- "AVERAGE TOTAL YEARS OF SCHOOLING FOR ADULT POPULATION"

subtitle <- paste0(
  "Sentiment analysis based on 111.000+ tweets in 14 African languages shows<br>",
  "some interesting differences in ",
  "<span style = 'color:#319400'>positive</span>, ",
  "<span style = 'color:#F7C608'>neutral</span> and ",
  "<span style = 'color:#952038'>negative</span> sentiments."
)

subtitle <- str_wrap("Education is widely accepted to be a fundamental resource for individuals and societies. 
The average number of years spent in school is a measure of a population’s education level.", 80)

caption <- create_caption(txt, bg, source)

## Fonts ----
font_add_google("Roboto", "font")
font_add_google("Roboto Mono", "Rono")
showtext_auto()

## Data ----

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

## Theme ----
theme_set(theme_minimal())

theme_update(
  text = element_text(family = "font",
                      size = 62, lineheight = 0.3),
  plot.background = element_rect(fill = bg,
                                 colour = bg),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.title = element_text(family = "font",
                            face = "bold",
                            colour = txt,
                            margin = margin(t = 5, b = 5),
                            size = 82,
                            hjust = 0.5),
  plot.subtitle = element_text(family = "font",
                               size = 55,
                               hjust = 0.5,
                               margin = margin(t = 5, b = 5)
  ),
  plot.caption = element_markdown(family = "font",
                                  size = 32,
                                  colour = txt,
                                  hjust = 1,
                                  margin = margin(t = 15)),
  plot.margin = margin(b = 20, t = 20, r = 20, l = 20),
  legend.position = "none",
  panel.grid.major.x = element_blank(),
  panel.grid.minor.y = element_blank()
)

## Plot ----

df %>% 
  ggplot(aes(x = year, y = mean_schooling, colour = country)) +
  geom_line(data = filter(df, year > "Y1980"),
            aes(x = year, y = mean_schooling, group = country, colour = country)) +
    geom_textsegment(data = df_wide,
                   aes(x = "Y1980", xend = "Y2000",
                       y = `Y1980`, yend = `Y2000`,
                       colour = country,
                       label = country),
                   family = "mono",
                   size = 11) +
  # geom_textsegment(data = df_wide,
  #                  aes(x = "Y2000", xend = "Y2017",
  #                      y = `Y2000`, yend = `Y2017`,
  #                      colour = country,
  #                      label = country),
  #                  family = "mono",
  #                  size = 11) +
  geom_flag(aes(country = iso2c), size = 12) +
  scale_colour_manual(values = pal) +
  scale_x_discrete(labels = c("1980", "2000", "2017")) +
  scale_y_continuous(labels = scales::number_format(suffix = " years")) +
  labs(title = title,
       subtitle = subtitle,
       caption = caption,
       x = NULL,
       y = NULL) 

## Saving ----
ggsave("./2023/05_slope.png",
       bg = bg,
       height = 9, width = 12)

system("open ./2023/05_slope.png")

