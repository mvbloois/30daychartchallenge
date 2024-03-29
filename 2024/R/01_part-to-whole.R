library(tidyverse)
library(rvest)
library(polite)
library(treemapify)
library(countrycode)
library(showtext) 

countrycode("Kosovo", "country.name", "continent")

font_add_google("Truculenta", "truculenta")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()

cols <- c("#00A5E3", "#8DD7BF", "#FF96C5", "#FF5768", "#FFBF65")
names(cols) <- c("Asia", "Africa", "Oceania", "Americas", "Europe")
cols2 <- colorspace::darken(cols, 0.8)
names(cols2) <- c("Asia", "Africa", "Oceania", "Americas", "Europe")
bg <- "#353C40"

url <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"

lst <- polite::scrape(polite::bow(url)) |> 
  rvest::html_nodes("table.wikitable") |> 
  rvest::html_table(fill = TRUE)

plot_data <- lst[[1]] |> 
  select(Location, Population) |> 
  mutate(Population = parse_number(Population)) |> 
  slice(-1) |> 
  mutate(continent = countrycode(Location,"country.name", "continent",
                                 custom_match = c(Kosovo = "Europe"))) |> 
  mutate(Location = if_else(Location == "Democratic Republic of the Congo",
                            "Congo", Location)) |> 
  drop_na()

plot_data |> 
  ggplot(aes(area = Population, fill = continent,
             subgroup = continent)) +
  geom_treemap(colour = bg) +
  geom_treemap_text(aes(label = Location,
                        colour = continent),
                    family = "robotoslab",
                    size = 38) +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols2) +
  labs(title = "PART TO WHOLE", 
       subtitle = "population by location",
       caption = "Data: Wikipedia") +
  theme(
    text = element_text(family = "truculenta",
                        colour = "#D6CECC"),
    legend.position = "none",
    plot.background = element_rect(fill = bg,
                                   colour = bg),
    plot.title = element_text(size = 88,
                              hjust = 0.5),
    plot.subtitle = element_text(size = 54,
                                 hjust = 0.5),
    plot.caption = element_text(size = 42,
                                hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(
  "./2024/R/01_part-to-whole.png",
  bg = bg,
  width = 8,
  height = 6,
)
