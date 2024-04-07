# Libraries
library(ggraph)
library(igraph)
library(rvest)
library(polite)
library(countrycode)
library(showtext) 


countrycode("Kosovo", "country.name", "continent")

font_add_google("Truculenta", "truculenta")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()

pal <- wesanderson::wes_palette("Moonrise3", 5, "discrete")
bg <- colorspace::lighten(pal[5], 0.8)

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
  drop_na() |> 
  mutate(whole = "earth")



edges <- bind_rows(
  plot_data |> 
    distinct(from = whole, to = continent),
  plot_data |> 
    distinct(from = continent, to = Location)
)

vertices <- bind_rows(
  tibble(
    name = c("earth", "Asia", "Africa", "Americas",
             "Europe", "Oceania"),
    size = rep(0, 6)
  ),
  plot_data |> 
    select(name = Location, size = Population)
) |> 
  mutate(label = if_else(size >= 10e6, name, ""))

set.seed(1234)

mygraph <- graph_from_data_frame(edges, vertices=vertices )

# Control the size of each circle: (use the size column of the vertices data frame)
ggraph(mygraph, layout = 'circlepack', weight=size) + 
  geom_node_circle(aes(fill = depth)) +
  geom_node_text(aes(label=label, filter=leaf, size=size*1e6),
                  family = "robotoslab",
                  colour = bg) +
  scale_fill_gradientn(colours = c(pal[1], pal[3])) + 
  labs(title = "CIRCULAR", 
       subtitle = "population by location",
       caption = "Data: Wikipedia") +
  theme_void() + 
  theme(
    text = element_text(family = "truculenta"),
    plot.background = element_rect(fill = bg,
                                   colour = bg),
    legend.position="FALSE",
    plot.title = element_text(size = 88,
                              hjust = 0.5),
    plot.subtitle = element_text(size = 54,
                                 hjust = 0.5),
    plot.caption = element_text(size = 42,
                                hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
    )

ggsave(
  "./2024/R/08_circular.png",
  bg = bg,
  width = 7,
  height = 7,
)
