
library(tidyverse)
library(here)
library(janitor)
library(sf)
library(tidygraph)
library(ggraph)
library(ggtext)
library(showtext)

## Fonts ----
font_add_google("Roboto Condensed", "font")
showtext_auto()

## @_ansgar
## https://github.com/bydata/30DayChartChallenge/blob/main/2023/03/03-flora-trees-nyc.R

ocean <- "#005477"
seafoam <- "#93E9BE"
txt <- "white"

subtitle <- "In 2008, it was estimated that nearly 100 million sharks were being killed by people every year, due to commercial and recreational fishing. In 2021, it was estimated that the population of oceanic sharks and rays had dropped by 71% over the previous half-century."

source <- "Data: Pollerspöck, J. & Straube, N. 2022, www\\.shark-references\\.com"


## Theme ----
theme_set(theme_void())

theme_update(
  text = element_text(family = "font",
                      size = 62, 
                      lineheight = 0.3),
  plot.background = element_rect(fill = ocean,
                                 colour = ocean),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.title = element_text(family = "font",
                            face = "bold",
                            colour = "white",
                            margin = margin(t = 5, b = 5),
                            size = 82,
                            hjust = 0.5),
  plot.subtitle = element_text(family = "font",
                               colour = "white",
                               size = 55,
                               hjust = 0.5,
                               margin = margin(t = 5, b = 5)
  ),
  plot.caption = element_markdown(family = "Ariel",
                                  size = 32,
                                  colour = txt,
                                  hjust = 1,
                                  margin = margin(t = 15)),
  plot.margin = margin(b = 20, t = 20, r = 20, l = 20),
  legend.position = "none"
)


sharks <- readxl::read_xlsx("./2023/data/List_Extant_Species_03_2021.xlsx") %>% 
  clean_names() %>% 
  select(superorder = superorder_sharks_and_rays_subclass_chimaera, order, family, genus, species) %>% 
  mutate(species = paste(genus, species))


origin_superorder_connections <- 
  tibble(
    from = "origin",
    to = unique(sharks$superorder)
  )

superorder_order_connections <- 
  sharks %>% distinct(from = superorder, to = order)

order_family_connections <- 
  sharks %>% distinct(from = order, to = family)

family_genus_connections <- 
  sharks %>% distinct(from = family, to = genus)

genus_species_connections <- 
  sharks %>% distinct(from = genus, to = species)

edges <- bind_rows(
  superorder_order_connections,
  order_family_connections,
  family_genus_connections,
  genus_species_connections,
  origin_superorder_connections
)

nodes <- distinct(edges, name = to, group = from) %>% 
  mutate(
    is_main = !str_detect(name, "\\ "),
    label = ifelse(is_main, name, NA)) %>% 
  add_row(name = "origin", is_main = FALSE) %>% 
  arrange(group, name)

nodes$id <- NA
is_leaf <- nodes$group != "origin" & nodes$name != "origin"
is_leaf <- nodes$group != "origin" & nodes$name != "origin"
nleaves <- nrow(nodes[is_leaf, ])
nodes$id[is_leaf] <- seq_len(nleaves)
nodes$angle <- 90 - 360 * nodes$id / nleaves

nodes$hjust <- ifelse(nodes$angle < -90, 1, 0)

# flip angle BY to make them readable
nodes$angle <- ifelse(nodes$angle < -90, nodes$angle + 180, nodes$angle)
nodes <- nodes %>% 
  mutate(angle = replace_na(angle, 0),
         hjust = replace_na(hjust, 0)
  )

graph <- igraph::graph_from_data_frame(edges, vertices = nodes)

ggraph(graph, layout = "dendrogram", circular = TRUE) +
  geom_edge_diagonal(color = "orange", edge_width = 0.1) +
  geom_node_text(
    aes(label = label),
    check_overlap = TRUE,
    size = 6.5,
    family = "font",
    colour = "white"
  ) +
  geom_label(aes(x = 0, y = 0, label = "Sharks & Rays"),
             size = 10.5,
             family = "font",
             colour = ocean,
             fill = "white"
             ) +
  labs(
    title = "Sharks & Rays",
    subtitle = str_wrap(subtitle, 80),
    caption = source
  ) +
  guides(fill = "none", size = "none")

## Saving ----
ggsave("./2023/11_circular.png",
       bg = ocean,
       height = 11, width = 10)

system("open ./2023/11_circular.png")

