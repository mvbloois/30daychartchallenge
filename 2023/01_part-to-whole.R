## Packages ----
library(tidyverse)
library(ggtext)
library(showtext)
library(RColorBrewer)
source("./2023/00_functions.R")

## Text and colours ----
txt <- "#CA132F"
bg  <- "#C5C5C5"

source <- "eerstekamer\\.nl"

title <- "EERSTE KAMER DER STATEN-GENERAAL"

subtitle <- str_wrap("On May 30th, 2023 the members of the States-Provincial and \
the electoral colleges of Bonaire, Saint Eustatius and Saba elect the new Senate \
of the Dutch States General. In this chart we compare the election results of \
2019 with the estimates based on the elections for the States-Provincial of March \
15th, 2023.", 80)

caption <- create_caption(txt, bg, source)

## Fonts ----
font_add_google("Playfair Display", "titlefont")
font_add_google("Fira Sans", "mainfont")
showtext_auto()

## Theme ----
theme_set(theme_void())

theme_update(
  text = element_text(family = "titlefont",
                      size = 62, lineheight = 0.3),
  plot.background = element_rect(fill = bg,
                                 colour = bg),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.title = element_text(family = "titlefont",
                            face = "bold",
                            colour = txt,
                            margin = margin(t = 5, b = 5),
                            size = 102,
                            hjust = 0.5),
  plot.subtitle = element_text(family = "titlefont",
                                   size = 62,
                                   hjust = 0.5
                                   ),
  plot.caption = element_markdown(family = "titlefont",
                                  size = 62,
                                  colour = "grey15",
                                  hjust = 0.5,
                                  margin = margin(b = 5)),
  plot.margin = margin(b = 20, t = 20, r = 0, l = 0),
)

## Data ----
df <- tribble(
  ~meting, ~fractie, ~zetels,
  1, "VVD"    , 12,
  1, "FvD"    , 12,
  1, "CDA"   ,  9,
  1, "GroenLinks", 8,
  1, "D66"    ,  7,
  1, "PvdA"   ,  6,
  1, "PVV"    ,  5,
  1, "SP"    ,  4,
  1, "ChristenUnie", 4,
  1, "PvdD"   ,  3,
  1, "50PLUS" ,  2,
  1, "SGP"    ,  2,
  1, "OSF"    ,  1,
  3, "VVD"    , 10,
  3, "FvD"    , 2,
  3, "BBB", 17,
  3, "JA21", 3,
  3, "Volt", 1, 
  3, "CDA"   ,  5,
  3, "GroenLinks", 8,
  3, "D66"    ,  5,
  3, "PvdA"   ,  7,
  3, "PVV"    ,  5,
  3, "SP"    ,  3,
  3, "ChristenUnie", 2,
  3, "PvdD"   ,  4,
  3, "SGP"    ,  1,
  3, "OSF", 1,
  3, "50PLUS", 1
  
) 

spectrum <- 
  c("SP", "PvdD", "50PLUS", "GroenLinks", "PvdA", "Volt", "D66",
    "ChristenUnie", "CDA", "VVD", "BBB", "JA21", "PVV", "FvD", "SGP", "OSF")

## Plot ----

getPalette = colorRampPalette(brewer.pal(9, "Set1"))

df  %>% 
  filter(meting != 2) %>% 
  mutate(fractie = factor(fractie, levels = spectrum)) %>% 
  ggplot(aes(x = meting, y = zetels, fill = fractie)) +
  geom_col(colour = "white", linewidth = 1, position = position_stack()) +
  scale_fill_manual(values = getPalette(16)) +
  geom_text(aes(x = 1, y = -4, label = "2019"),
            family = "titlefont",
            size = 25) +
  geom_text(aes(x = 3, y = -4, label = "2023"),
            family = "titlefont",
            size = 30) +
  xlim(c(-1,4)) +
  ylim(c(-4, 79)) +
  coord_polar(theta = "y") +
  labs(title = title,
       subtitle = subtitle,
       caption = caption,
       fill = "Party") 

## Saving ----
ggsave("./2023/01_part-to-whole.png",
       bg = bg,
       height = 12, width = 12)

system("open ./2023/01_part-to-whole.png")
