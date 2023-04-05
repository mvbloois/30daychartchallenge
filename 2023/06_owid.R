library(tidyverse)
library(janitor)
library(scales)
library(ggtext)
library(ggflags)
library(countrycode)
library(showtext)
install.packages("ggchicklet",                    # Install & load ggchicklet package
                 repos = "https://cinc.rud.is")
library(ggchicklet)
source("./2023/00_functions.R")

txt <- "#002147"
red <- "#ce261e"
amber <- "#f7c020"
bg <- "#ebeef2"
## Fonts ----
font_add_google("Lato", "font")
font_add_google("Playfair Display", "title")
showtext_auto()

t1 <- "Lead poisoning is estimated to account for about 1% of the global disease burden. On an individual level, being exposed to lead in the environment can hinder a child’s brain development: it can result in a reduction in IQ; cognitive function; and has been linked to higher levels of antisocial behavior. But it’s a global problem that we can tackle. Many countries have already made significant progress against it."


t2 <- "Lanphear et al. (2005) Low-Level Environmental Lead Exposure and Children’s Intellectual Function: An International Pooled Analysis. Environmental Health Perspectives"
t2 <- "Our World In Data" 
caption <- create_caption(txt, bg, t2)

lead <- read_csv("./2023/data/children-lead-5micrograms.csv") %>% 
  clean_names() %>% rename(children = 4) %>% 
  filter(entity != "Total") %>% 
  mutate(iso2c = str_to_lower(countrycode(entity, "country.name", "iso2c")),
         label =  number_format(big.mark = ".", decimal.mark = ",")(children)) %>% 
  arrange(desc(children)) %>% 
  head(20)

lead %>% 
  mutate(entity = if_else(entity == "Democratic Republic of Congo", "DR of Congo", entity)) %>% 
  ggplot(aes(x = fct_reorder(entity, children),
             y = children)) +
  geom_chicklet(fill = "#6C6C6A",
                colour = "grey40") +
  geom_flag(aes(y = 0, country = iso2c),
            size = 26/.pt) +
  geom_text(data = filter(lead, children > 28e6),
            aes(label = label),
            family = "font",
            colour = "white",
            size = 36/.pt,
            hjust = 1.2,
            vjust = 0.5) +
  geom_textbox(aes(x = "Iraq", y = 70e6,
           label = t1),
           family = "title",
           size = 46/.pt,
           lineheight = 0.5,
           hjust = 0,
           colour = txt,
           width = unit(150, "mm"),
           height = unit(60, "mm")) +
  scale_y_continuous(labels = number_format(big.mark = ".", decimal.mark = ",")) +
  labs(x = NULL,
       y = NULL,
       title = "Lead pollution is a widespread problem that receives little attention",
       subtitle = "Children with blood lead >5 µg/dL",
       caption = caption) +
  coord_flip() +
  theme_minimal() +
  theme(
    text = element_text(family = "font",
                        colour = txt,
                        size = 52),
    plot.title.position = "plot",
    plot.title = element_text(family = "title",
                              size = 82),
    plot.subtitle = element_text(size = 62),
    plot.caption = element_markdown(size = 36),
    plot.background = element_rect(fill = bg, 
                                   colour = bg),
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 52),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "#6C6C6A",
                                      linetype = "dotted"),
    plot.margin = margin(b = 20, t = 20, r = 30, l = 30)
  )

## Saving ----
ggsave("./2023/06_owid.png",
       bg = bg,
       height = 9, width = 12)

system("open ./2023/06_owid.png")

