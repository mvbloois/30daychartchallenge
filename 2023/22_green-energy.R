library(tidyverse)
library(janitor)
library(ggstream)
library(showtext)

font_add_google("Kanit", "font")
showtext_auto()
# https://opendata.cbs.nl/#/CBS/nl/dataset/82610NED/table



pal <- c(
  "#DC9456", "#312113", "#7A5230", "#AB7343", "#4CAF50", "#173518", "#3D8c40", "#265828"
) %>% rev()

levs <- c("Aardgas", "Steenkool", "Stookolie", "Overige fossiele brandstoffen",
          "Kernenergie, Biomasse", "Waterkracht", "Windenergie", "Zonnestroom",
          "Overige energiedragers") %>% rev()

electricity <- read_csv2("./2023/data/Elektriciteit_en_warmte__productie_en_inzet_naar_energiedrager_20042023_214106.csv") %>% 
  clean_names() %>% 
  select(centraal = 1,
         source   = 2,
         year     = 3,
         mwh      = 4,
         tj       = 5) %>% 
  mutate(year = parse_number(year)) %>% 
  filter(! str_detect(source, "Totaal")) %>% 
  mutate(source = factor(source,
                         levels = levs)) %>% 
  filter(year >= 2000)

labels <- 
  tibble(
    x     = c(2010, 2010, 2018, 2020),
    y     = c(-2e7, 2.5e7, 4e7, 5.0e7), 
    label = c("GAS", "COAL", "WIND", "SUN")
  )

electricity %>% 
  ggplot() +
  geom_stream(aes(x = year, y = mwh, fill = source),
              bw = .9) +
  geom_text(data = labels,
            aes(x = x, y = y, label = label),
            family = "font",
            size = 15,
            colour = colorspace::lighten("#add8e6", 0.8)) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = scales::label_comma(suffix = " MWh")) +
  labs(
    x = NULL,
    y = NULL,
    title = "Electricity production in the Netherlands",
    subtitle = "Yeah",
    caption = "Data: Centraal Bureau voor de Statistiek"
  ) +
  guides(fill = "none") +
  theme_minimal() +
  theme(
    text = element_text(family = "font",
                        size = 40),
    plot.background = element_rect(fill = "#add8e6"),
    plot.title.position = "plot",
    plot.title = element_text(size = 92),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(b = 20, t = 20, r = 30, l = 30)
  )

## Saving ----
ggsave("./2023/22_green-energy.png",
       bg = "white",
       height = 9, width = 12)

system("open ./2023/22_green-energy.png")

