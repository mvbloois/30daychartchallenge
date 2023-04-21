library(tidyverse)
library(janitor)
library(ggstream)
library(showtext)

font_add_google("Kanit", "font")
showtext_auto()
# https://opendata.cbs.nl/#/CBS/nl/dataset/82610NED/table



pal <- c(
  "#DC9456", "#312113", "#7A5230", "#AB7343", "black", "grey", "#4CAF50", "#173518", "#3D8c40", "#265828"
) %>% rev()

levs <- c("Aardgas", "Steenkool", "Stookolie", "Overige fossiele brandstoffen",
          "Kernenergie", "Overige energiedragers", "Biomassa", "Waterkracht", 
          "Windenergie", "Zonnestroom") %>% rev()

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
                         levels = levs)) 

perc <- electricity %>% 
  filter(year == 1998 | year == 2021) %>% 
  group_by(source, year) %>% 
  summarise(mwh = sum(mwh),
            .groups = "drop") %>% 
  group_by(year) %>% 
  mutate(share = scales::label_percent(0.1)(mwh / sum(mwh))) %>% 
  group_by(source) %>% 
  summarise(share = glue::glue_collapse(share, sep = " - "))

labels <- 
  tibble(
    source = c("Aardgas", "Steenkool", "Kernenergie", "Biomassa", "Windenergie", "Zonnestroom"),
    x     = c(1993, 1993, 1993, 2021.5, 2021.5, 2021.5),
    y     = c(2.7e7, 6.5e7, 8.5e7, 8.7e7, 10.2e7, 11.7e7), 
    label = c("GAS", "COAL", "NUCLEAR", "BIOMASS", "WIND", "SUN")
  ) %>% 
  inner_join(
    perc,
    by = join_by(source)
  ) %>% 
  mutate(label = glue::glue("{label} ({share})"))

electricity %>% 
  ggplot() +
  geom_area(aes(x = year, y = mwh, fill = source)) +
  geom_text(data = labels,
            aes(x = x, y = y, label = label),
            family = "font",
            size = 12,
            lineheight = 1,
            hjust = "left",
            colour = colorspace::lighten("black", 0.1)) +
  scale_fill_manual(values = pal) +
  scale_x_continuous(limits = c(1993, 2025)) +
  scale_y_continuous(labels = scales::label_comma(suffix = " MWh")) +
  labs(
    x = NULL,
    y = NULL,
    title = str_to_upper("Still a long way to go"),
    subtitle = str_wrap("Natural gas has historically been the most important source of energy production in the Netherlands, and the country has been one of the largest producers and exporters of natural gas in Europe. However, in recent years, there has been a shift away from natural gas due to concerns about its environmental impact, particularly with regards to the production and release of methane.\nTo replace natural gas, the Dutch government has been promoting the use of renewable energy sources such as wind and solar power. The Netherlands has one of the largest offshore wind farms in the world, and the government has set a target to generate 70% of the country's electricity from renewable sources by 2030 (ChatGPT 2023).", 120),
    caption = "Data: Centraal Bureau voor de Statistiek / 80030ned"
  ) +
  guides(fill = "none") +
  theme_minimal() +
  theme(
    text = element_text(family = "font",
                        size = 40),
    plot.background = element_rect(fill = "#add8e6"),
    plot.title = element_text(size = 92,
                              hjust = 0.5),
    plot.subtitle = element_text(size = 45,
                                 lineheight = 0.35),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 45,
                               vjust = 3.5),
    plot.margin = margin(b = 20, t = 20, r = 30, l = 10)
  )


## Saving ----
ggsave("./2023/22_green-energy.png",
       bg = "white",
       height = 9, width = 12)

system("open ./2023/22_green-energy.png")

