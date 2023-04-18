library(tidyverse)
library(gghighlight)
library(patchwork)
library(xts)
library(ggdendro)
library(showtext)

font_add_google("Bitter", "font")
showtext_auto()

`%notin%` <- Negate(`%in%`)
bg <- alpha("#FFCC00", 0.1)

df <- read_delim("./2023/data/demo_gind_tabular.tsv.gz") %>% 
  janitor::clean_names() %>% 
  mutate(country_code = str_sub(freq_indic_de_geo_time_period, 7, 9),
         variable = str_sub(freq_indic_de_geo_time_period, 1, 5)) %>% 
  pivot_longer(cols = starts_with("x"), names_to = "year", names_prefix = "x",
               values_to = "population") %>% 
  select(-1) %>% 
  filter(population != ": ") %>% 
  filter(variable == "A,AVG") %>% 
  filter(country_code %notin% c("EEA", "EU2", "EA1", "EA2", "RU", "TR",
                                "DE", "BY", "AZ", "FX", "EFT", "UA",
                                "RS", "AD", "AM", "BA", "GE","MC", "MD",
                                "ME", "SM", "XK", "AL", "MK")) %>% 
  mutate(population = parse_number(population),
         year = parse_integer(year),
         country_code = ifelse(country_code == "DE_", "DE", country_code)) %>% 
  select(-variable) %>% 
  add_row(country_code = "UK", year = 2020, population = 67081000) %>% 
  add_row(country_code = "UK", year = 2021, population = 67026300) %>% 
  bind_rows(
    tibble::tribble(
      ~country_code, ~year, ~population,
      "FR", 1981,	54016749,
      "FR", 1980, 53713830,
      "FR", 1979, 53428781,
      "FR", 1978, 53175309,
      "FR", 1977, 52925442,
      "FR", 1976, 52685266,
      "FR", 1975, 52450030,
      "FR", 1974, 52163552,
      "FR", 1973, 51814075,
      "FR", 1972, 51410363,
      "FR", 1971, 50970698,
      "FR", 1970, 50523586,
      "FR", 1969, 50087049,
      "FR", 1968, 49647279,
      "FR", 1967, 49181410,
      "FR", 1966, 48683086,
      "FR", 1965, 48168804,
      "FR", 1964, 47641952,
      "FR", 1963, 47121247,
      "FR", 1962, 46635189,
      "FR", 1961, 46143030,
      "FR", 1960, 45659575
    )
  ) %>% 
  arrange(country_code, year)

df %>% filter(country_code == "FR")

cpop <- df %>% 
  group_by(country_code) %>% 
  summarise(max_pop = max(population)) 

plt_1 <- df %>% 
  filter(country_code %in% pull(filter(cpop, max_pop > 1e6), country_code)) %>% 
  group_by(country_code) %>% 
  mutate(pop_idx = population / first(population) * 100,
         cluster = case_when(country_code %in% c("BG", "HU", "HR", "LV", "EE", "LT", "RO") ~ "Up and Down",
                             country_code %in% c("PL", "SK", "SI", "EL", "PT", "CZ", "IT") ~ "Up and Flat",
                             country_code %in% c("IE", "ES", "DK", "FR", "NL", "FI", "BE", "UK", "AT", "CH", "NO", "SE") ~ "Up and Up",
                             country_code %in% c("DE") ~ "Germany",
                             TRUE ~ country_code),
         cluster = factor(cluster, levels = rev(c("Germany", "Up and Down", "Up and Flat", "Up and Up")))) %>% 
  ggplot(aes(x = year, y = pop_idx, group = country_code)) +
  geom_line(colour = "#003399") +
  facet_wrap(~cluster) +
  gghighlight(
    unhighlighted_params = list(colour = alpha("#FFCC00", 0.5))
    ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Four types of population development in selected European countries"
    ) +
  theme_minimal() +
  theme(
    text = element_text(family = "font",
                        size = 40,
                        colour = "#003399"),
    plot.title.position = "plot",
    plot.title = element_text(size = 50,
                              hjust = 0.5),
    plot.background = element_rect(fill = alpha("#FFCC00", 0.0),
                                   colour = alpha("#FFCC00", 0.0)),
    strip.text = element_text(size = 40,
                              colour = "#003399"),
    axis.text = element_text(colour = "#003399"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = alpha("#003399", 0.4),
                                    linewidth = 0.1)
  )


m <- df %>% 
  filter(country_code %in% pull(filter(cpop, max_pop > 1e6), country_code)) %>% 
  pivot_wider(names_from = country_code, values_from = population) %>% 
  mutate(year = paste(year, "01-01", sep = "-")) %>% 
  column_to_rownames(var = "year") %>% 
  as.xts() %>% 
  as.matrix() %>% 
  t()




d1 <- TSclust::diss(m, "COR")
d2 <- hclust(d1)

dhc <- as.dendrogram(d2)
data <- dendro_data(dhc, type = "rectangle")

plt_2 <- ggplot(segment(data)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
               colour = "#003399") + 
  geom_text(data = data$labels,
            aes(x = x, y = y - 0.05, label = label),
            family = "font",
            size = 12,
            colour = "#003399",
            hjust = "left") +
  geom_hline(yintercept = 0.575,
             linetype = "dotted",
             colour = "#003399") +
  coord_flip() +
  scale_y_reverse(expand = c(0.2, 0)) +
  labs(title = "Clustering") +
  theme_void() +
  theme(
    text = element_text(family = "font",
                        size = 40,
                        colour = "#003399"),
        plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = alpha("#FFCC00", 0.0),
                                       colour = alpha("#FFCC00", 0.0)),
        axis.text.x = element_text(colour = "#003399")
      )


layout <- "
AAAAAB
AAAAAB
AAAAAB
AAAAAB
"

plt_1 / plt_2+ 
  plot_layout(design = layout) +
  plot_annotation(
    title = str_to_upper("Population development in Europe 1960 - 2021"),
    caption = "Data: EuroStat",
    theme = theme(text = element_text(family = "font",
                                      colour = "#003399"),
                  plot.background = element_rect(fill = alpha("#FFCC00", 0.1)),
                  plot.title.position = "plot",
                  plot.title = element_text(size = 88,
                                            face = "bold",
                                            hjust = 0.5),
                  plot.caption = element_text(size = 40),
                  plot.margin = margin(b = 20, t = 20, r = 30, l = 30))
  )

## Saving ----
ggsave("./2023/20_correlation.png", bg = "white",
       height = 9, width = 12)

system("open ./2023/20_correlation.png")
