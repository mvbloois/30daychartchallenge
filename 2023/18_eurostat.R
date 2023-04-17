library(tidyverse)
library(showtext)

## Fonts ----
font_add_google("Fira Code", "font")
showtext_auto()

bg = "#080034"
chalk <- "#FBF7F5"

tertiary <- readxl::read_xlsx("./2023/data/educ_uoe_grad09_spreadsheet.xlsx",
                  sheet = 3,
                  skip = 7) %>% 
  janitor::clean_names() %>% 
  select(-x3, -x5, -x7, -x9, -x11, -x13) %>% 
  drop_na() %>% 
  mutate(across(x2015:x2020, as.character)) %>% 
  pivot_longer(cols = starts_with("x"), names_to = "year", names_prefix = "x", values_to = "ratio") %>% 
  rename(country = time) %>% 
  mutate(ratio = as.numeric(ratio),
         country = ifelse(str_detect(country, "Germany"), "Germany", country),
         level = "tertiary") %>%
  drop_na()

bachelor <- readxl::read_xlsx("./2023/data/educ_uoe_grad09_spreadsheet.xlsx",
                              sheet = 5,
                              skip = 7) %>% 
  janitor::clean_names() %>% 
  select(-x3, -x5, -x7, -x9, -x11, -x13) %>% 
  drop_na() %>% 
  mutate(across(x2015:x2020, as.character)) %>% 
  pivot_longer(cols = starts_with("x"), names_to = "year", names_prefix = "x", values_to = "ratio") %>% 
  rename(country = time) %>% 
  mutate(ratio = as.numeric(ratio),
         country = ifelse(str_detect(country, "Germany"), "Germany", country),
         level = "bachelor") %>%
  drop_na()

master <- readxl::read_xlsx("./2023/data/educ_uoe_grad09_spreadsheet.xlsx",
                              sheet = 6,
                              skip = 7) %>% 
  janitor::clean_names() %>% 
  select(-x3, -x5, -x7, -x9, -x11, -x13) %>% 
  drop_na() %>% 
  mutate(across(x2015:x2020, as.character)) %>% 
  pivot_longer(cols = starts_with("x"), names_to = "year", names_prefix = "x", values_to = "ratio") %>% 
  rename(country = time) %>% 
  mutate(ratio = as.numeric(ratio),
         country = ifelse(str_detect(country, "Germany"), "Germany", country),
         level = "master") %>%
  drop_na()

doctor <- readxl::read_xlsx("./2023/data/educ_uoe_grad09_spreadsheet.xlsx",
                            sheet = 7,
                            skip = 7) %>% 
  janitor::clean_names() %>% 
  select(-x3, -x5, -x7, -x9, -x11, -x13) %>% 
  drop_na() %>% 
  mutate(across(x2015:x2020, as.character)) %>% 
  pivot_longer(cols = starts_with("x"), names_to = "year", names_prefix = "x", values_to = "ratio") %>% 
  rename(country = time) %>% 
  mutate(ratio = as.numeric(ratio),
         country = ifelse(str_detect(country, "Germany"), "Germany", country),
         level = "doctoral") %>%
  drop_na()

plot_data <- bind_rows(
  tertiary,
  bachelor,
  master,
  doctor
) %>% 
  filter(!str_detect(country, "European")) %>% 
  mutate(level = factor(level, levels = c("doctoral", "master", "bachelor", "tertiary"))) %>% 
  group_by(level) %>% 
  filter(year == max(year)) %>% 
  mutate(country = factor(country))

minmax <- plot_data %>% 
  group_by(country) %>% 
  summarise(min_ratio = min(ratio),
            max_ratio = max(ratio)) %>% 
  arrange(desc(country))

plot_data %>% 
  ggplot(aes(x = ratio, y = fct_rev(country))) +
  geom_vline(aes(xintercept = 100),
             linewidth = 1.2,
             colour = PrettyCols::prettycols("Summer")[12]) +
  geom_segment(data = minmax,
               aes(x = min_ratio, xend = max_ratio,
                   y = fct_rev(country), yend = fct_rev(country)),
               colour = "grey35") +
  geom_point(aes(colour = level),
             size = 4) +
  PrettyCols::scale_colour_pretty_d("Summer") +
  labs(x = "women per 100 men",
       y = NULL,
       subtitle = "Women score better than men in tertiary education\nbut are underrepresented at the doctoral level",
       title = "Graduates in tertiary education: 2020 or latest year available",
       caption = "Data: Eurostat") +
  theme_minimal() +
  theme(
    text = element_text(family = "font",
                        size = 40,
                        colour = bg),
    plot.background = element_rect(fill = chalk, colour = chalk),
    legend.position = "top",
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5,
                              vjust = 5,
                              size  = 34),
    plot.subtitle = element_text(hjust = 0.5,
                                 size = 80,
                                 lineheight = 0.35),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(b = 20, t = 30, r = 20, l = 20)
  )

## Saving ----
ggsave("./2023/18_eurostat.png",
       bg = bg,
       height = 9, width = 12)

system("open ./2023/18_eurostat.png")

