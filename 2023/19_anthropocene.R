library(tidyverse)
library(dplyr)
library(ggplot2)

## https://stackoverflow.com/questions/41391271/plot-an-archimedean-spiral-using-integer-values-with-ggplot2


france <- rvest::read_html("https://www.atomicarchive.com/almanac/test-sites/french-testing.html") %>% 
  rvest::html_element("table") %>% 
  rvest::html_table() %>% 
  janitor::clean_names() %>% 
  mutate(country = "France")

china <- rvest::read_html("https://www.atomicarchive.com/almanac/test-sites/prc-testing.html") %>% 
  rvest::html_element("table") %>% 
  rvest::html_table() %>% 
  janitor::clean_names() %>% 
  mutate(country = "China")

soviet <- rvest::read_html("https://www.atomicarchive.com/almanac/test-sites/soviet-testing.html") %>% 
  rvest::html_element("table") %>% 
  rvest::html_table() %>% 
  janitor::clean_names() %>% 
  mutate(country = "Soviet Union")

uk <- rvest::read_html("https://www.atomicarchive.com/almanac/test-sites/uk-testing.html") %>% 
  rvest::html_element("table") %>% 
  rvest::html_table() %>% 
  janitor::clean_names() %>% 
  mutate(country = "United Kingdom") %>% 
  select(-notes)

us <- rvest::read_html("https://www.atomicarchive.com/almanac/test-sites/us-testing.html") %>% 
  rvest::html_element("table") %>% 
  rvest::html_table() %>% 
  janitor::clean_names() %>% 
  mutate(country = "USA",
         tests = as.numeric(tests),
         years = ifelse(str_length(years) > 4, paste0(str_sub(years, 1, 5), "19", str_sub(years, 6, 7)), years)
  )

df <- bind_rows(
  france,
  china,
  soviet,
  uk,
  us
) %>% 
  filter(years != "Totals") %>% 
  filter(years != "") %>% 
  select(country, years, tests, total_yield_kilotons) %>% 
  drop_na() %>% 
  mutate(total_yield_kilotons = parse_number(total_yield_kilotons)) %>% 
  mutate(x_axis = ifelse(str_length(years) == 4,
                         as.numeric(years),
                         round((as.numeric(str_extract(years, "[0-9]{4}+$")) + as.numeric(str_extract(years, "^[0-9]{4}+")) + 1) / 2, 0)
  )
  )



df %>% 
  
  ggplot(aes(x = x_axis, y = tests, fill = country)) +
  geom_col(position = "stack") 













a <- 2
b <- 3
theta <- seq(0,25*pi,0.01)
r <- a + b*theta
df <- tibble(
         x=r*cos(theta),
         y=r*sin(theta)
         ) %>% # Cartesian coords
  mutate(rn = row_number(),
         age = case_when(rn <= 42 ~ "Meghalayan",
                         rn <= 82 ~ "Northgrippian",
                         rn <= 117 ~ "Greenlandia",
                         rn <= 1290 ~ "Upper Taratian",
                         rn <= 7740 ~ "Chibanian",
                         TRUE ~ "older"))

ggplot(df) +
  geom_path(aes(x, y, group = 1, colour = age),
            linewidth = 1) +
  ggthemes::scale_color_tableau() +
  theme_minimal()


df
