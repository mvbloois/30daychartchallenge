library(tidyverse)

df <- read_csv("./2023/data/annual-working-hours-vs-gdp-per-capita-pwt.csv") %>% 
  janitor::clean_names() %>% 
  mutate(grp = case_when(code %in% c("NLD", "ITA", "ESP", "PRT", "GRC") ~ "wine",
                         code %in% c("BRA", "IND", "RUS", "CHN", "ZAF") ~ "BRICS",
                         code %in% c("JAP", "KOR", "AUS", "NZL", "TWN") ~ "None West")
         ) %>% 
  filter(!is.na(grp))



df %>% 
  filter(year > 1949) %>% 
  ggplot(aes(x = gdp_per_capita_output_multiple_price_benchmarks,
             y = annual_working_hours_per_worker,
             colour = code)) +
  geom_path() +
  facet_wrap(~grp)



df %>% 
  filter(year > 1949) %>% 
  filter(code %in% c("BRA", "IND", "RUS", "CHN", "ZAF")) %>% 
  ggplot(aes(x = gdp_per_capita_output_multiple_price_benchmarks,
             y = annual_working_hours_per_worker,
             colour = code)) +
  geom_path()

df %>% 
  filter(year > 1949) %>% 
  filter(code %in% c("JAP", "KOR", "AUS", "NZL", "TWN")) %>% 
  ggplot(aes(x = gdp_per_capita_output_multiple_price_benchmarks,
             y = annual_working_hours_per_worker,
             colour = code)) +
  geom_path()
