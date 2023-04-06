
library(rvest)

`%notin%` <- Negate(`%in%`)

url <- "https://en.wikipedia.org/wiki/Shark_attack"

tbl <- read_html(url) %>% 
   rvest::html_nodes("table.wikitable") %>% 
   rvest::html_table(fill = TRUE)

table <- tbl[[1]]

table %>% 
  head(14) %>% 
  mutate(Totalattacks = str_remove(Totalattacks, "\\[.+") %>% parse_integer(.),
         Fatalattacks = str_remove(Fatalattacks, "\\[.+") %>% parse_integer(.),
         Lastfatality = str_remove(Lastfatality, "\\[.+") %>% parse_integer(.),
         Region = str_replace(Region, "\\(", " \\("),
         perc = Fatalattacks / Totalattacks) %>% 
  ggplot(aes(y = fct_reorder(Region, Totalattacks))) +
  geom_col(aes(x = Totalattacks)) +
  geom_col(aes(x = Fatalattacks), fill = "darkred") +
  geom_text(aes(x = -30, label = scales::percent_format(accuracy = 1L)(perc))) +
  theme_minimal()
  

