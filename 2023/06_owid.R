library(tidyverse)
library(janitor)
library(scales)
library(ggtext)
library(ggflags)
library(countrycode)
library(showtext)

txt <- "#212121"
bg <- "#a4b6ca"
## Fonts ----
font_add_google("Lato", "font")
showtext_auto()

t1 <- "Lead pollution is a widespread problem that receives little attention. 
Lead poisoning is estimated to account for about 1% of the global disease burden.1

This is a large burden for a problem that gets very little attention.

On an individual level, being exposed to lead in the environment can hinder a child’s brain development: it can result in a reduction in IQ; cognitive function; and has been linked to higher levels of antisocial behavior.2

But it’s a global problem that we can tackle. Many countries have already made significant progress against it."

t2 <- "Lanphear et al. (2005) Low-Level Environmental Lead Exposure and Children’s Intellectual Function: An International Pooled Analysis. Environmental Health Perspectives"

lead <- read_csv("./2023/data/children-lead-5micrograms.csv") %>% 
  clean_names() %>% rename(children = 4) %>% 
  filter(entity != "Total") %>% 
  mutate(iso2c = str_to_lower(countrycode(entity, "country.name", "iso2c")),
         label =  number_format(big.mark = ".", decimal.mark = ",")(children)) %>% 
  arrange(desc(children)) %>% 
  head(20)

lead %>% 
  ggplot(aes(x = children,
             y = fct_reorder(entity, children))) +
  geom_col(fill = "#6C6C6A") +
  geom_flag(aes(x = 0, country = iso2c),
            size = 7) +
  geom_text(data = filter(lead, children > 28e6),
            aes(label = label),
            colour = "white",
            hjust = 1.2,
            vjust = 0.25) +
  scale_x_continuous(labels = number_format(big.mark = ".", decimal.mark = ",")) +
  labs(x = NULL,
       y = NULL,
       title = "Lead Pollution") +
  theme_minimal() +
  theme(
    text = element_text(family = "font",
                        colour = txt),
    plot.background = element_rect(fill = bg, 
                                   colour = bg),
    axis.ticks.y = element_blank()
  )

