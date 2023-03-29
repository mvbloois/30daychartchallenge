library(tidyverse)
library(janitor)
library(ggplotify)
# devtools::install_github("mattflor/chorddiag")
library(circlize)

`%notin%` <- Negate(`%in%`)

eu <- 10
wrl <- 10

df <- read_delim("./2023/data/migr_asyappctza_tabular.tsv.gz") %>% 
  select(col_1 = 1, 7:16) %>% 
  separate_wider_delim(cols = col_1, delim = ",", names = c("freq", "citizen", "sex", "unit", "age", "asyl_app",  "geo")) %>% 
  filter(sex == "T") %>% 
  filter(age == "TOTAL") %>% 
  filter(asyl_app == "ASY_APP") %>%
  filter(citizen %notin% c("TOTAL", "EU27_2020", "EU28", "EXT_EU27_2020", "EXT_EU28")) %>% 
  filter(geo %notin% c("EU27_2020", "UK")) %>% 
  mutate(across(`2013 `:`2022 `, ~str_replace(., ": ", "0") %>% parse_integer)) %>% 
  rowwise() %>% 
  mutate(applicants = sum(c_across(`2013 `:`2022 `)))

orig <- df %>% 
  select(citizen, geo, applicants) %>% 
  group_by(citizen) %>% 
  summarise(applicants = sum(applicants, na.rm = TRUE),
            .groups = "drop") %>% 
  arrange(desc(applicants)) %>% 
  mutate(perc = applicants / sum(applicants, na.rm = TRUE),
         perc_cum = cumsum(perc)) %>% 
  pull(citizen)

dest <- df %>% 
  select(citizen, geo, applicants) %>% 
  group_by(geo) %>% 
  summarise(applicants = sum(applicants, na.rm = TRUE),
            .groups = "drop") %>% 
  arrange(desc(applicants)) %>% 
  mutate(perc = applicants / sum(applicants, na.rm = TRUE),
         perc_cum = cumsum(perc)) %>% 
  pull(geo)

plot_data <- df %>% 
  select(citizen, geo, applicants) %>% 
  mutate(citizen = ifelse(citizen %in% orig[1:wrl], citizen, "RoW"),
         geo = ifelse(geo %in% dest[1:eu], geo, "RoEU")) %>% 
  group_by(citizen, geo) %>% 
  summarise(applicants = sum(applicants, na.rm = TRUE)) %>% 
  pivot_wider(names_from = geo, values_from = applicants, values_fill = 0) %>% 
  column_to_rownames("citizen") 

order <- c(dest[1:eu], "RoEU", orig[1:wrl], "RoW") %>% unique()

circos.clear()
circos.par(start.degree = 45, 
           gap.degree = 1, 
           track.margin = c(-0.1, 0.1), 
           points.overflow.warning = FALSE)

plt <- as.ggplot(~chordDiagram(x = plot_data %>% 
                          as.matrix(),
             order = order,
             directional = 1,
             transparency = 0.5)
)

plt +
  labs(title = "Asylum applicants in the EU + CH",
       subtitle = "Exclusing the UK: 2013-2022",
       caption = "Source: Eurostat")

circos.clear()
