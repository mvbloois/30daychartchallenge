# https://data.ipu.org/women-ranking?month=3&year=2023
# https://www.riinu.me/2022/02/world-map-ggplot2/
library(tidyverse)
library(ggthemes)
library(gghalves)
library(classInt)
library(patchwork)
library(showtext)

font_add(family = "titlefont", "./resources/GothamBold.ttf")
font_add(family = "subtitlefont", "./resources/GothamMedium.ttf")
font_add(family = "font", "./resources/GothamBook.ttf")
showtext_auto()

primary <- "#0397D6"
pink <- "#FF8ADE"

df_2003 <-
  readxl::read_xlsx("./2023/data/stand_2003.xlsx", skip = 1) %>%
  janitor::clean_names() %>%
  select(country = 2,
         seats = seats_4,
         women = women_5) %>%
  mutate(
    country = str_remove_all(country, "\\*"),
    women = parse_number(women),
    share_2003 = women / seats
  ) %>%
  select(country, share_2003) %>%
  mutate(
    country = case_when(
      country == "Dem. Rep. of East Timor" ~ "Timor-Leste",
      country == "United Rep. of Tanzania" ~ "Tanzania",
      country == "Lao People's Democratic Rep." ~ "Laos",
      country == "The f.Y.R. of Macedonia" ~ "North Macedonia",
      country == "Saint Vincent & the Grenadines" ~ "Saint Vincent and the Grenadines",
      country == "Dem. People's Rep. of Korea" ~ "North Korea",
      country == "Iran (Islamic Rep. of)" ~ "Iran",
      country == "Micronesia (Fed. States of)" ~ "Micronesia",
      country == "Cote d'Ivoire" ~ "Ivory Coast",
      
      TRUE ~ country
    )
  )

df_2023 <-
  read_csv("./2023/data/chamber--current_women_percent.csv",
           skip = 5) %>%
  select(
    country = 2,
    elections = 3,
    seats = 4,
    women = 5
  ) %>%
  mutate(
    seats = parse_number(seats),
    women = parse_number(women),
    share_2023 = women / seats
  ) %>%
  drop_na() %>%
  select(country, share_2023) %>%
  mutate(
    country = case_when(
      str_detect(country, "Bolivia") ~ "Bolivia",
      country == "Cabo Verde" ~ "Cape Verde",
      country == "United Republic of Tanzania" ~
        "Tanzania",
      country == "Lao People's Democratic Republic" ~ "Laos",
      country == "Democratic People's Republic of Korea" ~ "North Korea",
      country == "Iran (Islamic Republic of)" ~ "Iran",
      country == "Eswatini" ~ "Swaziland",
      country == "Micronesia (Federated States of)" ~ "Micronesia",
      country == "Côte d'Ivoire" ~ "Ivory Coast",
      country == "Gambia (The)" ~ "Gambia",
      country == "Türkiye" ~ "Turkey",
      TRUE ~ country
    )
  )

plot_data <- df_2023 %>%
  left_join(df_2003,
            by = join_by(country)) %>%
  mutate(direction = ifelse(share_2023 >= share_2003, "Y", "N")) %>%
  pivot_longer(
    cols = starts_with("share"),
    names_to = "year",
    names_prefix = "share_",
    values_to = "share"
  )


df_2023_map <- df_2023 %>%
  mutate(
    country = case_when(
      str_detect(country, "Bolivia") ~ "Bolivia",
      country == "Democratic People's Republic of Korea" ~ "North Korea",
      country == "Russian Federation" ~ "Russia",
      country == "United States of America" ~ "USA",
      country == "United Kingdom" ~ "UK",
      country == "Republic of Moldova" ~ "Moldova",
      country == "United Republic of Tanzania" ~
        "Tanzania",
      country == "Viet Nam" ~ "Vietnam",
      country == "Lao People's Democratic Republic" ~ "Laos",
      country == "Republic of Korea" ~ "South Korea",
      country == "Eswatini" ~ "Swaziland",
      country == "Syrian Arab Republic" ~ "Syria",
      country == "Congo" ~ "Republic of Congo",
      country == "Antigua and Barbuda" ~ "Antigua",
      country == "Cabo Verde" ~ "Cape Verde",
      country == "Saint Kitts and Nevis" ~ "Saint Kitts",
      country == "Trinidad and Tobago" ~ "Trinidad",
      country == "Saint Vincent and the Grenadines" ~ "Saint Vincent",
      country == "Brunei Darussalam" ~ "Brunei",
      TRUE ~ country
    )
  ) %>%
  add_row(country = "Nigeria", share_2023 = 0.03611111)

# bins
brk <- c(0.00, 0.10, 0.20, 0.3, 0.4, 0.5, 0.6)
cols <- PrettyCols::prettycols("Teals", direction = -1)

newcol <- colorRampPalette(cols)
ncols <- 7
cols2 <- newcol(ncols)
vmin <- min(df_2023_map$share_2023, na.rm = TRUE)
vmax <- max(df_2023_map$share_2023, na.rm = TRUE)

world_map <-  map_data("world") %>%
  filter(!long > 180)

countries <-  world_map %>%
  distinct(region) %>%
  rowid_to_column() %>%
  left_join(df_2023_map,
            by = join_by(region == country))

## Plots ----

plot <- plot_data %>%
  ggplot(aes(x = year, y = share)) +
  geom_point(aes(colour = direction),
             size = 15,
             shape = "-") +
  geom_line(aes(group = country,
                colour = direction),
            alpha = 0.4) +
  geom_half_violin(
    data = plot_data %>% filter(year == 2003),
    nudge = 0.1,
    colour = "#006666",
    fill = "#66B2B2"
  ) +
  geom_half_violin(
    data = plot_data %>% filter(year == 2023),
    nudge = 0.1,
    side = "r",
    colour = "#006666",
    fill = "#66B2B2"
  ) +
  scale_y_continuous(
    limits = c(0, .65),
    breaks = c(0, .25, .50),
    labels = scales::label_percent()
  ) +
  scale_colour_manual(values = c("Y" = primary, "N" = "#960045")) +
  labs(subtitle = "Share of women in national assemblies 2003-2023",
       x = NULL,
       y = NULL) +
  guides(colour = "none") +
  theme_minimal() +
  theme(
    text = element_text(family = "font",
                        size = 50),
    plot.subtitle = element_text(family = "titlefont",
                              size = 30,
                              hjust = 0,
                              vjust = -5),
    plot.background = element_rect(fill = "#F0F8FF",
                                   colour = "#F0F8FF"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )


map <- countries %>%
  ggplot(aes(fill = share_2023, map_id = region)) +
  geom_map(map = world_map,
           colour = "white",
           
           linewidth = 0.1) +
  scale_fill_gradientn(
    name = str_wrap("Share of seats held by women in 2023", 14),
    colours = cols2,
    breaks = brk,
    labels = scales::label_percent(accuracy = 1L)(brk),
    limits = c(min(brk), max(brk))
  ) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  coord_map("moll") +
  guides(
    fill = guide_legend(
      direction = "vertical",
      #keyheight = unit(3.15, units = "mm"),
      #keywidth = unit(12, units = "mm"),
      #title.position = 'top',
      #title.hjust = 0.5,
      title.vjust = -5,
      #label.hjust = 0.5,
      #label.vjust = 5,
      #nrow = 1,
      #byrow = T,
      reverse = T,
      label.position = "right"
    )
  ) +
  theme_map() +
  theme(
    text = element_text(family = "font",
                        size = 40),
    plot.background = element_rect(fill = "#F0F8FF",
                                   colour = "#F0F8FF"),
    legend.background = element_rect(fill = "#F0F8FF"),
    legend.title = element_text(lineheight = 0.3),
    legend.position = c(.01, .2),
  )

lo <- "
ABBBBBB
ABBBBBB
ABBBBBB
"
lo <- "
BBBBBBB
BBBBBBB
AAAAAAA
"

plot / map +
  plot_layout(design = lo) +
  plot_annotation(
    title = "UN WOMEN: Women in National Assemblies",
    subtitle = str_wrap("From the local to the global level, women's leadership and political participation are restricted. Women are underrepresented as voters, as well as in leading positions, whether in elected office, the civil service, the private sector or academia. This occurs despite their proven abilities as leaders and agents of change, and their right to participate equally in democratic governance.", 100),
    caption = "Data: IPU Parline"
    ) & 
  theme(
    plot.title = element_text("titlefont",
                        size = 92),
    plot.subtitle = element_text("subfont",
                              size = 50,
                              lineheight = 0.3),
    plot.caption = element_text("font",
                              size = 35),
    plot.background = element_rect(fill = "#F0F8FF",
                                   colour = "#F0F8FF")
  )


## Saving ----
ggsave(
  "./2023/24_un-woman.png",
  bg = "#F0F8FF",
  height = 9,
  width = 12
)

system("open ./2023/24_un-woman.png")
