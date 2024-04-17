library(tidyverse)
library(rnaturalearth)
library(sf)
library(PrettyCols)
library(classInt)
library(showtext) 

font_add_google("Truculenta", "truculenta")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()

adb <- read_csv("./2024/data/KIDB-export-7ae48d9f-b8cd-4a61-b4b2-255a3020ae40.csv") |> 
  mutate(across(`2000`:`2023`, as.numeric)) |> 
  pivot_longer(cols = `2000`:`2023`, names_to = "year", values_to = "hdi") |> 
  select(country = Economy, year, hdi) |> 
  drop_na() |> 
  group_by(country) |> 
  filter(year == max(year)) |> 
  ungroup() |> 
  mutate(country = case_when(country == "China, People's Republic of" ~ "China",
                             country == "Lao People's Democratic Republic" ~ "Laos",
                             country == "Taipei,China" ~ "Taiwan",
                             country == "Viet Nam" ~ "Vietnam",
                             country == "Kyrgyz Republic" ~ "Kyrgyzstan",
                             country == "Korea, Republic of" ~ "South Korea",
                             TRUE ~ country))

world_map <- ne_countries(scale = "medium", returnclass = "sf") |> 
  left_join(
    adb,
    by = join_by(sovereignt == country)
  )

# bins
brk <- round(classIntervals(adb$hdi,
                            n = 5,
                            style = 'equal')$brks, 3)

cols <- MetBrewer::met.brewer("Hokusai2")
cols <- prettycols("Summer")
newcol <- colorRampPalette(cols)
ncols <- 7
cols2 <- newcol(ncols)
vmin <- min(adb$hdi, na.rm = TRUE)
vmax <- max(adb$hdi, na.rm = TRUE)
bg <- "white"


ggplot(data = world_map) +
  geom_sf(aes(fill = hdi)) +
scale_fill_gradientn(colours = cols2,
                     breaks = brk,
                     labels = brk,
                     limits = c(min(brk), max(brk)),
                     na.value = "grey65") +
  coord_sf(xlim = c(45, 180),
           ylim = c(-50, 55)) +
  labs(fill = "Human Development Index",
       title = "ASIAN DEVELOPMENT",
       subtitle = str_wrap("The Asian Development Bank envisions a prosperous, inclusive, resilient, and sustainable Asia and the Pacific, while sustaining its efforts to eradicate extreme poverty in the region.", 60),
       caption = "Source: Asian Development Bank") +
  guides(
    fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(4.15, units = "mm"),
      keywidth = unit(12, units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = TRUE,
      reverse = F,
      label.position = "bottom"
    )
  ) +
  theme_void() +
  theme(
    text = element_text(family = "robotoslab",
                        colour = "grey20",
                        size = 28),
    legend.position = "bottom",
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(family = "truculenta",
                              face = "bold",
                              size = 62),
    plot.subtitle = element_text(lineheight = 0.3,
                                 margin = margin(0,0,10,0)),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(
  "./2024/R/18_adb.png",
  bg = "white",
  width = 5,
  height = 5,
)

system("open ./2024/R/18_adb.png") 
