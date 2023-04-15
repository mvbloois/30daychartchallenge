# https://milospopovic.net/using-api-make-maps-r/
# https://github.com/cran/cubble
library(tidyverse)
library(janitor)
library(sf)
library(giscoR)
library(classInt)

windowsFonts(georg = windowsFont('Georgia'))

europe <- giscoR::gisco_get_countries(
  year = "2020",
  epsg = "4326",
  resolution = "10"
)

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"
crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

bb <- st_sfc(
  st_polygon(list(cbind(
    c(-10.6600, 33.00, 33.00, -10.6600, -10.6600),
    c(32.5000, 32.5000, 71.0500, 71.0500, 32.5000) 
  ))),
  crs = crsLONGLAT)

laeabb <- st_transform(bb, crs = crsLAEA)
b <- st_bbox(laeabb)

df <- read_delim("./2023/data/ilc_peps01n_tabular.tsv.gz") %>% 
  clean_names() %>% 
  separate(col = freq_unit_age_sex_geo_time_period,
           into = c("freq", "unit", "age", "sex", "country"),
           sep = ",") %>% 
  pivot_longer(starts_with("x"), names_to = "year", values_to = "values") %>% 
  filter(values != ": ") %>% 
  filter(!country %in% c("EU", "EU27_2007", "EU27_2020", "EU28")) %>%
  filter(sex == "T") %>% 
  filter(age == "Y_LT16") %>% 
  filter(unit == "PC") %>% 
  mutate(year = str_remove(year, "x"),
         values = parse_number(values)) %>% 
  group_by(country) %>% 
  filter(year == max(year))

# bins
brk <- round(classIntervals(df$values, 
                            n = 6, 
                            style = 'equal')$brks, 0)

# define the color palette
cols = rev(c('#05204d', '#004290', '#6458ae', '#dd98d1', '#eab0a2'))
cols <- MetBrewer::met.brewer("Hokusai2")
newcol <- colorRampPalette(cols)
ncols <- 7
cols2 <- newcol(ncols)
vmin <- min(df$values, na.rm=T)
vmax <- max(df$values, na.rm=T)
bg <- "white"

plot_data <- europe %>% 
  left_join(
    df,
    by = join_by(CNTR_ID == country)
  )

ggplot() +
  geom_sf(data = plot_data,
          aes(fill = values),
          colour = bg) +
  coord_sf(crs = crsLAEA, 
           xlim = c(b["xmin"], b["xmax"]), 
           ylim = c(b["ymin"], b["ymax"])) +
  scale_fill_gradientn(name="% children at risk of poverty or social exclusion",
                       colours=cols2,
                       breaks=brk,
                       labels=brk,
                       limits = c(min(brk),max(brk)),
                       na.value = "grey65") +
  labs(
    title = "Poverty and social exclusion in Europe",
    subtitle = str_wrap("Children under 16 years who are at risk of poverty or severely materially or socially deprived or living in households with very low work intensity.", 75),
    caption = "\nData: EuroStat"
  ) +
  guides(fill=guide_legend(
    direction = "horizontal",
    keyheight = unit(1.15, units = "mm"),
    keywidth = unit(12, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = TRUE,
    reverse = F,
    label.position = "bottom"
  )) +
  theme(
    text = element_text(family = "georg",
                        size = 10),
    plot.background = element_rect(fill = bg),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(face = "italic",
                                 size = 12),
    panel.background = element_rect(fill = bg),
    panel.grid = element_line(colour = "grey95"),
    legend.background = element_rect(fill = bg),
    legend.position = c(.5, .01),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

## Saving ----
ggsave("./2023/16_family.png",
       bg = bg)

system("open ./2023/16_family.png")
