library(tidyverse)
library(janitor)
library(showtext) 

font_add_google("Truculenta", "truculenta")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()
# Download and read file
download.file(
  "https://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/daggegevens/etmgeg_344.zip",
  "./2024/data/etmgeg_344.zip"
)

rt_raw <-
  read_delim("./2024/data/etmgeg_344.zip", delim = ",", skip = 50) |>
  clean_names()

fs::file_delete("./2024/data/etmgeg_344.zip")

plot_data <- rt_raw |> 
  mutate(date = ymd(yyyymmdd),
         max_temp = parse_number(tx)/ 10) |> 
  select(date, max_temp) |> 
  mutate(yr = year(date),
         mt = month(date),
         yr_mt = paste(yr , mt),
         dy = day(date)) |> 
  summarise(max_temp = max(max_temp),
            .by = yr_mt) |> 
  separate(yr_mt, into = c("yr", "mt")) |> 
  mutate(mt = factor(mt, levels = 1:12)) |> 
  filter(yr > 1961, yr < 2024)
  
plot_data |>   
  ggplot(aes(x = yr, y = fct_rev(mt), fill = max_temp)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#003BAE", "#15AB00", "#FF8106", "#B44010", "#FF2B00"),
                       breaks = c(10,20,30),
                       na.value = "white",
                       name = "temperature") +
  labs(title = "HEATMAP",
       subtitle = "Monthly maximum temperatures in Rotterdam between 1961 and 2023.",
       caption = "Source: KNMI") +
  coord_fixed() +
  theme_void() +
  theme(
    text = element_text(family = "robotoslab",
                        size = 28),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5),
    plot.title = element_text(family = "truculenta",
                              size = 68,
                              face = "bold"),
    plot.subtitle = element_text(size = 32,
                                 margin = margin(0, 0, 15, 0)),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(
  "./2024/R/14_heatmap.png",
  bg = "white",
  width = 10,
  height = 4,
)

system("open ./2024/R/14_heatmap.png")
