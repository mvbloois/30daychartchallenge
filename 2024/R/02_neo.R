
library(tidyverse)
library(readxl)
library(showtext) 

font_add_google("Poppins", "poppins")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()

bg <- "#caf0fc"
fg <- "#006d8c"

plot_data <- read_xlsx("./2024/data/NKR-export-29_03_2024_05_14_33.xlsx",
          skip = 7) |> 
  select(cancer = Kankersoort,
         year_of_diagnosis = `Jaar van diagnose`,
         prevalence = Aantal) |> 
  mutate(year_of_diagnosis = parse_number(year_of_diagnosis)) |> 
  mutate(cancer = case_when(cancer == "Longkanker" ~ "Lung cancer",
                            cancer == "Dikkedarm- en endeldarmkanker" ~ "Colorectal cancer",
                            cancer == "Borstkanker" ~ "Breast cancer",
                            cancer == "Prostaatkanker" ~ "Prostate cancer"))

txt <- tibble(
  cancer = c("Lung cancer", "Colorectal cancer", "Breast cancer", "Prostate cancer"),
  x = c(1989, 2010, 1995, 1997),
  y = c(9400, 15500, 11000, 5500)
)

plot_data |> 
  ggplot(aes(x = year_of_diagnosis, y = prevalence,
             group = cancer)) +
  geom_line(aes(colour = cancer),
            linewidth = 1) +
  geom_text(data = txt,
            aes(x, y, label = cancer,
                colour = cancer),
            size = 12,
            fontface = "bold") +
  scale_x_continuous(limits = c(1987, 2025)) +
  scale_y_continuous(limits = c(0, 17e3),
                     labels = scales::number_format()) +
  labs(
    x = "Year of diagnosis",
    y = NULL,
    title = toupper("Incidence of neoplasms"),
    subtitle = str_wrap("Incidence for four types of cancers in the Netherlands between 1989 and 2023.", 55),
    caption = "Data: NKR Cijfers / IKNL") +
  theme_minimal() +
  theme(
    text = element_text(family = "poppins",
                        colour = fg),
    legend.position = "none",
    plot.title.position = "plot",
    plot.title = element_text(size = 88),
    plot.subtitle = element_text(size = 54,
                                 lineheight = 0.3),
    plot.caption = element_text(size = 42),
    plot.background = element_rect(fill = bg,
                                   colour = bg),
    panel.grid.major = element_line(colour = fg,
                                    linewidth = 0.1),
    axis.text = element_text(size = 30,
                             colour = fg),
    axis.title.x = element_text(size = 30),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(
  "./2024/R/02_neo.png",
  bg = bg,
  width = 8,
  height = 6,
)

system("open ./2024/R/02_neo.png")

