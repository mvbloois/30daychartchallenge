library(tidyverse)
library(readxl)
library(PrettyCols)
library(showtext) 

font_add_google("Bebas Neue", "font")
showtext_auto()

df <- excel_sheets("./2024/data/worldcup_2018.xlsx") |> 
  set_names() |> 
  map(read_xlsx, path = "./2024/data/worldcup_2018.xlsx") |> 
  reduce(bind_rows) |> 
  janitor::clean_names() |> 
  mutate(yas = case_when(pos == "GK" ~ -0.001,
                         pos == "DF" ~ -0.004,
                         pos == "MF" ~ -0.007,
                         pos == "FW" ~ -0.01),
         pos = factor(pos, levels = c("GK", "DF", "MF", "FW")))

df_q <- df |> 
  summarise(quantiles = quantile(height),
            .by = pos) |> 
  bind_cols(q = rep(c(0, 0.25, 0.5, 0.75, 1),4)) |> 
  mutate(yas = case_when(pos == "GK" ~ -0.002,
                         pos == "DF" ~ -0.005,
                         pos == "MF" ~ -0.008,
                         pos == "FW" ~ -0.011)) |> 
  pivot_wider(names_from = q, values_from = quantiles) |> 
  mutate(lbl = case_when(pos == "GK" ~ "Goalkeepers",
                         pos == "DF" ~ "Defenders",
                         pos == "MF" ~ "Midfielders",
                         pos == "FW" ~ "Forwards"))



df |> 
  select(pos, height, yas) |> 
  ggplot(aes(x = height, colour = pos, fill = pos)) +
  geom_density(alpha = 0.5) +
  geom_segment(data = df_q,
               aes(x = `0`, xend = `1`, y = yas,
                   colour = pos),
               linewidth = 0.5) +
  geom_segment(data = df_q,
               aes(x = `0.25`, xend = `0.75`, y = yas,
                   colour = pos),
               linewidth = 1.5) +
  geom_point(data = df_q,
             aes(x = `0.5`, y = yas,
                 colour = pos),
             size = 4,
             inherit.aes = FALSE) +
  geom_text(data = df_q,
            aes(x = 164.5, y = yas, label = lbl, colour = pos),
            family = "font",
            size = 11,
            hjust = 1) +
  PrettyCols::scale_fill_pretty_d("Bold") +
  PrettyCols::scale_colour_pretty_d("Bold") +
  labs(x = "Height (in cm)",
       y = NULL,
       title = "PHYSICAL",
       subtitle = "The heights of players at the 2018 World Cup",
       caption = "Data: FIFA via www.topendsports.com") +
  expand_limits(x = c(160, 210)) +
  theme_minimal() +
  theme(
    text = element_text(family = "font",
                        size = 24),
    legend.position = "none",
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(size = 82),
    plot.subtitle = element_text(size = 42,
                                     lineheight = 0.35),
    plot.caption = element_text(size = 22),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(
  "./2024/R/10_physical.png",
  bg = "white",
  width = 7,
  height = 6,
)

system("open ./2024/R/10_physical.png")
