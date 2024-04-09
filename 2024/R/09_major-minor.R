
library(tidyverse)
library(ggtext)
library(showtext) 

font_add_google("Poppins", "font")
showtext_auto()

#  https://songdata.io/playlist/37i9dQZF1DX1qjRF3HScOB

pal <- c("#FFB921", "#0047AC")

df <- readxl::read_xlsx("./2024/data/kinkfm.xlsx",
                  col_names = c("x1","song", "rank", "artist", "key")) 

subtitle <- "Songs in a <span style = 'color:#FFB921;'>major</span> key outnumber songs in a
<span style = 'color:#0047AC;'>minor</span> key in the KINK1500,<br>a list of alternative rock songs."
 
df  |> 
  count(key, sort = TRUE) |> 
  mutate(key = str_replace(key, "♭", "b"),
         key = str_replace(key, "♯", "#")) |> 
  mutate(colour = str_extract(key, "Major|Minor")) |> 
  ggplot(aes(x = n, y = fct_reorder(key, n ),
             fill = colour)) +
  geom_col() +
  scale_fill_manual(values = pal) +
  labs(x = "# of Songs",
       y = NULL,
       title = "Major - Minor",
       subtitle = subtitle,
       caption = "Data: songdata.io") +
  theme_minimal() +
    theme(
      text = element_text(family = "font",
                          size = 24),
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.y = element_text(hjust = 1,
                                 margin = margin(r = -20)),
      plot.title.position = "plot",
      plot.title = element_markdown(size = 62),
      plot.subtitle = element_markdown(size = 42,
                                       lineheight = 0.35),
      plot.caption = element_text(size = 22),
      plot.margin = margin(10, 10, 10, 10)
    )

ggsave(
  "./2024/R/09_major-minor.png",
  bg = "white",
  width = 7,
  height = 6,
)

system("open ./2024/R/09_major-minor.png")

