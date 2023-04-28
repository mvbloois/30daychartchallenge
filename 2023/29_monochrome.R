library(tidyverse)
library(janitor)
library(aRtsy)
library(showtext)

font_add("font", "./resources/JMH Typewriter.otf")
showtext_auto()

set.seed(1)
canvas <- canvas_segments(colors = "grey", p = 0) 


colors <- c("grey")
background <-  "#F9FbFF"
n <-  250
H <- 0.1
p <-  0
size <-  0.2

full_canvas <- data.frame(
    x = numeric(), xend = numeric(),
    y = numeric(), yend = numeric()
  )
for (i in 1:n) {
    x <- stats::runif(1, min = 1982, max = 2080)
    y <- stats::runif(1, 0, 25)
    k <- H * (1 - sqrt(stats::runif(1, 1982, 2080)))
    if (stats::runif(1, 0, 1) > p) {
      row <- data.frame(x = x - k, xend = x + k, y = y, yend = y, col = sample(colors, size = 1))
    } else {
      row <- data.frame(x = x, xend = x, y = y - k, yend = y + k, col = sample(colors, size = 1))
    }
full_canvas <- rbind(full_canvas, row)
}
sizes <- sample(size, size = n, replace = TRUE)
ggplot2::ggplot(data = full_canvas, mapping = ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +theme_void()



df <- read_csv2("./2023/data/Bevolking op 1 januari (x mln).csv") %>% 
  clean_names() %>% 
  separate(prognose_interval_95_percent,
           into = c("low95", "hi95"),
           sep = " - ") %>% 
  separate(prognose_interval_67_percent,
           into = c("low67", "hi67"),
           sep = " - ") %>% 
  mutate(
    across(low95:hi67, ~str_replace(., ",", ".") %>% parse_number)
    )

df_even <- df %>% 
  filter(jaar %% 2 == 0)

df_odd <- df %>% 
  filter(jaar %% 2 != 0)

ft_sz <- 14

df %>% 
  ggplot(aes(x = jaar, y = waarneming)) +
  geom_segment(data = full_canvas,
               aes(x = x, y = y, xend = xend, yend = yend),
               colour = "grey90", size = 0.2) +
  geom_ribbon(aes(ymin = low95, ymax = hi95),
              fill = "grey80",
              alpha = 0.8) +
  geom_ribbon(aes(ymin = low67, ymax = hi67),
              fill = "grey60",
               alpha = 0.8) +
  geom_text(data = df_even,
            aes(label = "x"),
            family = "font",
            size = ft_sz) +
  geom_text(data = df_even,
            aes(y = prognose, label = "+"),
             family = "font",
            size = ft_sz) +
  geom_text(data = df_odd,
            aes(label = "-"),
            family = "font",
            size = ft_sz,
            angle = 3) +
  geom_text(data = df_odd,
            aes(y = prognose, label = "-"),
            family = "font",
            size = ft_sz,
            angle = 3) +
  geom_text(aes(x = 1980, y = 25, label = "million"),
            family = "font",
            size = 15,
            angle = 359) +
  geom_text(aes(x = 2065, y = 19.4, label = "C.I. 67%"),
            family = "font",
            size = 12,
            angle = 4) +
  geom_text(aes(x = 2064, y = 18.1, label = "C.I. 95%"),
            family = "font",
            size = 12,
            angle = 359) +
  ylim(c(0,NA)) +
  xlim(c(1980, 2070)) +
  labs(
    title = "Dutch population expected to grow\nfrom 17.6 million in 2022 to 20.8 million in 2070",
    subtitle = "Actual population: \"x-x-x\" and projected population: \"+-+-+\"",
    caption = "Data: Bureau voor de Statistiek",
    x = NULL,
    y = "population"
       ) +
  theme_minimal() +
  theme(
    text = element_text(family = "font",
                         size = 50),
    panel.grid = element_line(linetype = "dashed"),
    axis.title.y = element_text(vjust = 3),
    plot.title.position = "plot",
    plot.title = element_text(size = 90,
                              hjust = 0.5,
                              lineheight = 0.3),
    plot.subtitle = element_text(size = 55,
                                 margin = margin(t = 15, b = 15)),
    plot.caption = element_text(size = 40,
                                vjust = -7),
    plot.margin = margin(20,20,20,20, "mm")
  )

## Saving ----
ggsave("./2023/29_monochrome.png",
       bg = background,
       height = 9, width = 12)

system("open ./2023/29_monochrome.png")

