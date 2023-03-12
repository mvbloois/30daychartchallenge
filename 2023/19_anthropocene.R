library(tibble)
library(dplyr)
library(ggplot2)

## https://stackoverflow.com/questions/41391271/plot-an-archimedean-spiral-using-integer-values-with-ggplot2

a <- 2
b <- 3
theta <- seq(0,25*pi,0.01)
r <- a + b*theta
df <- tibble(
         x=r*cos(theta),
         y=r*sin(theta)
         ) %>% # Cartesian coords
  mutate(rn = row_number(),
         age = case_when(rn <= 42 ~ "Meghalayan",
                         rn <= 82 ~ "Northgrippian",
                         rn <= 117 ~ "Greenlandia",
                         rn <= 1290 ~ "Upper Taratian",
                         rn <= 7740 ~ "Chibanian",
                         TRUE ~ "older"))

ggplot(df) +
  geom_path(aes(x, y, group = 1, colour = age),
            linewidth = 1) +
  ggthemes::scale_color_tableau() +
  theme_minimal()


df
