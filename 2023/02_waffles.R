library(tidyverse)
library(waffle)

# https://www.24kitchen.nl/recepten/stroopwafels
# https://liamgilbey.github.io/ggwaffle/index.html

ingredients <-
  tibble(
part        =c(rep("cookie", 4), rep("sirup", 4)),
ingredient = c("flower", "sugar", "butter", "egg", "sugar", "creme", "sirup", "butter"),
grams      = c(240, 95, 80, 60, 500, 82, 75, 225)
) 

ggplot(ingredients, aes(fill = ingredient, values = grams)) +
  geom_waffle(colour = "white", n_rows = 25, flip = TRUE) + 
  facet_wrap(~part, nrow = 1, strip.position = "bottom") +
  coord_equal() +
  theme_void() +
  labs(
    title = "Sirup waffles from Gouda",
    subtitle = "ingredients to make sirup waffles, add pinches of salt and cinnamon powder",
    caption = "Ingredients: 24Kitchen",
    fill = "Ingredients"
  ) +
  theme(
    legend.position = "bottom"
  )
