library(dplyr)
library(ggplot2)

df <- tibble::tibble(
  var1 = c("A", "A", "A", "B", "B"),
  var2 = c("X", "Y", "Z", "Y", "Z"),
  var3 = c(33, 33, 34, 50, 50)
)

df %>% 
  mutate(var1 = as.factor(var1),
         var2 = as.factor(var2)) %>% 
  ggplot(aes(x = as.numeric(var1), y = var3, fill = var2)) +
  geom_col(linewidth = 1, position = position_stack()) +
  xlim(c(-1,3)) +
  coord_polar(theta = "y") +
  labs(title = "part-to-whole") +
  theme_void()

ggsave("./2023/part-to-whole.png")

