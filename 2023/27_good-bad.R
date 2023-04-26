## https://www.cedricscherer.com/2021/06/06/visualizing-distributions-with-raincloud-plots-and-how-to-create-them-with-ggplot2/

library(tidyverse)
library(ggdist)
library(colorspace)
library(showtext)

font_add_google("Rye", "rye")
showtext_auto()

# download.file("https://datasets.imdbws.com/title.basics.tsv.gz",
#               destfile = "./2023/data/title.basics.tsv.gz")
# download.file("https://datasets.imdbws.com/title.ratings.tsv.gz",
#               destfile = "./2023/data/title.ratings.tsv.gz")

good <- "[Gg]ood |[Gg]ood$|[Gg]ood-|[Gg]ood,"
bad  <- "[Bb]ad |[Bb]ad$|[Bb]ad-|[Bb]ad-,"
ugly <- "[Uu]gly |[Uu]gly$|[Uu]gly-|[Uu]gly,"

titles <- read_delim("./2023/data/title.basics.tsv.gz") %>%
  filter(
    str_detect(primaryTitle, good) |
      str_detect(primaryTitle, bad) |
      str_detect(primaryTitle, ugly)
  )

ratings <- read_delim("./2023/data/title.ratings.tsv.gz")


df <- titles %>%
  inner_join(ratings,
             by = join_by(tconst)) %>%
  mutate(
    type = case_when(
      str_detect(primaryTitle, good) &
        str_detect(primaryTitle, bad) &
        str_detect(primaryTitle, ugly)
      ~ "The Good, The Bad and The Ugly",
      str_detect(primaryTitle, good) ~ "Good",
      str_detect(primaryTitle, bad) ~ "Bad",
      str_detect(primaryTitle, ugly) ~ "Ugly",
      TRUE ~ "xxx"
    )
  ) %>%
  mutate(type = factor(
    type,
    levels = c("Ugly", "Bad", "Good", "The Good, The Bad and The Ugly")
  ))

df %>%
  filter(titleType == "movie") %>%
  filter(type %in% c("Ugly", "Bad", "Good")) %>%
  ggplot(aes(x = type, y = averageRating, fill = type)) +
  stat_dots(side = "left",
            colour = "darkgreen",
            alpha = 0.75) +
  stat_halfeye(
    adjust = .5,
    width = .6,
    justification = -.1,
    .width = c(.5, .95),
    point_colour = "black",
    alpha = 0.75
  ) +
  geom_hline(
    yintercept = 8.8,
    linetype = "dashed",
    linewidth = 1.2,
    colour = "darkred"
  ) +
  geom_text(aes(x = 1, y = 6.95, label = "The Good, the Bad and the Ugly (1966)"),
            family = "rye",
            size = 16,
            vjust = 5,
            colour = "darkred") +
  scale_fill_manual(values = c("#554124", "#23904F", "#3C2515")) +
  coord_flip(xlim = c(1.2, NA),
             ylim = c(NA, 10)) +
  labs(
    title = "Good, Bad Or Ugly?",
    subtitle = "Ratings of movies with either good, bad or ugly in the title",
    caption = "Data: IMDB",
    x = NULL,
    y = NULL
  ) +
  guides(fill = "none") +
  theme_minimal() +
  theme(
    text = element_text(family = "rye",
                        size = 90),
    plot.background = element_rect(fill = "#FFFAFA"),
    plot.title.position = "plot",
    plot.title = element_text(size = 90,
                              hjust = 0.5),
    plot.subtitle = element_text(
      size = 50,
      lineheight = 0.35,
      hjust = 0.5
    ),
    plot.caption = element_text(hjust = 0.5),
    plot.margin = margin(10,10,10,10)
  )


## Saving ----
ggsave(
  "./2023/27_good-bad.png",
  bg = "#FFFAFA",
  height = 9,
  width = 12
)

system("open ./2023/27_good-bad.png")




# fs::file_delete("./2023/data/title.ratings.tsv.gz")
# fs::file_delete("./2023/data/title.basics.tsv.gz")
