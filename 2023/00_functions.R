
create_caption <- function(txt_clr = "grey15", bg = "white", source = "datasource") {
  sysfonts::font_add(family = "fa-brands", regular = "./resources/Font Awesome 6 Brands-Regular-400.otf")
  sysfonts::font_add(family = "fa-solid", regular = "./resources/fa-solid-900.ttf")

  table    <- glue::glue("<span style=\"font-family:fa-solid;color:{txt_clr}\">&#xf0ce;</span>")
  mastodon <- glue::glue("<span style=\"font-family:fa-brands;color:{txt_clr}\">&#xf4f6;</span>")
  twitter  <- glue::glue("<span style=\"font-family:fa-brands;color:{txt_clr}\">&#xf099;</span>")
  github   <- as.character(glue::glue("<span style=\"font-family:fa-brands;color:{txt_clr}\">&#xf09b;</span>"))
  
  ## Cannot get the spacing right
  ## https://github.com/doehm/tidytues/blob/main/scripts/2023/week%209%20afrisenti/afrisenti.R
  space  <- glue::glue("<span style='color:{bg}'>.</span>")
  space2 <- glue::glue("<span style='color:{bg}'>--</span>") # can't believe I'm doing this
  
  glue::glue("{table}{space2}{source}<br>{mastodon}{space2}@martijnvanbloois{space}@fosstodon.org{space2}{twitter}{space2}@prancke{space2}{github}{space2}mvbloois")
  
}
