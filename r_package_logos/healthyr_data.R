library(tidyverse)
library(hexSticker)
library(showtext)

font_add_google("Major Mono Display")

data_tbl <- tibble(
    x = 1:5,
    y = c(1,3,2,5,4)
)

p <- data_tbl %>%
    ggplot(mapping = aes(x = x
                         , y = y)) +
    geom_col(fill = "white", color = "red",size = 0.2) +
    theme_void() +
    theme_transparent() +
    theme(legend.position = "none")

sticker(
    p,
    package = "{healthyR.data}",
    filename = "C:/Users/Steve/Documents/GitHub/r-package-logos/r_package_logos/healthyr_data/test4.png",
    p_size = 11,
    s_x = 1,
    s_y = .8,
    s_width = 1.4,
    s_height = 0.5,
    p_family = "Major Mono Display",
    h_fill = "white",
    h_color = "steel blue",
    h_size = 0.8,
    p_color = "steel blue"
)
