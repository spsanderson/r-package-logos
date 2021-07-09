library(tidyverse)
library(hexSticker)
library(showtext)

font_add_google("Major Mono Display")

data_tbl <- data.frame(t = seq(0, 2 * pi, by = 0.01))
xhrt <- function(t) 16 * sin(t) ^ 3
yhrt <- function(t) 13 * cos(t) - 5 * cos(2 * t) - 2 * cos(3 * t) - cos(4 * t)
data_tbl$y <- yhrt(data_tbl$t)
data_tbl$x <- xhrt(data_tbl$t)

p <- data_tbl %>%
    ggplot(mapping = aes(x = x
                         , y = y)) +
    geom_path(color = "red",size = 0.2) +
    theme_void() +
    theme_transparent() +
    theme(legend.position = "none")

sticker(
    p,
    package = "{healthyR.data}",
    filename = "C:/Users/Steve/Documents/GitHub/r-package-logos/r_package_logos/healthyr_data/test1.png",
    p_size = 11,
    s_x = 1,
    s_y = .7,
    s_width = 1.4,
    s_height = 1.2,
    p_family = "Major Mono Display",
    h_fill = "white",
    h_color = "steel blue",
    h_size = 0.8,
    p_color = "steel blue"
)
