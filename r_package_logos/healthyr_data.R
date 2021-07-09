library(tidyverse)
library(hexSticker)
library(showtext)

font_add_google("VT323")
font_add_google("Amatic SC")
font_add_google("IBM Plex Mono")
font_add_google("Nova Mono")


data_tbl <- data.frame(t = seq(0, 2 * pi, by = 0.01))
xhrt <- function(t) 16 * sin(t) ^ 3
yhrt <- function(t) 13 * cos(t) - 5 * cos(2 * t) - 2 * cos(3 * t) - cos(4 * t)
data_tbl$y <- yhrt(data_tbl$t)
data_tbl$x <- xhrt(data_tbl$t)

dt_cross_tbl <- tribble(
    ~ x, ~ y,
    -2, -4,
    -4, -4,
    -4,  0,
    -2,  0,
    -2,  2,
    2,  2,
    2,  0,
    4,  0,
    4, -4,
    2, -4,
    2, -6,
    -2, -6,
    -2, -4
)

p <- data_tbl %>%
    ggplot(mapping = aes(x = x
                         , y = y)) +
    geom_path(color = "red",size = 0.2) +
    geom_path(data = dt_cross_tbl,
              mapping = aes(
                  x = x
                  , y = y
              )
              , color = "red"
              , size = 0.2) +
    theme_void() +
    theme_transparent() +
    theme(legend.position = "none")

sticker(
    p,
    package = "{healthyR}",
    filename = "C:/Users/Steve/Desktop/healthyR logos/healthyr/test7.png",
    p_size = 13,
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
