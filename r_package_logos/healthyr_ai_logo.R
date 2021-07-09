library(tidyverse)
library(healthyR.ts)
library(hexSticker)
library(showtext)

font_add_google("Major Mono Display")
font_add_google("Amatic SC")
font_add_google("IBM Plex Mono")
font_add_google("Nova Mono")


data_tbl <- data.frame(t = seq(0, 2 * pi, by = 0.01))
xhrt <- function(t) 16 * sin(t) ^ 3
yhrt <- function(t) 13 * cos(t) - 5 * cos(2 * t) - 2 * cos(3 * t) - cos(4 * t)
data_tbl$y <- yhrt(data_tbl$t)
data_tbl$x <- xhrt(data_tbl$t)

set.seed(123)
df_rw_tbl <- ts_random_walk(
    .sd = .6,
    .num_walks = 10,
    .initial_value = 1,
    .periods = 15
)

df_rw_tbl$x <- df_rw_tbl$x - 1
df_rw_tbl$x2 <- df_rw_tbl$x * -1
df_rw_tbl %>%
    ggplot(mapping = aes(
        x = x,
        y = cum_y,
        color = factor(run),
        group = factor(run)
    )) +
    geom_line(alpha = 0.8) +
    ts_random_walk_ggplot_layers(df_rw_tbl)

p <- data_tbl %>%
    ggplot(mapping = aes(x = x
                         , y = y)) +
    geom_path(color = "red",size = 0.2) +
    geom_line(data = df_rw_tbl,
              mapping = aes(
                  x = x,
                  y = cum_y,
                  color = factor(run),
                  group = factor(run)
              )
              , size = 0.1) +
    geom_line(data = df_rw_tbl,
              mapping = aes(
                  x = x2,
                  y = cum_y,
                  color = factor(run),
                  group = factor(run)
              )
              , size = 0.1) +
    theme_void() +
    theme_transparent() +
    theme(legend.position = "none")

sticker(
    p,
    package = "{healthyR.ai}",
    filename = "C:/Users/Steve/Desktop/healthyR logos/healthyr_ai/test1.png",
    p_size = 13,
    s_x = 1,
    s_y = .6,
    s_width = 1.4,
    s_height = 1.2,
    p_family = "Amatic SC",
    h_fill = "white",
    h_color = "steel blue",
    h_size = 0.8,
    p_color = "steel blue"
)

imgurl <- ("C:/Users/Steve/Desktop/healthyR logos/hypercube.png")
sticker(
    imgurl,
    package = "{healthyR.ai}",
    filename = "C:/Users/Steve/Desktop/healthyR logos/healthyr_ai/test3.png",
    p_size = 13,
    s_x = 1,
    s_y = .9,
    s_width = .6,
    s_height = 1.2,
    p_family = "Major Mono Display",
    h_fill = "white",
    h_color = "steel blue",
    h_size = 0.8,
    p_color = "steel blue"
)
