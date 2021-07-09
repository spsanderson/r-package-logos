library(tidyverse)
library(healthyR.ts)
library(hexSticker)
library(showtext)

font_add_google("Major Mono Display")
font_add_google("Amatic SC")
font_add_google("IBM Plex Mono")
font_add_google("Nova Mono")

set.seed(123)
df_rw_tbl <- ts_random_walk(
    .sd = .6,
    .num_walks = 25,
    .initial_value = 1,
    .periods = 15
)

df_rw_tbl %>%
    ggplot(mapping = aes(
        x = x,
        y = cum_y,
        color = factor(run),
        group = factor(run)
    )) +
    geom_line(alpha = 0.8) +
    ts_random_walk_ggplot_layers(df_rw_tbl)

p <- df_rw_tbl %>%
    ggplot(
        mapping = aes(
            x = x
            , y = cum_y
            , color = factor(run)
            , group = factor(run)
        )
    ) +
    geom_line(size = 0.1) +
    theme_void() +
    theme_transparent() +
    theme(legend.position = "none")

sticker(
    p,
    package = "{healthyR.ts}",
    filename = "C:/Users/Steve/Desktop/healthyR logos/healthyr_ts/test1.png",
    p_size = 13,
    s_x = 1,
    s_y = 1,
    s_width = 1.75,
    #s_height = 1.2,
    p_family = "Major Mono Display",
    h_fill = "white",
    h_color = "steel blue",
    h_size = 0.8,
    p_color = "steel blue"
)
