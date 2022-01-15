library(tidyverse)
library(hexSticker)
library(showtext)
library(broom)

font_add_google("Major Mono Display")

alpha <- c(1,1.5,2,2.5)
beta <- 1
beta <- rev(alpha)

df_tbl <- tibble(
    dist    = "Beta",
    alpha   = alpha,
    beta    = beta,
    dist_type = paste0(dist,": ", "c(", alpha, ", ", beta, ")")
) %>%
    group_by(dist_type) %>%
    mutate(
        density(rnorm(500000, alpha, beta)) %>%
            tidy() %>%
            nest(data = c(x, y))
    ) %>%
    ungroup() %>%
    unnest(cols = data)

p <- df_tbl %>%
ggplot(aes(x = x, y = y, group = dist_type, color = dist_type)) +
    geom_line() +
    theme_void() +
    theme_transparent() +
    theme(legend.position = "none")

sticker(
    p,
    package = "{TidyDensity}",
    filename = "C:/Users/Steve/Documents/GitHub/r-package-logos/r_package_logos/TidyDensity/test4.png",
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

