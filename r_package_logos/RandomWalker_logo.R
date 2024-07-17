# Load required libraries
library(tidyverse)
library(hexSticker)
library(showtext)

font_add_google("VT323")
font_add_google("Amatic SC")
font_add_google("IBM Plex Mono")
font_add_google("Nova Mono")
font_add_google("Major Mono Display")

# Version 1
# Function to generate a 2D random walk
generate_random_walk <- function(n_steps = 100) {
    # Generate random steps
    x_steps <- sample(c(-1, 1), n_steps, replace = TRUE)
    y_steps <- sample(c(-1, 1), n_steps, replace = TRUE)
    
    # Calculate positions
    x_positions <- cumsum(x_steps)
    y_positions <- cumsum(y_steps)
    
    # Create a data frame
    walk_df <- data.frame(
        Step = 1:n_steps,
        X = x_positions,
        Y = y_positions
    )
    
    return(walk_df)
}

# Generate a random walk
random_walk_df <- generate_random_walk(n_steps = 100)

# Plot the random walk
p1 <- ggplot(random_walk_df, aes(x = X, y = Y)) +
    geom_path(color = "blue", alpha = 0.7) +
    #geom_point(color = "red", size = 0.1) +
    theme_void() +
    labs(
        title = "",
        x = "",
        y = ""
    ) +
    theme(axis.text.x=element_blank(), #remove x axis labels
          axis.ticks.x=element_blank(), #remove x axis ticks
          axis.text.y=element_blank(),  #remove y axis labels
          axis.ticks.y=element_blank()  #remove y axis ticks
    )

sticker(
    p,
    package = "{RandomWalker}",
    filename = "C:/Users/steve/Documents/GitHub/r-package-logos/r_package_logos/RandomWalker/logo.png",
    p_size = 13,
    s_x = 1,
    s_y = .9,
    s_width = 1.1,
    s_height = 1.1,
    p_family = "Major Mono Display",
    h_fill = "white",
    h_color = "steel blue",
    h_size = 1,
    p_color = "steel blue"
)

# Version 2

# Function to simulate a 2D random walk
random_walk <- function(steps = 1000, step_size = 1) {
    x <- cumsum(runif(steps, -step_size, step_size))
    y <- cumsum(runif(steps, -step_size, step_size))
    data.frame(step = 1:steps, x = x, y = y)
}

# Generate random walk data
walk_data <- random_walk(steps = 100, step_size = 0.5)

# Plot the random walk
p <- ggplot(walk_data, aes(x = x, y = y)) +
    geom_path(linewidth = 0.25, color = "steelblue", alpha = .618) +
    geom_point(data = walk_data[c(1, max(walk_data[["step"]])), ], size = 0.5) +
    labs(title = "",
         x = "",
         y = "") +
    theme_void() +
    theme(legend.position = "none") +
    theme(axis.text.x=element_blank(), #remove x axis labels
          axis.ticks.x=element_blank(), #remove x axis ticks
          axis.text.y=element_blank(),  #remove y axis labels
          axis.ticks.y=element_blank()  #remove y axis ticks
    )

sticker(
    p,
    package = "{RandomWalker}",
    filename = "C:/Users/steve/Documents/GitHub/r-package-logos/r_package_logos/RandomWalker/logo.png",
    p_size = 13,
    s_x = 1,
    s_y = .8,
    s_width = 1,
    s_height = 1,
    p_family = "Major Mono Display",
    h_fill = "white",
    h_color = "steel blue",
    h_size = 1,
    p_color = "steel blue"
)

# Version 3
# Set random seed for reproducibility
set.seed(123)

# Number of steps
n <- 1000

# Generate random walk
random_walk <- data.frame(
    x = cumsum(rnorm(n)),
    y = cumsum(rnorm(n)),
    step = 1:n
)

# Create the plot
p3 <- ggplot(random_walk, aes(x = x, y = y)) +
    geom_path(aes(color = step), linewidth = 0.5) +
    #geom_point(data = random_walk[c(1, n), ], aes(color = step), size = 0.1) +
    scale_color_viridis_c(option = "plasma") +
    labs(x = "",
         y = "") +
    theme_void() +
    theme(legend.position = "none") +
    theme(axis.text.x=element_blank(), #remove x axis labels
          axis.ticks.x=element_blank(), #remove x axis ticks
          axis.text.y=element_blank(),  #remove y axis labels
          axis.ticks.y=element_blank()  #remove y axis ticks
    )

sticker(
    p,
    package = "{RandomWalker}",
    filename = "C:/Users/steve/Documents/GitHub/r-package-logos/r_package_logos/RandomWalker/logo.png",
    p_size = 13,
    s_x = 1,
    s_y = .8,
    s_width = 1,
    s_height = 1,
    p_family = "Major Mono Display",
    h_fill = "white",
    h_color = "steel blue",
    h_size = 1,
    p_color = "steel blue"
)

library(patchwork)

ppp <- p1 + p2 + p3 + plot_layout(ncol = 3)

sticker(
    ppp,
    package = "{RandomWalker}",
    filename = "C:/Users/steve/Documents/GitHub/r-package-logos/r_package_logos/RandomWalker/logo.png",
    p_size = 13,
    s_x = 1,
    s_y = .8,
    s_width = 1,
    s_height = 1,
    p_family = "Major Mono Display",
    h_fill = "white",
    h_color = "steel blue",
    h_size = 1,
    p_color = "steel blue"
)
