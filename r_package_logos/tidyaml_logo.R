library(tidyverse)
library(hexSticker)
library(showtext)

font_add_google("VT323")
font_add_google("Amatic SC")
font_add_google("IBM Plex Mono")
font_add_google("Nova Mono")
font_add_google("Major Mono Display")

# Generate some sample data
set.seed(123)
df <- tibble(
    x = runif(100, 0, 10),
    y = rbinom(100, 1, 1 / (1 + exp(-1 * (0.5 * x - 2.5))))
)

# Fit a logistic regression model to the data
model <- glm(y ~ x, data = df, family = "binomial")

# Create a new data frame with a range of values for x
df.new <- data.frame(x = seq(0, 10, 0.01))

# Compute the predicted values of y for each value of x
df.new$y.pred <- predict(model, newdata = df.new, type = "response")

# Plot the original data and the fitted curve
p <- ggplot(df, aes(x, y)) +
    geom_point(size = 0.5, alpha = 0.3) +
    geom_line(data = df.new, aes(x, y.pred), color = "steelblue", 
              linewidth = 0.5, alpha = 0.5) +
    theme_void() +
    theme_transparent() +
    theme(legend.position = "none")

sticker(
    p,
    package = "{tidyaml}",
    filename = "C:/Users/steve/Documents/GitHub/r-package-logos/r_package_logos/tidyaml/logo.png",
    p_size = 13,
    s_x = 1,
    s_y = .8,
    s_width = 1,
    s_height = 0.8,
    p_family = "Major Mono Display",
    h_fill = "white",
    h_color = "steel blue",
    h_size = 0.8,
    p_color = "steel blue"
)
