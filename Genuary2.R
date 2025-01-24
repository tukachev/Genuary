# Genuary 2025 Day 2: Layers upon layers upon layers.
# Fractal layers of life

library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)

# Generate synthetic data for fractal layers of life
set.seed(2025) # Set seed for reproducibility

# Number of layers
num_layers <- 10

# Create data for each layer
data <- tibble(layer = 1:num_layers) %>%
  mutate(
    # Random number of points for each layer
    num_points = map_int(layer, ~ sample(50:100, 1)),
    # Angles for points in each layer
    angle = map(num_points, ~ seq(0, 2 * pi, length.out = .x)),
    # Radius with some noise
    radius = map2(layer, num_points, ~ .x + runif(.y, -0.45, 0.45))
  ) %>%
  unnest(c(angle, radius)) %>%
  mutate(
    # Calculate x and y coordinates
    x = radius * cos(angle),
    y = radius * sin(angle),
    # Alpha (transparency) for each layer
    alpha = 1 / (layer + 1),
    layer = factor(layer),
    # Color for each layer
    color = layer
  )

ggplot(data, aes(x, y)) +
  geom_polygon(aes(group = layer, fill = layer, alpha = alpha), color = "#581f18", linewidth = 0.3) +
  scale_fill_viridis_d(option = "plasma") +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none")

# Save the plot as a PNG file
ggsave("Genuary2.png", dpi = 300, width = 5, height = 5, bg = "black")
