# Genuary 2025 Day 1: Vertical or horizontal lines only
# This script generates a plot with random vertical and horizontal
# lines with varying colors and thicknesses.

library(ggplot2)
library(dplyr)

# Create a data frame for vertical and horizontal lines
set.seed(2025) # Set seed for reproducibility
data <- data.frame(
  x = runif(100, 0, 10), # Random x-coordinates
  y = runif(100, 0, 10), # Random y-coordinates
  type = sample(
    c("vertical", "horizontal"), # Line types
    100,
    replace = TRUE
  )
)

# Define a color palette for the lines
colors_palette <- c("#f0a202", "#f18805", "#d95d39", "#202c59", "#581f18")

# Add colors and line thickness to the data
data <- data %>%
  mutate(
    color = sample(colors_palette, n(), replace = TRUE), # Randomly assign colors
    size = runif(n(), 0.1, 3.75) # Randomly assign line thickness
  )

# Plot the lines using ggplot2
ggplot(data, aes(x = x, y = y)) +
  geom_vline(
    data = data %>% filter(type == "vertical"),
    aes(xintercept = x, color = color, size = size), alpha = runif(1)
  ) +
  geom_hline(
    data = data %>% filter(type == "horizontal"),
    aes(yintercept = y, color = color, size = size), alpha = runif(1)
  ) +
  scale_color_identity() +
  scale_size_identity() +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Save the plot as a PNG file
ggsave("Genuary1.png", dpi = 300, width = 5, height = 5, bg = "#e6e4ce")
