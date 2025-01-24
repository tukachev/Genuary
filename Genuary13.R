# Genuary 2025 Day 13: Triangles and nothing else.
library(tidyverse)

# Function to rotate points around a center
rotate_points <- function(center_x, center_y, size, scale, angle) {
  # Convert angle to radians
  angle <- angle * pi / 180
  # Calculate the coordinates of the triangle
  x <- center_x + size * scale * c(0, -0.5, 0.5)
  y <- center_y + size * scale * c(1, -1, -1)
  # Rotate the coordinates
  x_rot <- center_x + (x - center_x) * cos(angle) - (y - center_y) * sin(angle)
  y_rot <- center_y + (x - center_x) * sin(angle) + (y - center_y) * cos(angle)
  # Return the rotated coordinates as a tibble
  tibble(x = x_rot, y = y_rot)
}

set.seed(2025) # Set the random seed for reproducibility

# Create a tibble with the triangle data
n_triangles <- 2025 #number of triangles
triangles <- tibble(
  id = 1:n_triangles,
  center_x = runif(n_triangles, 0, 40), # Random x-coordinates
  center_y = runif(n_triangles, 0, 40), # Random y-coordinates
  size = runif(n_triangles, 0.2, 1), # Random size
  scale = runif(n_triangles, 0.5, 3), # Random scale
  angle = runif(n_triangles, 0, 360) # Random angle
) %>%
  mutate(coords = purrr::pmap(list(center_x, center_y, size, scale, angle), rotate_points)) %>%
  unnest(coords)

ggplot(triangles, 
  aes(x = x, y = y, group = id, fill = id)) +
  geom_polygon(alpha = 0.45,
               color = NA,
               linewidth = 0) +
  scale_fill_viridis_c(option = "plasma") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black")
  )

# Save the plot as a PNG file
ggsave("Genuary13.png", dpi = 300, width = 5, height = 5)