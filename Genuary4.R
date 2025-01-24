# Genuary 2025 Day 4: Black on black.
# "Pulse of Darkness" 

library(tidyverse)
library(gganimate)

# Parameters for the animation
num_circles <- 60  # Number of circles
max_radius <- 120  # Maximum radius of the circles
min_radius <- 1    # Minimum radius of the circles
frames <- 100      # Number of frames in the animation

# Set random seed for reproducibility
set.seed(2025)

# Create initial data frame for circles
circles <- data.frame(
  id = rep(1:num_circles, each = frames),  
  frame = rep(1:frames, times = num_circles),  
  x = rep(runif(num_circles), each = frames),  
  y = rep(runif(num_circles), each = frames),  
  radius = rep(min_radius, num_circles * frames)  
)

# Update radius of circles over frames to create pulsing effect
circles <- circles %>%
  group_by(id) %>%
  mutate(radius = min_radius + (max_radius - min_radius) * abs(sin(2 * pi * (frame - 1) / frames))) %>%
  ungroup()

p <- ggplot(circles, aes(x = x, y = y, group = id)) +
  geom_point(
    aes(size = radius),
    shape = 21,  
    color = "#1E1E1E",  # Border color
    fill = "black",  # Fill color
    stroke = 2,  
    alpha = 0.8  
  ) +
  scale_size_identity() +  
  theme_void() +  
  theme(
    panel.background = element_rect(fill = "black"),  
    plot.background = element_rect(fill = "black")  
  ) +
  transition_time(frame) +  
  ease_aes("linear")  

# Render the animation and save as a GIF
animate(
  p,
  nframes = frames,
  fps = 10,  # Frames per second
  width = 800, 
  height = 800,  
  renderer = gifski_renderer("Genuary4.gif")
  )