# Genuary 2025 Day 3: Exactly 42 lines of code. Matrix raining code style
pacman::p_load(tidyverse, gganimate, ggfx, extrafont)
loadfonts()
set.seed(2025); rows <- 42; cols <- 89; frames <- 300; trail_length <- 30
chars <- c("\u30A2", "\u30AB", "\u30B5", "\u30BF", "\u30CA", "\u30CF", "\u30DE",
  "\u30E4", "\u30E3", "\u30C0", "\u30D0", "\u30D1", "\u30A4", "\u30A3", "\u30AD", 
  LETTERS, 0:9, "@", "#", "$", "%", "&", "*", "+")
columns_data <- tibble(x = 1:(cols), active = runif(cols) < 1,
  start_frame = sample(1:(frames - trail_length), cols, replace = TRUE))
data <- columns_data %>% filter(active) %>% rowwise() %>%
  mutate(frame = list(start_frame:(start_frame + frames)), 
         trail = list(0:(trail_length - 1))) %>%
  unnest_longer(frame) %>% unnest_longer(trail) %>% group_by(x, frame) %>% 
  mutate(y = rows - (frame - start_frame - trail), 
         char = sample(chars, n(), replace = TRUE),
         brightness = ifelse(trail == 0, 1, 0.9 * (1 - (trail / trail_length))),
         color = ifelse(trail == 0, "white", "#00FF00")) %>%
  filter(y > 0, y <= rows, frame <= frames) %>% ungroup() %>%
  group_by(x, frame) %>% filter(n() > 1) %>% ungroup()
code_lines <- readLines("Genuary3.R")
code_lines <- sprintf("%02d %s", seq_along(code_lines), code_lines)
code_df <- tibble(line = rep(seq_along(code_lines), each = max(nchar(code_lines))),
  char = str_pad(code_lines, max(nchar(code_lines)), side = "right") %>%
    strsplit("") %>% unlist(), 
  x = rep(1:max(nchar(code_lines)), times = length(code_lines)),
  y = 42 - rep(seq_along(code_lines), each = max(nchar(code_lines))) + 1)
combined_data <- data %>% left_join(code_df, by = c("x", "y")) %>%
  mutate(char = ifelse(char.y != " ", char.y, char.x))
p <- ggplot() +
  geom_text(data = code_df, aes(x = x, y = y, label = char), color = "#006600",
    size = 8, family = "Consolas", fontface = "bold") +
  with_outer_glow(geom_text(data = combined_data, 
                            aes(x = x, y = y, label = char, 
                                color = color, alpha = brightness), size = 8,
                            family = "Consolas", fontface = "bold"),
                            colour = "#00FF99", sigma = 2, expand = 1) +
  scale_color_identity() + scale_alpha_identity() + coord_cartesian() + theme_void() +
  theme(panel.background = element_rect(fill = "#0D0208", color = "#0D0208"),
    plot.background = element_rect(fill = "#0D0208", color = "#0D0208")) +
  transition_states(frame, transition_length = 2, state_length = 1)
animate(p, nframes = frames, fps = 5, width = 1200, height = 1200,
  renderer = gifski_renderer("Genuary3.gif"))
