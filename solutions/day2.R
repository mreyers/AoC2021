library(tidyverse)

# Submarine must dive, dive, dive
# Day 2, a)
input <- read.delim(
  "data/input02.txt",
  sep = "\n",
  header = FALSE,
  col.names = "directions"
)

# forward x -> + horizontal by x
# down x -> + depth by x
# up x -> - depth by x

# Horizontal and depth coordinates start at 0, i.e. (0, 0)
# Desired output is horizontal * depth
# e.g. (10, 15) = 150 for puzzle submission

# This problem can be separated as there is no need to observe sequentially
# All forward commands can be simply processed
modified_input <- input %>%
  separate(col = directions, into = c("direction", "quantity"), sep = " ", convert = TRUE) %>%
  group_by(direction) %>%
  summarize(total_movement = sum(quantity))

horizontal_output <- modified_input %>%
  filter(direction == "forward") %>%
  pull(total_movement)

depth_output <- modified_input %>%
  filter(direction != "forward") %>%
  arrange(direction) %>%
  summarize(depth = first(total_movement) - last(total_movement)) %>%
  pull(depth)

final_output <- horizontal_output * depth_output


# 2. b)
# Third tracked value called aim
# Still start at (h, d, a) = (0, 0, 0)
# Now down and up increase and decrease aim
# Now forward acts on that aim value
  # Increase horizontal position by x units (so this is unchanged, horizontal is still same total)
  # Increase depth by aim multiplied by x

# Sequencing now matters a bit more though not too intensely
aimed_output <- input %>%
  separate(col = directions, into = c("direction", "quantity"), sep = " ", convert = TRUE) %>%
  mutate(depth = 0,
         aim_mod = case_when(direction == "down" ~ 1.0 * quantity,
                             direction == "up" ~ -1.0 * quantity,
                             TRUE ~ 0),
         aim = cumsum(aim_mod),
         depth_mod = ifelse(direction == "forward", aim * quantity, 0),
         horizontal_mod = ifelse(direction == "forward", quantity, 0)) %>%
  summarize(tot_depth = sum(depth_mod),
            tot_horiz = sum(horizontal_mod)) %>%
  mutate(total_coord = tot_depth * tot_horiz) %>%
  pull(total_coord)
