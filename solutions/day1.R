library(tidyverse)

# Day 1, a)
input <- read.delim(
  "data/input01.txt",
  sep = "\n",
  header = FALSE,
  col.names = "depths"
)

head(input)

# Goal is to count number of times the depth increases
# This matches the lag function rather well
output_a <- input %>%
  mutate(increases = depths > lag(depths)) %>%
  filter(increases) %>%
  nrow()

# Day 1, b)
# Same input, now its 3 number sums to check for increases
# Start with day 1+2+3, end when out of 3 number sums
  # This one is then the lead function
output_b <- input %>%
  mutate(three_number_sum = depths + lead(depths, n = 1) + lead(depths, n = 2),
         increases = three_number_sum > lag(three_number_sum)) %>%
  filter(increases) %>%
  nrow()
