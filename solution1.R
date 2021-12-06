library(tidyverse)
library(magrittr)

# read file
depth <- read_delim("1.txt", delim = "\n", col_names = FALSE) %>%
  setNames("x") 

# detect increasing depth
depth %<>% mutate(incr = x > lag(x, 1))
count(depth, incr)

# pt 2
# detect increasing rolling mean of depth
depth %<>% mutate(
  roll_x = zoo::rollmean(x, k = 3, align = "left", fill = NA),
  roll_incr = roll_x > lag(roll_x, 1)
)
count(depth, roll_incr)
