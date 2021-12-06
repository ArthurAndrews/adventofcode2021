library(tidyverse)

decode_text <- function(code) {
  # decode text "x1,y1 -> x2,y2" into a one-row data frame
  coord <- code %>% 
    str_replace("->", ",") %>%
    str_split(",") %>% unlist() %>%
    as.numeric() %>%
    setNames(c("x1", "y1", "x2", "y2")) %>%
    as_tibble_row()
  return(coord)
}

make_vent_line <- function(x1, y1, x2, y2) {
  # make data frame of vent line points 
  if (x1 == x2) {
    line <- tibble(x = x1, y = seq(y1, y2))
  } else if (y1 == y2) {
    line <- tibble(x = seq(x1, x2), y = y1)
  } else {
    stop("invalid line")
  }
  return(line)
}

# read file
vent0 <- read_delim("5.txt", delim = "\n", col_names = FALSE) %>%
  setNames("code") %>%
  mutate(id = 1:n()) %>% relocate(id)

# add coordinates and vent lines to vent data frame
vent <- vent0 %>%
  mutate(coord = code %>% map(decode_text)) %>%
  unnest(coord) %>%
  filter(x1 == x2 | y1 == y2) %>%
  mutate(line = select(., x1:y2) %>% pmap(make_vent_line))

# unnest the lines
ventlines <- vent %>%
  select(id, code, line) %>%
  unnest(line)

# calculate overlaps and print
overlaps <- ventlines %>%
  group_by(x, y) %>% summarize(ct = n(), .groups = "drop") %>%
  filter(ct > 1)
print(overlaps)

# part 2
make_vent_line2 <- function(x1, y1, x2, y2) {
  # make data frame of vent line points 
  line <- tibble(x = seq(x1, x2), y = seq(y1, y2))
  return(line)
}

vent <- vent0 %>%
  mutate(coord = code %>% map(decode_text)) %>%
  unnest(coord) %>%
  mutate(line = select(., x1:y2) %>% pmap(make_vent_line2))

# unnest the lines
ventlines <- vent %>%
  select(id, code, line) %>%
  unnest(line)

# calculate overlaps and print
overlaps <- ventlines %>%
  group_by(x, y) %>% summarize(ct = n(), .groups = "drop") %>%
  filter(ct > 1)
print(overlaps)
