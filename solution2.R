library(tidyverse)
library(magrittr)

input <- tribble(
  ~cmd,      ~axis,         ~op,
  "forward",  "horizontal",  add,
  "down",     "depth",       add,
  "up",       "depth",       subtract
)

reg <- str_c(input$cmd, collapse = "|")

parse_cmd <- function(cmd_text) {
  tibble(
    cmd = str_extract(cmd_text, reg),
    amt = str_remove(cmd_text, reg) %>% as.numeric()
  ) 
}

data <- read_delim("2.txt", delim = "\n", col_names = FALSE) %>%
  setNames("cmd_text") %>%
  mutate(cmd_df = map(cmd_text, parse_cmd)) %>% unnest(cmd_df) %>%
  left_join(input, by = "cmd") %>%
  mutate(l = select(., cmd, amt, axis, op) %>% pmap(lst))

apply_cmd <- function(pos, l) {
  # apply changes to position pos
  pos[l$axis] <- l$op(pos[l$axis], l$amt)
  pos["prod"] <- pos["horizontal"] * pos["depth"]
  return(pos)
}

init_pos = c("horizontal" = 0, "depth" = 0, "prod" = 0)
with(data, reduce(l, apply_cmd, .init = init_pos)) %>% print()

# pt 2

apply_cmd2 <- function(pos, l) {
  # apply command but with aim
  if (l$cmd == "down") {
    pos["aim"]   %<>% add(l$amt)
  } else if (l$cmd == "up") {
    pos["aim"]   %<>% subtract(l$amt)
  } else if (l$cmd == "forward") {
    pos["horizontal"] %<>% add(l$amt)
    pos["depth"] %<>% add(l$amt * pos["aim"])
  } else {
    stop("invalid command")
  }
  pos["prod"] <- pos["horizontal"] * pos["depth"]
  return(pos)
}

init_pos2 = c("horizontal" = 0, "depth" = 0, "aim" = 0, "prod" = 0)
with(data, reduce(l, apply_cmd2, .init = init_pos2)) %>% print()


