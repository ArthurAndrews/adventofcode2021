library(tidyverse)
library(ggfx)

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
  line <- tibble(x = seq(x1, x2), y = seq(y1, y2))
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
  mutate(
    hor_ver = x1 == x2 | y1 == y2,
    line = select(., x1:y2) %>% pmap(make_vent_line)
  )

# unnest the lines
ventlines <- vent %>%
  select(id, code, hor_ver, line) %>%
  unnest(line)

# part 1

## calculate overlaps and print
overlaps1 <- ventlines %>%
  filter(hor_ver) %>%
  group_by(x, y) %>% summarize(ct = n(), .groups = "drop") %>%
  filter(ct > 1)
print(overlaps1)

# part 2

## calculate overlaps and print
overlaps2 <- ventlines %>%
  group_by(x, y) %>% summarize(ct = n(), .groups = "drop") %>%
  filter(ct > 1)
print(overlaps2)

# plot

overlaps <- ventlines %>%
  group_by(x, y) %>% summarize(ct = n(), .groups = "drop")

full_grid <- 
  expand_grid(
    x = seq(1, max(overlaps$x)), 
    y = seq(1, max(overlaps$x))
  ) %>%
  left_join(overlaps, by = c("x", "y")) %>%
  mutate(ct = ct %>% replace_na(0))

small <- 
  expand_grid(
    x = seq(1, 200), 
    y = seq(1, 200)
  ) %>%
  left_join(overlaps, by = c("x", "y")) %>%
  mutate(ct = ct %>% replace_na(0))


plot_with_glow <- function(glow_size = 0, data, glow_sigma = 5) {
  # plot vents with glowing intersections
  
  g <- data %>%
    ggplot() + 
    geom_tile(aes(x, y, fill = ct)) +
    theme(
      legend.position = "none",
      axis.text.x  = element_blank(), 
      axis.ticks.x = element_blank(),
      axis.text.y  = element_blank(),  
      axis.ticks.y = element_blank(),
      rect = element_blank(),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()
    ) + 
    xlab(NULL) + ylab(NULL)
  
  if (glow_size > 0) {
    g <- g +
    with_outer_glow(
      geom_point(
        aes(x, y), 
        color = "white",
        size = 0.5,
        alpha = 0.3,
        shape = "square",
        na.rm = TRUE,
        data = data %>% 
          mutate(glow = if_else(ct > 1, ct, NA_real_)) %>%
          drop_na(glow)
      ),
      colour = "pink",
      sigma = glow_sigma,
      expand = glow_size
    ) 
  }
  
  return(g)
}

plots <- seq(1:10) %>% setNames(as.character(.) %>% str_c(".png"))

map(plots, plot_with_glow, data = full_grid) %>% 
  imap(~ggsave(.y, .x, dev = "png", dpi = 450))

gifski::gifski(
  c(names(plots), rev(names(plots))),
  delay = 0.10, 
  gif_file = "animation.gif"
)
utils::browseURL("animation.gif")
