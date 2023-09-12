library(tidyverse)
library(ggimage)
library(magick)

image_dir <- "quality_review_v3"

collage_df <- tibble(
  class = list.dirs(image_dir, full.names = FALSE, recursive = FALSE)
) %>% 
  mutate(
    image = map(class, ~ list.files(paste0(image_dir, "/", .x), full.names = TRUE, recursive = TRUE))
  ) %>% 
  unnest(image) %>% 
  group_by(class) %>% 
  slice_sample(n = 12) %>% 
  ungroup

collage_col <- 4
collage_row <- 3

collage_df %>% 
  # filter(class %in% unique(class)[1:2]) %>% 
  group_by(class) %>%
  # slice_sample(n = 2) %>% 
  mutate(
    image = map_chr(image, function(x) {
      new_path <- tempfile(fileext = str_extract(x, "\\.\\w+$"))
      image_read(x) %>% 
        image_scale(geometry = "400x400!") %>% 
        image_write(new_path)
      return(new_path)
    })
  ) %>% 
  mutate(
    # x = rep(1:ceiling(n()/4), length.out = n()),
    # y = rep(1:ceiling((n() + 1)/3), each = 4, length.out = n())
    x = rep(1:collage_col, length.out = n()),
    y = rep(1:collage_row, each = collage_col, length.out = n())
  ) %>% 
  ggplot(aes(x,y, image = image)) +
  geom_image(by = "width", size = 1/3) +
  facet_wrap(~class, ncol = 2) +
  scale_x_continuous(expand = expansion(0,.5)) +
  scale_y_continuous(expand = expansion(0,.5)) +
  scale_size_identity() +
  coord_equal() +
  theme_void() +
  theme(strip.text = element_text(size = 20,
                                  margin = margin(3,0,3,0)),
        strip.background = element_rect(fill = "gray90"),
        legend.position = "none")
