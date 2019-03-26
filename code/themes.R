theme_fhi_basic <- function(base_size = 12,
                            base_family = "",
                            base_line_size = base_size / 22,
                            base_rect_size = base_size / 22) {
  half_line <- base_size / 2
  
  # sysfonts::font_add_google("IBM Plex Sans")
  # sysfonts::font_add_google("MS Comic Sans")
  
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      axis.text = element_text(colour = "black", size = rel(0.8)),
      axis.ticks = element_line(colour = "black", size = rel(0.5)),
      axis.ticks.length = unit(rel(.25), "cm"),
      axis.title.x = element_text(margin = margin(t = base_size), vjust = 1),
      axis.title.y = element_text(angle = 90, margin = margin(r = base_size), vjust = 1),
      axis.line = element_line(colour = "black", size = rel(1)),
      panel.border = element_rect(
        fill = NA,
        colour = NA,
        size = rel(1)
      ),
      panel.grid = element_blank(),
      complete = TRUE
    )
}

theme_fhi_lines <- function(base_size = 12,
                            base_family = "",
                            base_line_size = base_size / 22,
                            base_rect_size = base_size / 22) {
  theme_fhi_basic(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      panel.grid = element_line(colour = "black"),
      panel.grid.major = element_line(size = rel(0.1)),
      panel.grid.minor = element_line(size = rel(0.05)),
      complete = TRUE
    )
}
