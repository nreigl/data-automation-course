# theme_ep.R

library(ggplot2)

# Color scale
scale_color_ep <- function(...) {
  scale_color_manual(
    values = c(
      "#0079BC", # Accent 1
      "#D6E1F2", # Accent 2
      "#FF8C42", # Accent 3
      "#8ACC61", # Accent 4
      "#E85C70", # Accent 5
      "#E091D4" # Accent 6
    ),
    ...
  )
}

# Fill scale
scale_fill_ep <- function(...) {
  scale_fill_manual(
    values = c(
      "#0079BC",
      "#D6E1F2",
      "#FF8C42",
      "#8ACC61",
      "#E85C70",
      "#E091D4"
    ),
    ...
  )
}
# Custom ggplot2 theme for Eesti Pank
theme_ep <- function(base_size = 11, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.background = element_rect(fill = "#FFFFFF", color = NA),
      panel.background = element_rect(fill = "#FFFFFF", color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#858A8C"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "#000000"),
      axis.title = element_text(color = "#000000"),
      plot.title = element_text(
        face = "bold",
        color = "#000000",
        margin = margin(b = 10)
      ),
      plot.caption = element_text(size = 9, color = "#444444", hjust = 0),
      legend.background = element_rect(fill = "#FFFFFF", color = NA),
      legend.key = element_rect(fill = "#FFFFFF", color = NA),
      legend.position = "top",
      legend.title = element_blank(),
      legend.margin = margin(b = 5),
      legend.box.margin = margin(t = 5, b = 5)
    )
}
