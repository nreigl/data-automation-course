library(quarto)
library(here)
library(knitr)

quarto_render(
  input = here("teaching_materials", "fetching.qmd"),
  output_format = "pdf"
)

quarto_render(
  input = here("teaching_materials", "fetching.qmd"),
  output_format = "docx"
)

