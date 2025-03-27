library(here)
library(knitr)

# Extract just the code
purl(
  input = here("teaching_materials", "fetching.qmd"),
  output = here("teaching_materials", "fetching_script.R")
)

# Extract code with markdown as comments
purl(
  input = here("teaching_materials", "fetching.qmd"),
  output = here("teaching_materials", "fetching_annotated.R"),
  documentation = 2
)
