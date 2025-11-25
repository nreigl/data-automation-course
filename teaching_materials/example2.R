library(quarto)     # for compiling Quarto presentations and documents
library(pxweb)      # query and fetch data from Statistics Estonia
library(tidyverse)  # dplyr, ggplot2, readr, etc.
library(lubridate)  # for date handling (yq function)
library(here)       # path settings

# pxweb_interactive() # All available databases
# pxweb_interactive("https://andmed.stat.ee/api/v1/en/stat") # Statistics Estonia

pa111_meta <- pxweb_get(
  url = "https://andmed.stat.ee/api/v1/en/stat/PA111")
pa111_meta

pa111_meta$variables[[1]]

px_query_list1 <- list(
  "Näitaja" = c("GR_W_D1", "GR_W_D5", "GR_W_D9"),
  "Tegevusala" = "*",
  "Vaatlusperiood" = "*"
)

pa111 <- pxweb_get(
  url = "https://andmed.stat.ee/api/v1/en/stat/PA111",
  query = px_query_list1
)

pa111
class(pa111)

# Convert to data frame
pa111.df <- as.data.frame(pa111, column.name.type = "text", variable.value.type = "text")
pa111.code <- as.data.frame(pa111, column.name.type = "code", variable.value.type = "code")
# Optional: Combine text and code in one data frame
pa111.df$Näitaja <- pa111.code$Näitaja

head(pa111.df, n=3)

selected_sectors <- c("Manufacturing", "Construction", "Information and communication", "Real estate activities", "Total - all activities")
# Clean and reshape
pa111_plotdata <- pa111.df |>
  mutate(
    quarter = yq(str_replace(`Reference period`, " ", "-")),
    Indicator = recode(
      Näitaja,
      "GR_W_D1" = "D1",
      "GR_W_D5" = "Median",
      "GR_W_D9" = "D9"
    )
  ) |>
  filter(
    `Economic activity` %in% selected_sectors,
    Indicator %in% c("D1", "Median", "D9")
  ) |>
  select(quarter, `Economic activity`, Indicator, value = `PA111: AVERAGE MONTHLY GROSS WAGES (SALARIES), MEDIAN, DECILES AND NUMBER OF EMPLOYEES`) |>
  pivot_wider(names_from = Indicator, values_from = value)

overall_median <- pa111_plotdata |>
  filter(`Economic activity` == "Total - all activities") |>
  select(quarter, total_median = Median)

pa111_plotdata <- pa111_plotdata |>
  filter(`Economic activity` != "Total - all activities") |>
  left_join(overall_median, by = "quarter")

meta <- pa111$metadata[[1]]
title <- meta$label
source <- meta$source

source_note <- glue::glue(
  "Source: {source}\n\"{title}\""
)

ggplot(pa111_plotdata, aes(x = quarter)) +
  geom_ribbon(aes(ymin = D1, ymax = D9), fill = "skyblue", alpha = 0.5) +
  geom_line(aes(y = Median), color = "darkblue", linewidth = 1) +
  geom_line(aes(y = total_median), color = "red", linetype = "dashed", linewidth = 0.7) +
  facet_wrap(~`Economic activity`, ncol = 2) +
  labs(
    title = "Wage Distribution by Sector (1st to 9th Decile, Median)",
    subtitle = "Red dashed line shows overall median (Total - all activities)",
     caption = source_note,
    x = "",
    y = "Gross Nominal Monthly Wages (EUR)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# quarto::quarto_render(here("teaching_materials", "example2_statistics_estonia.qmd"), output_format = "all")
