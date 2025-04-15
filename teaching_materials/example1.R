# install.packages("rdbnomics")
library(quarto)     # for compiling Quarto presentations and documents
library(rdbnomics)     # for compiling Quarto presentations and documents
library(tidyverse)  # dplyr, ggplot2, readr, etc.
library(here)  # path settings
library(plotly)  # for interactive plots
library(countrycode)  # for country codes

unemp <- rdb(ids = "Eurostat/ei_lmhr_m/M.PC_ACT.SA.LM-UN-T-TOT.EE") # fetch data

glimpse(unemp)

# Extract source and series ID from the metadata
(source_name <- unique(unemp$dataset_code))  
(provider_code <- unique(unemp$provider_code))  
(country_name <- unique(unemp$`Geopolitical entity (reporting)`)  )
(series_id <- unique(unemp$series_code))  

# Plot the data
p1 <- ggplot(unemp, aes(x = period, y = value)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(
    title = paste("Unemployment Rate in ", country_name),
    subtitle = paste("Monthly, seasonally adjusted —", provider_code),
    x = "Date", y = "Percent",
    caption = paste("Source:", provider_code, "| Dataset:", source_name, "| ID:", series_id)
  ) +
  theme_minimal()
p1

ggplotly(p1)

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


unemp |> 
  filter(period >= "2010-01-01") |>
  ggplot(aes(x = period, y = value)) + 
  geom_line(color = "steelblue", linewidth = 1) +
  geom_hline(yintercept = 0, color = "#000", linewidth = 0.5) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    labels = scales::label_number(suffix = "%"),
    name = NULL
  ) +
  labs(
    title = paste("Unemployment Rate in", country_name),
    subtitle = paste("Monthly, seasonally adjusted"),
    x = NULL,
    caption = paste(
      "Source:",
      provider_code,
      "| Dataset:",
      source_name,
      "| ID:",
      series_id
    )
  ) +
  theme_ep() 

library(openxlsx)

wb <- createWorkbook()
addWorksheet(wb, "Unemployment Data")
writeData(wb, "Unemployment Data", unemp)
saveWorkbook(wb, "teaching_materials/unemp_data.xlsx", overwrite = TRUE)

# by ID
bop <- rdb(ids = c("IMF/BOP/A.FR.BCA_BP6_EUR", "IMF/BOP/A.DE.BCA_BP6_EUR"))
bop %>% count(`Reference Area`)

# by Mask
bop <- rdb(provider = "IMF",
           dataset_code =  "BOP",
           mask = "A.FR+DE.BCA_BP6_EUR")
bop %>% count(`Reference Area`)

# by Dimension
dim <- list(
  REF_AREA = c("DE", "FR"),
  INDICATOR = c("BCA_BP6_EUR"), 
  FREQ = "A"
)
## Here I do not include FREQUENCY in the dimension list. I would download annual and quarterly data
# dim <- list(
#   REF_AREA = c("DE", "FR"),
#   INDICATOR = c("BCA_BP6_EUR")
# ) 
bop <- rdb(provider = "IMF", dataset_code = "BOP", dimensions = dim)
bop %>% count(`Reference Area`)

# Line plot with color by country
p2 <- ggplot(bop, aes(x = period, y = value, color = `Reference Area`)) +
  geom_step(linewidth = 1) +
  labs(
    title = "Balance of Payments (BCA, EUR)",
    subtitle = "France vs Germany — Annual",
    x = "Year",
    y = "EUR (Millions)",
    caption = "Source: IMF / DBnomics"
  ) +
  theme_minimal()
p2

# cci_ee <- rdb("OECD", "DSD_HHDASH@DF_HHDASH_CTRY", "Q.EST.CCICP.IX")
cci <- rdb("OECD", "DSD_HHDASH@DF_HHDASH_CTRY", "Q..CCICP.IX")
# cci <- rdb("OECD", "DSD_HHDASH@DF_HHDASH_CTRY", "..CCICP.IX") # also wildcard the frequency
unique(cci$REF_AREA)

cci_eu <- cci %>%
  filter(
    REF_AREA %in% c("EU27_2020", "AUT", "EST", "FIN", "DEU",  "LVA", "LTU",  "POL", "SWE")) |> 
  mutate(
    REF_AREA = case_when(
      REF_AREA == "EU27_2020" ~ "EU27",
      TRUE ~ countrycode(REF_AREA, origin = "iso3c", destination = "iso2c")
    )
  )
unique(cci_eu$`Reference area`)


library(gghighlight)
desired_order <- c(
  "European Union (27 countries from 01/02/2020)",
  "Austria",
  "Germany",
  "Estonia",
  "Finland",
  "Lithuania",
  "Latvia",
  "Poland",
  "Sweden"
)
unique(cci_eu$`Reference area`)

p3 <- cci_eu |>
  ggplot() +
  geom_hline(yintercept = 100, linetype = "solid", size = 0.25) +
  geom_point(
    data = cci_eu %>%
      group_by(`Reference area`) %>%
      slice_max(period),
    aes(x = period, y = value, color = `Reference area`),
    shape = 16
  ) +
  geom_line(aes(x = period, y = value, color = `Reference area`)) +
  gghighlight(
    use_direct_label = FALSE,
    unhighlighted_params = list(colour = alpha("grey85", 1))
  ) +
  geom_text(
    data = cci_eu %>%
      group_by(`Reference area`) %>%
      slice_max(period),
    aes(x = period, y = value, color = `Reference area`, label = round(value)),
    hjust = -0.5,
    vjust = 0.5,
    size = 2.5,
    fontface = "bold"
  ) +
  scale_x_date(
    date_labels = "%Y",
    date_breaks = "2 years",
    name = NULL  
  ) +
  scale_y_continuous(
    breaks = c(90, 95, 100, 105, 110),
    labels = c("90", "", "100", "", "110"), 
    name = NULL
  ) +
  facet_wrap(
    ~ factor(`Reference area`, levels = desired_order)
  ) +
  coord_cartesian(clip = "off") +
  theme_bw() +
  theme(
    legend.position = "none"  
  )+
  labs(title = "Consumer Confidence – Index", 
       subtitle = "Selected European Countries & EU27",
       caption = "Source: OECD / DBnomics") 


p3
