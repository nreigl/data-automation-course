# install.packages("rdbnomics")
# library(rdbnomics)

library(quarto)     # for compiling Quarto presentations
library(rdbnomics)  # for accessing economic data via DBnomics
library(pxweb)
library(tidyverse)  # dplyr, ggplot2, readr, etc.
library(plotly)     # interactive visualizations
library(gt)         # pretty tables
library(countrycode)

unemp <- rdb(ids = "Eurostat/ei_lmhr_m/M.PC_ACT.SA.LM-UN-T-TOT.EE") # fetch data

glimpse(unemp)
colnames(unemp)

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

source(here::here("scripts", "theme_ep.R"))
ggplot(unemp, aes(x = period, y = value)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_hline(yintercept = 0, color = "#000", linewidth = 0.5) +
  scale_x_date(
    date_breaks = "2 year",
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

head(rdb_datasets(provider_code = "Eurostat"))

head(rdb_dimensions(provider_code = "Eurostat", dataset_code = "ei_lmhr_m"))

head(rdb_series(
  provider = "Eurostat",
  dataset_code = "ei_lmhr_m",
  query = "United Kingdom"
))

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

unemp2 <- rdb(ids = c("AMECO/ZUTN/EA19.1.0.0.0.ZUTN", "Eurostat/une_rt_q/Q.SA.Y15-24.PC_ACT.T.EA19"))

# See which providers and datasets are included
dim(unemp2)
unique(unemp2$provider_code)
unique(unemp2$dataset_code)
unique(unemp2$series_code)
unique(unemp2$`@frequency`)
unique(unemp2$`Seasonal adjustment`)

# Summarize coverage and data availability
unemp2_summary <- unemp2 %>%
  group_by(series_code) %>%
  summarize(
    provider = first(provider_code),
    dataset = first(dataset_code),
    start_all = min(period, na.rm = TRUE),
    end_all = max(period, na.rm = TRUE),
    start_data = min(period[!is.na(value)]),
    end_data = max(period[!is.na(value)]),
    n_obs = sum(!is.na(value)),
    .groups = "drop"
  )


unemp2_summary_table <- unemp2_summary |> 
gt() %>%
  tab_header(
    title = "Time Coverage and Non-Missing Observations",
    subtitle = "For Each Series from AMECO and Eurostat"
  ) %>%
  cols_label(
    series_code = "Series ID",
    provider = "Provider",
    dataset = "Dataset",
    start_all = "Start (all)",
    end_all = "End (all)",
    start_data = "Start (non-NA)",
    end_data = "End (non-NA)",
    n_obs = "# Obs"
  ) %>%
  fmt_date(
    columns = c(start_all, end_all, start_data, end_data),
    date_style = "iso"
  ) %>%
  tab_options(
    table.width = pct(100),
    column_labels.font.weight = "bold"
  )

unemp2_summary_table

# Metadata vectors
providers <- unique(unemp2$provider_code)
datasets <- unique(unemp2$dataset_code)
series_ids <- unique(unemp2$series_code)

# Create a label that combines dataset + series ID
unemp2_clean <- unemp2 %>%
  drop_na(value) %>%
  mutate(label = case_when(
    series_code == "EA19.1.0.0.0.ZUTN" ~ "Total, AMECO",
    series_code == "Q.SA.Y15-24.PC_ACT.T.EA19" ~ "Youth (15–24), Eurostat",
    TRUE ~ series_code
  ))

p3 <- ggplot(unemp2_clean, aes(x = period, y = value, color = label)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Unemployment Rates from Multiple Sources (EA19)",
    subtitle = "AMECO and Eurostat — Different definitions",
    x = "Year", y = "Percent",
    caption = paste("Series IDs:", paste(unique(unemp2_clean$series_code), collapse = " | "))
  ) +
  theme_minimal()
p3

cci_ee <- rdb("OECD", "DSD_HHDASH@DF_HHDASH_CTRY", "Q.EST.CCICP.IX")
unique(cci_ee$series_name)
unique(cci_ee$REF_AREA)

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
cci_eu |>
  ggplot() +
  geom_hline(yintercept = 100, linetype = "solid", size = .25) +
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
    hjust = -.5,
    vjust = .5,
    size = 2.5,
    fontface = "bold"
  ) +
  scale_x_date(
  date_labels = "%Y",
  date_breaks = "2 years"
)+
  scale_y_continuous(
    breaks = c(90, 95, 100, 105, 110),
    labels = c("90", "", "100", "", "110")
  ) +
  facet_wrap(
    ~ factor(`Reference area`, levels = desired_order)
  ) +
  coord_cartesian(clip = "off") +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

mir_mortgage_ee <- rdb("ECB", "MIR", "M.EE.B.A2C.A.R.A.2250.EUR.N")
unique(mir_mortgage_ee$series_name)

# mir_mortgage_ee <- rdb("ECB", "MIR", "M.EE.B.A2C.A.R.A.2250.EUR.N")
mir <- rdb("ECB", "MIR", "M..B..A.R.A..EUR.N")
unique(mir$REF_AREA)
unique(mir$BS_ITEM)
unique(mir$`BS counterpart sector`)

# Filter by BS_ITEM and countries
mir_filtered <- mir %>%
  filter(
    REF_AREA %in% c("EE", "LV", "LT"),
    BS_ITEM %in% c("A2I", "A2C", "A2B", "A2J", "A2A")
  )

country_list <- paste(sort(unique(mir_filtered$REF_AREA)), collapse = ", ")
item_list <- paste(unique(mir_filtered$BS_ITEM), collapse = ", ")

caption_text <- paste(
  "Source: ECB / DBnomics — Dataset code: MIR",
  paste0("\nFiltered: REF_AREA in ", country_list, "; BS_ITEM in ", item_list)
)

mir_filtered <- mir_filtered %>%
  mutate(facet_label = paste0(`BS counterpart sector`, ".\n\n", `Balance sheet item`))

p4 <- ggplot(mir_filtered, aes(x = period, y = value, color = REF_AREA)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ facet_label, labeller = label_wrap_gen(width = 30), ncol = 3) +
  labs(
    title = "Interest Rates for Households and Firms",
    subtitle = "Faceted by Loan Type and Borrower Sector",
    x = "Date", y = "Percent",
    caption = caption_text
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
p4

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
pa111_plotdata <- pa111.df %>%
  mutate(
    quarter = yq(str_replace(`Reference period`, " ", "-")),
    Indicator = recode(
      Näitaja,
      "GR_W_D1" = "D1",
      "GR_W_D5" = "Median",
      "GR_W_D9" = "D9"
    )
  ) %>%
  filter(
    `Economic activity` %in% selected_sectors,
    Indicator %in% c("D1", "Median", "D9")
  ) %>%
  select(quarter, `Economic activity`, Indicator, value = `PA111: AVERAGE MONTHLY GROSS WAGES (SALARIES), MEDIAN, DECILES AND NUMBER OF EMPLOYEES`) %>%
  pivot_wider(names_from = Indicator, values_from = value)

overall_median <- pa111_plotdata %>%
  filter(`Economic activity` == "Total - all activities") %>%
  select(quarter, total_median = Median)

pa111_plotdata <- pa111_plotdata %>%
  filter(`Economic activity` != "Total - all activities") %>%
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
