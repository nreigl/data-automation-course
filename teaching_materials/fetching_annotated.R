#' ---
#' title: "Fetching and Visualizing Official Statistics with R"
#' author: "Nicolas"
#' date: "2025-04-17"
#' institute: "Eesti Pank"
#' format:
#'    revealjs: #  for presentation
#'     output-file: presentation_for_distribution_fetching.html
#'    # html: # for website
#'    #  output-file: page_for_distribution_fetching.html
#'     encoding: "UTF-8" # Estonian speical characters
#'     incremental: true
#'     preview-links: true
#'     scroll-view: false
#'     theme: simple
#'     smaller: true
#'     slide-number: true
#'     font-size: 0.6em
#'     transition: slide
#'     toc: true
#'     toc-depth: 2  # or 3, 4 etc.
#'     chalkboard: false # chalkboard and embed-resources can not be activted together
#'     embed-resources: false
#' editor_options:
#'   chunk_output_type: console
#' ---
#' 
#' # Interfaces to Official Statistics
#' 
#' - Packages or set of classes and methods to read data and metadata documents through exchange frameworks
#'   - Use [R](https://www.r-project.org/) (or [Python](https://www.python.org/)) packages to read data from APIs, databases, and web pages
#'     - Individual packages:
#'       - [eurostat](https://cran.r-project.org/package=eurostat): Access data from Eurostat
#'       - [OECD](https://cran.r-project.org/package=OECD): Access data from the OECD API
#'   - General-purpose packages:
#'     - [rdbnomics](https://cran.r-project.org/package=rdbnomics): Unified access to many economic databases (e.g. ECB, **Eurostat**, IMF, World Bank)
#' 
#' ---
#'   
#'   - Interface standards:
#'     - [SDMX](https://sdmx.org/?page_id=5008): Statistical Data and Metadata Exchange format
#'     - [pxweb](https://cran.r-project.org/package=pxweb): Access to data sources using the PX-Web API (e.g. Statistics Sweden, Statistics Estonia)
#' 
#' ---
#' 
#' ###  DBnomics 
#' 
#'  - [DBnomics](https://db.nomics.world/) is a database of databases
#'     - free platform to aggregate publicly-available economic data provided by national and international statistical institutions, but also by researchers and private companies
#'     - Unified interface to access data from many sources
#'     - Harmonized data formats and metadata 
#'     - Data series are available upon release by the provider 
#'     - Each revision is archived to build a real-time database
#' 
#' ## How to fetch data (from DBnomics using R) 
#' 
#' - DBnomics R client
#'  
#' ::: {.fragment}
#'  
## ---------------------------------------------------------------------------
#| eval: false
#| echo: true
# install.packages("rdbnomics")
# library(rdbnomics)

#' 
#' :::
#' 
#' ---
#' 
#' ### Packages used in this tutorial
#' 
#' 
#'   - 📦 Fetching data (`rdbnomics`)
#'   - 🧹 Data wrangling and transformation (`tidyverse`)
#'   - 📊 Visualization (`ggplot2`, `plotly`)
#'   - 📋 Tabular summaries (`gt`)
#'   - 🧾 Building this presentation (`quarto`)
#' 
#' ::: {.fragment}
#' 
## ---------------------------------------------------------------------------
library(quarto)     # for compiling Quarto presentations
library(rdbnomics)  # for accessing economic data via DBnomics
library(tidyverse)  # dplyr, ggplot2, readr, etc.
library(plotly)     # interactive visualizations
library(gt)         # pretty tables

#' 
#' :::
#' 
#' ---
#' 
#' ### Example: Fetch Unemployment Data 
#' 
#'    - Assume we know exactly the series ID we want to fetch
#'      - Unemployment rate, ILO definition, total, Estonia, from Eurostat
#' 
#' ::: {.fragment}
#' 
## ---------------------------------------------------------------------------
unemp <- rdb(ids = "Eurostat/ei_lmhr_m/M.PC_ACT.SA.LM-UN-T-TOT.EE") # fetch data

#' 
#' :::
#' 
#' --- 
#' 
#' 
## ---------------------------------------------------------------------------
glimpse(unemp)
colnames(unemp)

#' 
#' ---
#' 
## ---------------------------------------------------------------------------
# Extract source and series ID from the metadata
(source_name <- unique(unemp$dataset_code))  
(provider_code <- unique(unemp$provider_code))  
(country_name <- unique(unemp$`Geopolitical entity (reporting)`)  )
(series_id <- unique(unemp$series_code))  

#' 
#' --- 
#' 
## ---------------------------------------------------------------------------
#| echo: true
#| output-location: slide
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

#' 
#' ---
#' 
#' ### Interactive plot
#' 
## ---------------------------------------------------------------------------
ggplotly(p1)

#' 
#' 
#' ---
#' 
#' ### How do we find the series ID/mask/dimensions?
#' 
#' - Go to the [DBnomics website](https://db.nomics.world/)
#'   - Search directly for a series or pick a provider
#'   - Search for the [data](https://db.nomics.world/Eurostat/ei_lmhr_m?tab=list) you want (dataset_code)
#'   - Click on the [series](https://db.nomics.world/Eurostat/ei_lmhr_m/M.PC_ACT.SA.LM-UN-T-TOT.EE) (series_code) 
#'   - Copy the series ID from the URL  
#' 
#' ---
#' 
#' - Show the available datasets of a provider:
#' 
#' ::: {.fragment}
#' 
#' 
## ---------------------------------------------------------------------------
head(rdb_datasets(provider_code = "Eurostat"))

#' 
#' :::
#' 
#' ---
#' 
#' - Show the dimensions of a dataset:
#' 
#' ::: {.fragment}
#' 
#' 
## ---------------------------------------------------------------------------
head(rdb_dimensions(provider_code = "Eurostat", dataset_code = "ei_lmhr_m"))

#' 
#' :::
#' 
#' ---
#' 
#' 
#'  - Query to filter/select series from a provider's dataset
#' 
#' ::: {.fragment}
#' 
## ---------------------------------------------------------------------------
#| cache: true
head(rdb_series(
  provider = "Eurostat",
  dataset_code = "ei_lmhr_m",
  query = "United Kingdom"
))

#' 
#' :::
#' 
#' 
#' ## Fetch two (or more) series at once
#' 
#'  - Example: Balance of Payments (BOP) for France and Germany from the [IMF](https://db.nomics.world/IMF/BOP?tab=list) for Current Account, Total, Net, Euros, Millions, Annual
#' 
#' 
#' ::: {.fragment}
#' 
#' ::: {.panel-tabset}
#' 
#' ### Option A
#' 
#' 
## ---------------------------------------------------------------------------
# by ID
bop <- rdb(ids = c("IMF/BOP/A.FR.BCA_BP6_EUR", "IMF/BOP/A.DE.BCA_BP6_EUR"))
bop %>% count(`Reference Area`)

#' 
#' 
#' ### Option B: 
#' 
## ---------------------------------------------------------------------------
# by Mask
bop <- rdb(provider = "IMF",
           dataset_code =  "BOP",
           mask = "A.FR+DE.BCA_BP6_EUR")
bop %>% count(`Reference Area`)

#' 
#' 
#' ### Option C: 
#' 
#'  
#' 
#' <span style="color:red; font-weight:bold;">⚠️You must specify all dimensions!</span>
#' 
#' 
## ---------------------------------------------------------------------------
# by Dimension
dim <- list(
  REF_AREA = c("DE", "FR"),
  INDICATOR = c("BCA_BP6_EUR"),
  FREQ = "A"
)
bop <- rdb(provider = "IMF", dataset_code = "BOP", dimensions = dim)
bop %>% count(`Reference Area`)

#' 
#' 
#' :::
#' 
#' :::
#' 
#' ---
#' 
#' 
#' 
## ---------------------------------------------------------------------------
#| echo: true
#| output-location: slide
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

#' 
#' ## Fetch two series from different datasets of different providers 
#' 
## ---------------------------------------------------------------------------
unemp2 <- rdb(ids = c("AMECO/ZUTN/EA19.1.0.0.0.ZUTN", "Eurostat/une_rt_q/Q.SA.Y15-24.PC_ACT.T.EA19"))

#' 
## ---------------------------------------------------------------------------
# See which providers and datasets are included
dim(unemp2)
unique(unemp2$provider_code)
unique(unemp2$dataset_code)
unique(unemp2$series_code)
unique(unemp2$`@frequency`)
unique(unemp2$`Seasonal adjustment`)

#' 
#' ---
#' 
## ---------------------------------------------------------------------------
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


#' 
#' 
#' ---
#' 
## ---------------------------------------------------------------------------
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

#' 
#' ---
#' 
## ---------------------------------------------------------------------------
unemp2_summary_table

#' 
#' ----
#' 
## ---------------------------------------------------------------------------
# Metadata vectors
providers <- unique(unemp2$provider_code)
datasets <- unique(unemp2$dataset_code)
series_ids <- unique(unemp2$series_code)

#' 
## ---------------------------------------------------------------------------
# Create a label that combines dataset + series ID
unemp2_clean <- unemp2 %>%
  drop_na(value) %>%
  mutate(label = case_when(
    series_code == "EA19.1.0.0.0.ZUTN" ~ "Total, AMECO",
    series_code == "Q.SA.Y15-24.PC_ACT.T.EA19" ~ "Youth (15–24), Eurostat",
    TRUE ~ series_code
  ))

#' 
#' 
#' --- 
#' 
## ---------------------------------------------------------------------------
#| echo: true
#| output-location: slide
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

#' 
#' ## Fetch large amounts of data 
#' 
#'  - Sometimes you need to fetch many if not all dimensions of the data
#'  - You can wildcard dimension and post-filter
#'  - Example: MFI Interest Rate Statistics from the ECB
#'     - Start with a [single series](https://db.nomics.world/ECB/MIR/M.EE.B.A2C.A.R.A.2250.EUR.N) (Estonia, mortgage rates)
#'  
#' ::: {.fragment}
#' 
## ---------------------------------------------------------------------------
mir_mortgage_ee <- rdb("ECB", "MIR", "M.EE.B.A2C.A.R.A.2250.EUR.N")
unique(mir_mortgage_ee$series_name)

#' 
#' :::
#' 
#' ---
#' 
#' ### Wildcarding dimensions
#' 
#'   - To fetch **multiple values** for a dimension (e.g. countries), just **remove** the value from that position 
#'     - Example: remove `"EE"` to fetch all countries (`REF_AREA`)
#' 
#' ::: {.fragment}
#' 
#' <span style="color:red;">⚠️ This can take a while</span>
#' 
## ---------------------------------------------------------------------------
#| cache: true
# mir_mortgage_ee <- rdb("ECB", "MIR", "M.EE.B.A2C.A.R.A.2250.EUR.N")
mir <- rdb("ECB", "MIR", "M..B..A.R.A..EUR.N")
unique(mir$REF_AREA)
unique(mir$BS_ITEM)
unique(mir$`BS counterpart sector`)

#' 
#' :::
#'  
#' ---
#' 
#' ### Filter and plot 
#' 
#' - Filter Estonia, Latvia and Lithuania
#' - Keep only selected **BS items** (loan categories)  
#' 
#' ::: {.fragment}
#' 
## ---------------------------------------------------------------------------
# Filter by BS_ITEM and countries
mir_filtered <- mir %>%
  filter(
    REF_AREA %in% c("EE", "LV", "LT"),
    BS_ITEM %in% c("A2I", "A2C", "A2B", "A2J", "A2A")
  )

#' 
#' :::
#' 
#' ---
#' 
#' ### Plot interest rates by country & type
#' 
#' ::: {.fragment}
#' 
#' 
## ---------------------------------------------------------------------------
#| echo: true
#| output-location: slide
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

#' 
#' :::
#' 
#' ---
#' 
