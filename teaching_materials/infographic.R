library(rdbnomics)

trade <- get_eurostat("ext_lt_intertrd", time_format = "date")

unique(trade$geo)
unique(trade$partner)
