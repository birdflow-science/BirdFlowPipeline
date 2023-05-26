## Create table to join 6-letter code to common name of species

tax_join <- data.table::fread(file.path('data-raw', 'eBird_Taxonomy_v2021.csv')) %>% select(SPECIES_CODE, PRIMARY_COM_NAME)

usethis::use_data(tax_join, internal = TRUE)
