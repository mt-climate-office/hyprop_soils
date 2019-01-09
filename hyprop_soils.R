library(magrittr)
library(tidyverse)
library(writexl)

# Load all functions
list.files("./R",
           full.names = TRUE) %>%
  purrr::walk(source)

# Process all the csvs in the wet_tension directory
# also outputs samples data
wet_tension <- process_wet_tension_dir("./data/wet_tension/")

# Process the dry tension observations xlsx file
dry_tension <- process_dry_tension_observations("./data/dry_tension_observations.xlsx")

# Process all the csvs in the pario directory
# pario <- process_pario_dir("./data/pario/")

# Output the processed datasets
append(wet_tension,list(dry_tension_observations = dry_tension)) %>%
  writexl::write_xlsx('./data-derived/hyprop_soils_output.xlsx')

# R/T, just to check
'./data-derived/hyprop_soils_output.xlsx' %>%
readxl::excel_sheets() %>%
  magrittr::set_names(.,.) %>%
  purrr::map(function(sheet){
    readxl::read_excel('./data-derived/hyprop_soils_output.xlsx',
                       sheet = sheet)
  })

