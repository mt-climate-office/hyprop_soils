library(tidyverse)
library(magrittr)
library(readxl)

process_dry_tension_observations <- function(x){
  x <- "./data/dry_tension_observations.xlsx"
  x %>%
    readxl::read_excel() %>%
    dplyr::rename(station_key = site,
                  wet_mass = `Wet Mass`,
                  dry_mass = `Dry Mass`)
}
