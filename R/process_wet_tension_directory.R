library(tidyverse)
library(magrittr)

process_wet_tension_file = function(x){
  # x <- "./data/wet_tension/161021_havrenmt_2.csv"

  message("Processing ", basename(x))
  params <- x %>%
    basename() %>%
    tools::file_path_sans_ext() %>%
    # stringr::str_remove("\\(.*\\)") %>%
    stringr::str_split("_") %>%
    magrittr::extract2(1) %>%
    magrittr::set_names(c("sample_date","station_key","depth")) %>%
    as.list() %>%
    as_tibble() %>%
    dplyr::mutate(sample_date = lubridate::as_date(sample_date),
                  depth = as.numeric(depth))


  out <- readr::read_file(x) %>%
    stringr::str_split("\r\nDate / Time") %>%
    unlist() %>%
    magrittr::extract(1:2) %>%
    purrr::map(read_csv) %>%
    magrittr::set_names(c("samples","wet_tension_observations"))

  out$samples %<>%
    magrittr::set_names(c("Key","Value")) %>%
    dplyr::mutate(Key = Key %>%
                    stringr::str_remove("\\[.*\\]") %>%
                    stringr::str_remove(":") %>%
                    stringr::str_trim()) %>%
    na.omit() %>%
    dplyr::mutate(Key = Key %>% stringr::str_replace("Serial number",c("Serial number 1","Serial number 2"))) %>%
    tidyr::spread(Key,Value) %>%
    dplyr::mutate(sample_date = params$sample_date,
                  station_key = params$station_key,
                  depth = params$depth) %>%
    dplyr::select(station_key,
                  depth,
                  sample_date,
                  soil_surface_area = `Soil surface area`,
                  soil_column_height = `Soil column height`,
                  lower_tensiometer_position =`Position lower tensiometer`,
                  upper_tensiometer_position = `Position upper tensiometer`,
                  measurement_head_net_weight = `Measurement head net weight`,
                  sampling_ring_weight = `Empty soil sampling ring weight`,
                  upper_air_entry_pressure = `Air entry pressure upper tensiometer`,
                  lower_air_entry_pressure = `Air entry pressure lower tensiometer`,
                  solid_substance_density = `Density of solid substance`,
                  initial_water_content = `Initial water content`,
                  dry_weight = `Dry soil weight`,
                  bulk_density = `Density`,
                  porosity = `Porosity`,
                  tensiometer = `Tensiometer`,
                  scale = `Scale`,
                  busnumber = `Busnumber`,
                  subaddress = `Subaddress`,
                  serial_number_1 = `Serial number 1`,
                  sensor_unit_name = `Sensor unit name`,
                  firmware_version = `Firmware Version`,
                  serial_number_2 = `Serial number 2`,
                  scale_name = `Scale name`)

  out$wet_tension_observations %<>%
    dplyr::rename(`Date / Time` = X1) %>%
    dplyr::mutate(`Date / Time` = lubridate::as_datetime(`Date / Time`,
                                                         format = "%m*%d*%Y %I:%M:%S %p",
                                                         tz = "UTC") %>%
                    as.POSIXct()) %>%
    dplyr::rename(measurement_datetime = `Date / Time`,
                  tension_lower = `Tension bottom [hPa]`,
                  tension_upper = `Tension top [hPa]`,
                  temperature = `Temperature [°C]`,
                  gross_weight = `Gross weight [g]`,
                  net_weight = `Net weight [g]`,
                  weight_change = `Weight change [g]`) %>%
    dplyr::mutate(station_key = params$station_key,
                  depth = params$depth) %>%
    dplyr::select(station_key,
                  depth,
                  everything())

  return(out)

}

### Run on all CSVs in a directory ###
process_wet_tension_dir <- function(directory){

  # directory <- "./data/wet_tension/"
  out <- directory %>%
    list.files(full.names = TRUE, pattern = ".csv") %>%
    purrr::map(process_wet_tension_file) %>%
    unlist(recursive = FALSE)

  out %<>%
    names() %>%
    unique() %>%
    magrittr::set_names(.,.) %>%
    purrr::map(function(x){
      out[names(out) == x]
    }) %>%
    purrr::map(dplyr::bind_rows)

  return(out)
}
