library(tidyverse)
library(magrittr)

process_meter_soil_file = function(x){
  
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
  
  
  if(params$type == "Config"){

    config_data <- read_csv(x,
                            col_names = c("Key","Value"),
                            col_types = cols(
                              Key = col_character(),
                              Value = col_character()
                            )) %>%
      dplyr::mutate(Key = Key %>%
                      stringr::str_remove("\\[.*\\]") %>%
                      stringr::str_remove(":") %>%
                      stringr::str_trim()) %>%
      tidyr::spread(Key,Value) %>%
      dplyr::rename(soil_surface_area = `Soil surface area`,
                    soil_column_height = `Soil column height`,
                    lower_tensiometer_position =`Position lower tensiometer`,
                    upper_tensiometer_position = `Position upper tensiometer`,
                    measurement_head_net_weight = `Measurement head net weight`,
                    sampling_ring_weight = `Empty soil sampling ring weight`,
                    upper_air_entry_pressure = `Air entry pressure upper tensiometer`,
                    lower_air_entry_pressure = `Air entry pressure lower tensiometer`,
                    solid_substance_density = `Density of solid substance`,
                    bulk_density = `Bulk density`,
                    dry_weight = `Dry weight`,
                    porosity = `Porosity`) %>%
      dplyr::mutate(sample_date = params$date,
        station_key = params$station_key,
        depth = params$depth) %>%
      dplyr::select(sample_date,
        station_key,
        depth,
        soil_surface_area,
        soil_column_height,
        lower_tensiometer_position,
        upper_tensiometer_position,
        measurement_head_net_weight,
        sampling_ring_weight,
        upper_air_entry_pressure,
        lower_air_entry_pressure,
        solid_substance_density,
        bulk_density,
        dry_weight,
        porosity)
    
    return(list(config = config_data))
  }
  
  if(params$type == "Tension"){
    tension_data <- read_csv(x) %>%
      dplyr::mutate(`Date / Time` = lubridate::as_datetime(`Date / Time`, 
                                                           format = "%m*%d*%Y %I:%M:%S %p",
                                                           tz = "UTC") %>%
                      as.POSIXct()) %>%
      dplyr::rename(measurement_datetime = `Date / Time`,
        tension_lower = `Tension bottom [hPa]`,
                    tension_upper = `Tension top [hPa]`,
                    temperature = `Temperature [°C]`) %>%
      dplyr::mutate(station_key = params$station_key,
        depth = params$depth) %>%
      dplyr::select(station_key,
        depth,
        everything())
    
    return(list(tension = tension_data))
  }
  
  if(params$type == "Weight"){
    
    weight_data <- read_csv(x) %>%
      dplyr::mutate(`Date / Time` = lubridate::as_datetime(`Date / Time`, 
                                                           format = "%m*%d*%Y %I:%M:%S %p",
                                                           tz = "UTC") %>%
                      as.POSIXct()) %>%
      dplyr::rename(measurement_datetime = `Date / Time`,
                    gross_weight = `Gross weight [g]`,
                    net_weight = `Net weight [g]`,
                    weight_change = `Weight change [g]`) %>%
      dplyr::mutate(station_key = params$station_key,
                    depth = params$depth) %>%
      dplyr::select(station_key,
                    depth,
                    everything())
    
    return(list(weight = weight_data))
  }
  
  if(params$type == "pFVWC"){
    
    pf_vwc_data <- read_csv(x,
                            col_names = c("pF","VWC"),
                            col_types = cols(
                              pF = col_double(),
                              VWC = col_double()
                            )) %>%
      na.omit() %>% # filter out NA rows
      dplyr::mutate(KSat = NA,
                    # Convert from % to c^3/c^3 ratio
                    VWC = VWC / 100) %>%
      dplyr::mutate(station_key = params$station_key,
                    depth = params$depth) %>%
      dplyr::select(station_key,
                    depth,
                    everything())
    
    return(list(pf_vwc = pf_vwc_data))
    
  }
  
  if(params$type == "pario"){
    pario_data = tibble(time = c(NA,NA,NA,NA,NA),
                        pressure = NA,
                        cumulative_relative_mass = NA,
                        particle_diameter = NA) %>%
      dplyr::mutate(station_key = params$station_key,
                    depth = params$depth) %>%
      dplyr::select(station_key,
                    depth,
                    everything())
    
    return(list(pario = pario_data))
  }
  
  stop("Invalid input file.")
}


### Run on all CSVs in a directory ###
process_meter_soil_dir <- function(directory){
  all_data <- directory %>%
    list.files(full.names = TRUE, pattern = ".csv") %>%
    purrr::map(process_meter_soil_file) %>%
    unlist(recursive = FALSE)
  
  all_data %<>%
    names() %>%
    unique() %>%
    magrittr::set_names(.,.) %>%
    purrr::map(function(x){
      all_data[names(all_data) == x]
    }) %>%
    purrr::map(dplyr::bind_rows)
  
  return(list(configuration = all_data$config, 
              tensiometer = dplyr::full_join(all_data$tension, 
                                        all_data$weight), 
              water_retention = all_data$pf_vwc))
}

## Try it in the current working directory.
test <- process_meter_soil_dir(getwd())

test$configuration %>% names()
