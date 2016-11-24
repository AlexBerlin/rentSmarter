#' Extract relevant data from the 'object_info2' column.
#' 
#' @param data data.frame that at the least includes the 'object_info2' column.
#' @param keep_object_info2_column single logical value; whether to keep the 'object_info2' column.
#' @return A new \code{data.frame} with new columns that were extracted from the 'object_info2' column.
#' @examples
#' extractInfoOneView(data)
extractObjectInfo2 <- function(data, keep_object_info2_column = TRUE) {
  if (!is.data.frame(data)) stop("'data' has to be a data.frame!")
  if (!"object_info2" %in% names(data)) stop("'data' needs to include the column 'object_info2'!")
  

  ## Extract information on whether bath tube and/or shower are present in flat
  
  # Detect German words for 'shower' and 'bath tube' 
  data$shower <- stringi::stri_detect_fixed(data$object_info2, "Dusche")
  data$bath <- stringi::stri_detect_fixed(data$object_info2, "Badewanne")
  # Logical AND for two previous vectors
  data$bath_and_shower <- data$bath & data$shower
  
  
  ## Extract information on whether several item are present or not in flat
  ## Each item's presence can be detected by a certain, fixed German term found in the text (or not)
  
  # Washing machine
  data$washing_machine <- stringi::stri_detect_fixed(data$object_info2, "Waschmaschine")

  # Dish washer
  data$washing_machine <- stringi::stri_detect_fixed(data$object_info2, "Spülmaschine")

  # Balcony
  data$balcony <- stringi::stri_detect_fixed(data$object_info2, "Balkon")
  
  # Garden
  data$garden <- stringi::stri_detect_fixed(data$object_info2, "Garten")

  # Basement
  data$basement <- stringi::stri_detect_fixed(data$object_info2, "Keller")

  # Elevator / lift
  data$basement <- stringi::stri_detect_fixed(data$object_info2, "Aufzug")

  # Bike storage
  data$bike_storage <- stringi::stri_detect_fixed(data$object_info2, "Fahrradkeller")

  # Pets allowed in flat
  data$pets_allowed <- stringi::stri_detect_fixed(data$object_info2, "Haustiere erlaubt")

  # Terrace
  data$terrace <- stringi::stri_detect_fixed(data$object_info2, "Terrasse")
  
  
  ## Extract information about the parking situation in the flat's area
  
  # There several different, fixed choices
  data$parking_situation <- "parking_situation_not_specified"
  data$parking_situation[grep("gute Parkmöglichkeiten", 
                              data$object_info2, perl = TRUE)] <- "good"
  data$parking_situation[grep("schlechte Parkmöglichkeiten", 
                              data$object_info2, perl = TRUE)] <- "bad"
  data$parking_situation[grep("Anwohnerparken", 
                              data$object_info2, perl = TRUE)] <- "residential_parking_zone"
  data$parking_situation[grep("eigener", 
                              data$object_info2, perl = TRUE)] <- "private_parking_space"
  data$parking_situation[grep("Tiefgaragenstellplatz", 
                              data$object_info2, perl = TRUE)] <- "underground_garage"
  

  
  ## Extract numerical value which indicates how far (in minutes by foot) the next 
  ## public transport station is
  
  # a) The value is found after a fixed phrase
  data$public_transport <- 
    as.character(stringi::stri_match_all_regex(data$info_one_view, "Verkehrsmittel: (\\w+)"))
  # b) Remove the fixed phrase before the value to get it as a string
  data$public_transport <- stringi::stri_replace_all_fixed(data$public_transport,
                                                           "Verkehrsmittel: ", replacement = "")
  # c) Convert string to numeric value
  data$public_transport <- as.numeric(data$public_transport)
  
  
  ## Extract information about the kind of heating in the flat
  
  # a) Match pattern 'Heizung: (\\w+)' 
  # b) Results returns a list where each entry is a matrix with two columns. The entry in the
  #    second column is the string needed.
  heating <- stringi::stri_match_all_regex(data$object_info2, "Heizung: (\\w+)")
  # c) Use lapply() + unlist() to get the needed string from the second column of each list element
  data$heating <- unlist(lapply(heating, `[[`, 2))
  # e) remove 'heating' object
  rm(heating)
  
  
  ## Extract information about the kind of floor cover(s) in the flat / room
  
  # There several different, fixed choices
  data$floor_cover <- "floor_cover_not_specified"
  data$floor_cover[grep("Dielen", data$object_info2, perl = TRUE)] <- "plank"
  data$floor_cover[grep("Parkett", data$object_info2, perl = TRUE)] <- "parquet"
  data$floor_cover[grep("Laminat", data$object_info2, perl = TRUE)] <- "laminate"
  data$floor_cover[grep("Teppich", data$object_info2, perl = TRUE)] <- "carpet"
  data$floor_cover[grep("Fliesen", data$object_info2, perl = TRUE)] <- "floortile"
  data$floor_cover[grep("PVC", data$object_info2, perl = TRUE)] <- "pvc"
  data$floor_cover[grep("Fußbodenheizung", data$object_info2, perl = TRUE)] <- "floor_heating"
  
  
  ### Extract information about the internet connection in the flat
  
  # There are only a few fixed choices
  data$internet_connection <- "internet_connection_not_specified"
  data$internet_connection[grep("DSL", data$object_info2, perl = TRUE)] <- "dsl"
  data$internet_connection[grep("WLAN", data$object_info2, perl = TRUE)] <- "wifi"


  ## Extract information which indicates the DSL (internet) speed in the flat. There are several 
  ## fixed choices, which indicate the numerical range of the speed.
  
  # a) The string needed is found after a fixed phrase. The output is in matrix format, but we can
  #    add the subset '[, 1]' to directly extract the needed string value from the matrix.
  data$speed_dsl <- 
    stringi::stri_match_first_regex(data$object_info2, "DSL-Speed: (\\w+)-(\\w+)")[, 1]
  # b) We can now remove the redudant phrase before the relevant values
  data$speed_dsl <- stringi::stri_replace_all_fixed(data$speed_dsl, "DSL-Speed: ", "")
  
  
  ### Extract information about the telephone connection in the flat
  
  # There are only a few fixed choices
  data$telephone_connection <- "telephone_connection_not_specified"
  data$telephone_connection[grep("Analog", data$object_info2, perl = TRUE)] <- "analog"
  data$telephone_connection[grep("ISDN", data$object_info2, perl = TRUE)] <- "ISDN"
  data$telephone_connection[grep("Internettelefonie", 
                                 data$object_info2, perl = TRUE)] <- "internet_telephone"
  
  
  ### Extract information about the TV in the flat
  
  # There two fixed choices
  data$tv <- "tv_not_specified"
  data$tv[grep("Kabel", data$object_info2, perl = TRUE)]    <- "cable"
  data$tv[grep("Satellit", data$object_info2, perl = TRUE)] <- "satellit"

  
  if (!keep_object_info2_column) data$object_info2 <- NULL
  
  return(data)
}

