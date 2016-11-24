#' Extract relevant data from the 'object_info1' column.
#' 
#' @param data data.frame that at the least includes the 'object_info1' column.
#' @param keep_object_info1_column single logical value; whether to keep the 'object_info1' column.
#' @return A new \code{data.frame} with new columns that were extracted from the 'object_info1' column.
#' @examples
#' extractInfoOneView(data)
extractObjectInfo1 <- function(data, keep_object_info1_column = TRUE) {
  if (!is.data.frame(data)) stop("'data' has to be a data.frame!")
  if (!"object_info1" %in% names(data)) stop("'data' needs to include the column 'object_info1'!")
  
  
  ## Extract information on which floor the flat is located
  
  # There several different, fixed choices
  data$floor <- "floor_not_specified"
  data$floor[grep("Keller", data$object_info1, perl = TRUE)] <- "keller"
  data$floor[grep("Tiefparterre", data$object_info1, perl = TRUE)] <- "tiefparterre"
  data$floor[grep("Hochparterre", data$object_info1, perl = TRUE)] <- "hochparterre"
  data$floor[grep("EG", data$object_info1, perl = TRUE)] <- "eg"
  data$floor[grep("1. OG", data$object_info1, perl = TRUE)] <- "1.og"
  data$floor[grep("2. OG", data$object_info1, perl = TRUE)] <- "2.og"
  data$floor[grep("3. OG", data$object_info1, perl = TRUE)] <- "3.og"
  data$floor[grep("4. OG", data$object_info1, perl = TRUE)] <- "4.og"
  data$floor[grep("5. OG", data$object_info1, perl = TRUE)] <- "5.og"
  data$floor[grep("als 5. OG", data$object_info1, perl = TRUE)] <- "higher_than_5.og"
  data$floor[grep("Dachgeschoss", data$object_info1, perl = TRUE)] <- "dachgeschoss"
  
  
  ## Extract information about the building type that the flat is located in
  
  # There several different, fixed choices
  data$building_type <- "building_not_specified"
  data$building_type[grep("Altbau", data$object_info1, perl = TRUE)] <- "altbau"
  data$building_type[grep("sanierter Altbau", data$object_info1, perl = TRUE)] <- "sanierter_altbau"
  data$building_type[grep("Neubau", data$object_info1, perl = TRUE)] <- "neubau"
  data$building_type[grep("Reihenhaus", data$object_info1, perl = TRUE)] <- "reihenhaus"
  data$building_type[grep("Doppelhaus", data$object_info1, perl = TRUE)] <- "doppelhaus"
  data$building_type[grep("Einfamilienhaus", data$object_info1, perl = TRUE)] <- "einfamilienhaus"
  data$building_type[grep("Mehrfamilienhaus", data$object_info1, perl = TRUE)] <- "mehrfamilienhaus"
  data$building_type[grep("Hochhaus", data$object_info1, perl = TRUE)] <- "hochhaus"
  data$building_type[grep("Plattenbau", data$object_info1, perl = TRUE)] <- "plattenbau"
  
  ## Extract information on whether the flat is partly or fully furnished
  
  # If word 'teilmöbliert' is present --> partly furnished
  # If word 'möbliert' (without the preceding 'teil') --> fully furnished
  data$partly_furnished <- stringi::stri_detect_regex(data$object_info1, "teilmöbliert")
  data$fully_furnished <- stringi::stri_detect_regex(data$object_info1, "\\bmöbliert")
  
  
  ## Extract information on whether the flat is barrier free, i.e. accessible for wheelchairs
  
  # If word 'Barrierefrei' is present --> barrier free
  data$barrier_free <- stringi::stri_detect_regex(data$object_info1, "Barrierefrei")
  
  
  if (!keep_object_info1_column) data$object_info1 <- NULL
  
  return(data)
}

