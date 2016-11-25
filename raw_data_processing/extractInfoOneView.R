#' Extract relevant data from the 'info_one_view' column.
#' 
#' @param data data.frame that at the least includes the 'info_one_view' column.
#' @param keep_info_one_view_column single logical value; whether to keep the 'info_one_view' column.
#' @return A new \code{data.frame} with new columns that were extracted from the 'info_one_view' column.
#' @examples
#' extractInfoOneView(data)
extractInfoOneView <- function(data, keep_info_one_view_column = TRUE) {
  if (!is.data.frame(data)) stop("'data' has to be a data.frame!")
  if (!"info_one_view" %in% names(data)) stop("'data' needs to include the column 'info_one_view'!")
  
  
  ## Extract info about 'daily rent' (TRUE / FALSE)
  # Set column to TRUE if string "Tagesmiete" (daily rent) is found
  data$daily_rent <- stringi::stri_detect_regex(data$info_one_view, "Tagesmiete")
  
  
  ## Extract numerical value for 'Kaltmiete' (bare rent)
  # a) Match pattern 'Miete: (\\w+)' 
  # b) Results returns a list where each entry is a matrix with two columns. The entry in the
  #    second column is  the numerical value for the rent.
  bare_rent <- stringi::stri_match_all_regex(data$info_one_view, "Miete: (\\w+)")
  # c) Use lapply() + unlist() to get the rent value from second column of each list element
  data$bare_rent <- unlist(lapply(bare_rent, `[[`, 2))
  # d) convert string to numeric
  data$bare_rent <- as.numeric(data$bare_rent)
  # e) remove 'bare_rent' object
  rm(bare_rent)
  
  
  ## Check if NA in results match with TRUE in 'daily_rent' column as expected
  stopifnot(is.na(data$bare_rent) == data$daily_rent)
  
  
  ## Extract numerical value for 'Nebenkosten' (utilities)
  
  # Same process as previous step 'Kaltmiete'. Only the string pattern changes:
  # Match pattern 'Nebenkosten: (\\w+)'
  utilities <- stringi::stri_match_all_regex(data$info_one_view, "Nebenkosten: (\\w+)")
  data$utilities <- unlist(lapply(utilities, `[[`, 2))
  # This will likely produce a warning message:                                 (!!!!!)
  # 'NAs introduced by coercion '                                               (!!!!!)
  # This is for offers without the respective information and can be ignored    (!!!!!)
  data$utilities <- as.numeric(data$utilities)
  rm(utilities)
  
  
  ## Extract numerical value for 'Nebenkosten' (other cost)
  
  # Same process as previous steps. Only the string pattern changes:
  # Match pattern 'Sonstige Kosten: (\\w+)'
  other_cost <- stringi::stri_match_all_regex(data$info_one_view, "Sonstige Kosten: (\\w+)")
  data$other_cost <- unlist(lapply(other_cost, `[[`, 2))
  # This will likely produce a warning message:                                 (!!!!!)
  # 'NAs introduced by coercion '                                               (!!!!!)
  # This is for offers without the respective information and can be ignored    (!!!!!)
  data$other_cost <- as.numeric(data$other_cost)
  rm(other_cost)
  
  
  ## Extract numerical value for 'Kaution' (deposit)
  
  # Same process as previous steps. Only the string pattern changes:
  # Match pattern 'Kaution: (\\w+)'
  deposit <- stringi::stri_match_all_regex(data$info_one_view, "Kaution: (\\w+)")
  data$deposit <- unlist(lapply(deposit, `[[`, 2))
  data$deposit <- as.numeric(data$deposit)
  rm(deposit)
  
  
  ## Extract numerical value for numbers of tenants in the shared flat 
  
  # a) The number we are looking for is found before and after certain phrases in the string
  data$number_tenants <- as.character(stringi::stri_match_all_regex(data$info_one_view, 
                                                                   "Zimmer in \\w+ WG"))
  # b) From the resulting string we want to extract the sole number (= total number of tenants)
  data$number_tenants <- as.numeric(stringi::stri_match_first_regex(data$number_tenants, 
                                                                   "[0-9]{1,2}"))
  
  
  ## Extract the data from which the room is available from 

  # a) The date is found after a fixed phrase
  data$available_from <- 
    as.character(
      stringi::stri_match_all_regex(data$info_one_view,
                                    "frei ab: [0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9][0-9][0-9]"))
  # b) Remove the fixed phrase before the date to get the data as a string
  data$available_from <- stringi::stri_replace_all_fixed(data$available_from,
                                                        "frei ab: ", replacement = "")
  # c) Convert string to a date format
  data$available_from <- dmy(data$available_from)
  
  ## Extract the data until which the room is available ('available_until')
  # Same steps as for 'available_from'
  data$available_until <- 
    as.character(
      stringi::stri_match_all_regex(data$info_one_view,
                                    "frei bis: [0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9][0-9][0-9]"))
  data$available_until <- stringi::stri_replace_all_fixed(data$available_until,
                                                         "frei bis: ", replacement = "")
  data$available_until <- dmy(data$available_until)
  
  ## Calculate the duration which the room is available (NA = indefinite)
  # Durdation is difference between end and start date. NA means rental offer has no end 
  # date, i.e. indefinite
  data$duration <- (data$available_until - data$available_from)
  
  
  ## Mark whether the room is avialble only for a limited time
  
  data$limited_time <- dplyr::if_else(is.na(data$duration), TRUE, FALSE)
  
  ## Extract the numerical value for the ZIP code
  
  # The ZIP code is the first five digit number found in the string
  data$zip <- as.integer(stringi::stri_match_first_regex(data$info_one_view, "[0-9]{5}"))
  
  
  ## Extract the (raw) string info for the district
  
  # The first word of the district name is always found after the first mention of the 
  # word 'Berlin' in the string.
  # The next word after that might be part of the district name or not, but most districts can
  # be identified by the first word alone. The rest of the districts can (to a large degree) be
  # identified by their ZIP code. This will be done in a separate function.
  district <- stringi::stri_match_all_regex(data$info_one_view, "Berlin (\\w+)")
  data$district <- unlist(lapply(district, `[[`, 2))
  data$district <- as.character(data$district)
  rm(district)
  
  
  ## Extract the address string
  
  # Extracting the address is a bit tricky. The address has a differing length, but is always found
  # after the district and before the room size in square meter ('m²').
  # Therefore we have to determine the start and end position of these strings for each vector 
  # element in order to extract the address information between them.
  start_pos <- stringi::stri_locate_first_regex(data$info_one_view, data$distric)[, "end"]
  end_pos   <- stringi::stri_locate_first_regex(data$info_one_view, "[0-9]+m²")[, "start"]
  
  # Fetch the substring between the 'start_pos' and 'end_pos'
  data$address <- stringi::stri_sub(data$info_one_view, start_pos + 1, end_pos - 1)
  # Delete leading and trailing spaces
  data$address <- stringi::stri_trim(data$address)
  # Delete 'start_pos' & 'end_pos'
  rm(start_pos, end_pos)
  
    
  if (!keep_info_one_view_column) data$info_one_view <- NULL
  
  return(data)
}

