#' Extract relevant data from the 'offer_date' column.
#' 
#' @param data data.frame that at the least includes the 'offer_date' column.
#' @param keep_date_column single logical value; whether to keep the date column.
#' @param keep_time_column single logical value; whether to keep the time column.
#' @return A new \code{data.frame} with new columns that were extracted from the 'offer_date' column.
#' @examples
#' extractOfferDate(data)
extractOfferDate <- function(data, 
                             keep_date_column = FALSE, 
                             keep_time_column = FALSE) {
  if (!is.data.frame(data)) stop("'data' has to be a data.frame!")
  if (!"offer_date" %in% names(data)) stop("'data' needs to include the column 'offer_date'!")
  
  ## Extract time of offer from 'offer_date' column
  # a) Use regex to match offer time (function returns matrix format)
  # b) Convert matrix format to character string
  data$offer_time <- as.character(stringi::stri_match_last_regex(data$offer_date, 
                                                                 "[0-9][0-9]\\:[0-9][0-9]?"))
  # c) Replace ":" between hour and minute to match format date format in the next steps
  data$offer_time <- stringi::stri_replace_first_fixed(data$offer_time, 
                                                       pattern = ":", 
                                                       replacement = "-")
  
  ## Extract date of offer from 'offer_date' column
  # a) Use regex to match offer date pattern dd/mm/yyyy (function returns matrix format)
  # b) Convert matrix format to data vector with lubridate::hm()
  data$offer_date <- 
    lubridate::dmy(stringi::stri_match_first_regex(data$offer_date, 
                                                   "[0-9][0-9]\\.[0-9][0-9]\\.[0-9]{4}"))
  
  ## Create column 'offer_date_time' which includes the full date + time of the offer
  # a) paste together the two columns 'offer_date' + 'offer_time'
  data$offer_date_time <- paste(data$offer_date, data$offer_time)
  # b) Use 'lubridate::ymd_hm' to convert to proper date format
  data$offer_date_time <- lubridate::ymd_hm(data$offer_date_time)
  
  if (!keep_date_column) data$offer_date <- NULL
  if (!keep_time_column) data$offer_date <- NULL
  
  return(data)
}