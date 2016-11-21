#' Extract relevant data from the 'size_and_rent' column.
#' 
#' @param data data.frame that at the least includes the 'size_and_rent' column.
#' @param keep_size_and_rent_column single logical value; whether to keep the 'size_and_rent' column.
#' @return A new \code{data.frame} with new columns that were extracted from the 'size_and_rent' column.
#' @examples
#' extractSizeRent(data)
extractSizeRent <- function(data, keep_size_and_rent_column = TRUE) {
  if (!is.data.frame(data)) stop("'data' has to be a data.frame!")
  if (!"size_and_rent" %in% names(data)) stop("'data' needs to include the column 'size_and_rent'!")
  
  ## Extract room size ('Zimmergröße') (in square meter) from 'size_and_rent' column
  # a) Match first number inside 'size_and_rent' as room size. Expected value 
  #    between 0-999 (returns matrix format)
  data$size <- stringi::stri_match_first_regex(data$size_and_rent, "[0-9]{1,3}")
  # b) convert to numeric vector
  data$size <- as.numeric(data$size)
  
  ## Extract total rent ('Gesamtmiete') from 'size_and_rent' column
  # a) match last number as rent. Expected value between 0-9999 (returns matrix format)
  data$rent <- stringi::stri_match_last_regex(data$size_and_rent, "[0-9]{1,4}")
  # b) # b) convert to numeric vector
  data$rent <- as.numeric(data$rent)
  
  if (!keep_size_and_rent_column) data$size_and_rent <- NULL
  
  return(data)
}