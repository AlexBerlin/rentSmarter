#' Extract relevant data from the 'offer_date' column.
#' 
#' @param data 
#' @return 
#' @examples
#' extractAllData(data)
extractAllData <- function(data) {
  ## Delete columns with redundant data from web crawler
  data$X_type <- NULL
  data$X_template <- NULL
  data$X_cached_page_id <- NULL
  
  ## Remove all offers without an 'id'
  data <- data[!is.na(data$id), ]
  
  data <- extractOfferDate(data)
  data <- extractSizeRent(data)
  # warnining message: 'NAs introduced by coercion' (twice)
  data <- extractInfoOneView(data) 
  data <- extractObjectInfo1(data)
  # warnining message: 'NAs introduced by coercion'
  data <- extractObjectInfo2(data) 
  data <- extractWgDetails(data)
  
  return(data)
}
