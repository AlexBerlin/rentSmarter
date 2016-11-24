#' Extract relevant data from the 'wg_details' column.
#' 
#' @param data data.frame that at the least includes the 'wg_details' column.
#' @param keep_wg_details_column single logical value; whether to keep the 'wg_details' column.
#' @return A new \code{data.frame} with new columns that were extracted from the 'wg_details' column.
#' @examples
#' extractInfoOneView(data)
extractWgDetails <- function(data, keep_wg_details_column = TRUE) {
  if (!is.data.frame(data)) stop("'data' has to be a data.frame!")
  if (!"wg_details" %in% names(data)) stop("'data' needs to include the column 'wg_details'!")
  
  
  ## Extract info about the type of flat share, e.g. student or lgbt flat share
  ## Choices are from a limited selection. Multiple choices are possible.
  
  # Check presence of certain phrases in text and set TRUE or FALSE in the respective columns
  data$type_student <- stringi::stri_detect_regex(data$wg_details, "Studenten-WG")
  data$type_working <- stringi::stri_detect_regex(data$wg_details, "Berufstätigen-WG")
  data$type_necessity <- stringi::stri_detect_regex(data$wg_details, "Zweck-WG")
  data$type_non_necessity <- stringi::stri_detect_regex(data$wg_details, "keine Zweck-WG")
  data$type_women <- stringi::stri_detect_regex(data$wg_details, "Frauen-WG")
  data$type_men <- stringi::stri_detect_regex(data$wg_details, "Männer-WG")
  data$type_business <- stringi::stri_detect_regex(data$wg_details, "Business-WG")
  data$type_hostel <- stringi::stri_detect_regex(data$wg_details, "Wohnheim")
  data$type_with_kids <- stringi::stri_detect_regex(data$wg_details, "WG mit Kindern")
  data$type_fraternity <- stringi::stri_detect_regex(data$wg_details, "Verbindung")
  data$type_lgbt <- stringi::stri_detect_regex(data$wg_details, "Schwul/Lesbisch")
  data$type_multi_gen <- stringi::stri_detect_regex(data$wg_details, "Mehrgenerationen")
  data$type_senior <- stringi::stri_detect_regex(data$wg_details, "Senioren-WG")
  data$type_party <- stringi::stri_detect_regex(data$wg_details, "Party-WG")
  data$type_vegetarian <- stringi::stri_detect_regex(data$wg_details, "Vegetarisch/Vegan")
  data$type_with_aid <- stringi::stri_detect_regex(data$wg_details, "Wohnen für Hilfe")
  data$type_single_parent <- stringi::stri_detect_regex(data$wg_details, "Alleinerziehende")
  
  
  ## Extract information about languages spoken in flat
  ## Choices are from a limited selection. Multiple choices are possible.
  
  # a) Find word(s), i.e. the different languages, after the fixed phrase 'Sprache/n:'
  #    Word(s) is located at the end of the entire 'wg_details' string.
  languages <- stringi::stri_match_all_regex(data$wg_details,
                                             "Sprache/n: [\\w\\s]+")
  # b) Remove fixed phrase 'Sprache/n: ' to have one character vector which contains all spoken
  #    languages in its respective elements
  data$languages <- stringi::stri_replace_all_fixed(languages, "Sprache/n: ", "")
  # c) Remove object 'languages' 
  rm(languages)
 
  
  ## Extract numerical value for the desired min / max age, i.e. desired minimum and/or maximum age 
  ## of the new flat mate
  
  # a) The information is found after a fixed phrase
  # b) Convert matrix output to character vector
  min_max_age <- as.character(stringi::stri_match_all_regex(data$wg_details,
                                                            "zwischen [0-9]{2} und [0-9]{2}"))
  # c) The first number found in the result of b) is our min_age 
  data$looking_min_age <- as.numeric(stringi::stri_match_first_regex(min_max_age, "[0-9]{2}"))
  # d) The last number found in the result of b) is our max_age 
  data$looking_max_age <- as.numeric(stringi::stri_match_last_regex(min_max_age, "[0-9]{2}"))
  # e) remove object 'min_max_age'
  rm(min_max_age)
  
  
  ## Extract numerical value for the min / max age of the current tenants
  
  # a) The two numerical values are found in a fixed phrase
  tenants_min_max_age <- stringi::stri_match_all_regex(data$wg_details,
                                                       "Bewohneralter: [0-9]{1,2} bis [0-9]{1,2}")
  # b) The first numerical value is the minimum age
  data$looking_min_age <- as.numeric(stringi::stri_match_first_regex(tenants_min_max_age, 
                                                                    "[0-9]{1,2}"))
  # c) The last numerical value is the maximum age
  data$looking_max_age <- as.numeric(stringi::stri_match_last_regex(tenants_min_max_age, 
                                                                   "[0-9]{1,2}"))
  # d) Remove object 'tenants_min_max_age'
  
  
  ## Extract information about the desired gender of the new flatmate
  ## There are three different choices. Each can be extracted through a certain regex phrase:
  
  # man OR woman --> contains phrase "Gesucht wird: Frau oder Mann"
  data$looking_man_or_woman <- stringi::stri_detect_regex(data$wg_details, "Frau oder Mann")
  # ONLY man --> contains phrase "Gesucht wird: Mann"
  data$looking_man <- stringi::stri_detect_regex(data$wg_details, "Gesucht wird: Mann")
  # ONLY woman --> contains phrase "Gesucht wird: Frau" NOT followed by word "oder"
  data$looking_woman <- stringi::stri_detect_regex(data$wg_details, "Gesucht wird: Frau [^oder]")
  
  
  ## Extract numerical value for the amount of male and female tenants currently living in the flat
  
  # a) Extract the relevant phrase with information, e.g. "(2 Frauen und 2 Männer)"
  amount_men_women <- stringi::stri_extract_all_regex(data$wg_details, 
                                                      "\\([0-9]{1,2} \\w+ und [0-9]{1,2} \\w+\\)")
  # b) First number of 'amount_men_women' is amount women 
  data$tenants_women <- as.numeric(stringi::stri_match_first_regex(amount_men_women, "[0-9]{1,2}"))
  # c) Second (last) number of 'amount_men_women' is amount men
  data$tenants_men <- as.numeric(stringi::stri_match_last_regex(amount_men_women, "[0-9]{1,2}"))
  # d) Remove object 'amount_men_women'
  rm(amount_men_women)
  
  
  ## Extract info on smoking preferences in flat
  
  # There are four different choices
  data$smoking <- "smoking_not_specified"
  data$smoking[grep("Rauchen nicht erwünscht", data$wg_details)] <- "smoking_not_at_all"
  data$smoking[grep("Rauchen im Zimmer", data$wg_details)] <- "smoking_in_room"
  data$smoking[grep("Rauchen auf dem Balkon", data$wg_details)] <- "smoking_on_balcony"
  data$smoking[grep("Rauchen überall erlaubt", data$wg_details)] <- "smoking_everywhere"
  
  if (!keep_wg_details_column) data$wg_details <- NULL
  
  return(data)
}

