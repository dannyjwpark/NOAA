#library(readr)
#library(magrittr)
#library(dplyr)
#library(stringr)

### Date column: unites year, month, day
### LATITUDE & LONGITUDE: convert to numeric class
### eq_location_clean() : cleans LOCATION_NAME col by stripping out the country name (including colon)
###   and converts names to title case (from all-caps)

#----------------------------------------------------------------------------------------------
#' @title Module 1: Obtain and Clean Data
#'
#' @description Take raw data frame and return a clean data frame
#'
#' @param path A character string file path to the downloaded source.
#'
#' @return Returns a dataframe result after calling readr's read_delim
#'
#' @importFrom readr read_delim
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   earthquake_data <- load_data()
#'   print(class(earthquake_data))
#'   head(earthquake_data)
#' }
#'

load_data <- function(path = file.path("data_raw", "signif.txt")){
    earthquake_data <- read.delim(path, sep = "\t")
}
#----------------------------------------------------------------------------------------------

#' @title Create a date from NOAA data given DAY, MONTH, YEAR
#'
#' @description The NOAA data has columns called DAY, MONTH, YEAR. Part of the requirements for
#' cleaning this data is to create a date column. All of the years are provided, but some of
#' the months and days are NA. We'll replace NA days with 1st and NA months with Jan.
#' Also, note that as.Date does not play nice with negative years, so these have been removed.
#' Vectors days, months and years must be of the same length.
#'
#' @param days Day of the year as integer vector
#' @param months Month of the year as integer vector
#' @param years An integer vector of years
#'
#' @return A vector of the date objects returned from passing date strings to as.Date
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   earthquake_data <- load_data() %>% dplyr::filter(YEAR >= 0) %>%
#'        dplyr::mutate(date = get_date(DAY, MONTH, YEAR))
#'   print(class(earthquake_data))
#'   head(earthquake_data)
#' }
#'
get_date <- function(days, months, years){
  #sum(is.na(earthquake_data$YEAR))  # [1] 0
  #sum(is.na(earthquake_data$DAY))   # [1] 557
  #sum(is.na(earthquake_data$MONTH)) # [1] 405
  n <- length(days)
  dates <- seq(as.Date(Sys.Date()), by=0, len=n)
  for(i in 1:n){
    day <- days[i]
    month <- months[i]
    year <- years[i]
    if(is.na(day)){day <- 1}
    if(is.na(month)){month <- 1}
    date_str = paste(year, month, day, sep="-")
    dates[i] <- as.Date(date_str)
  }
  return(dates)
}

#---------------------------------------------------------------------------------------------
#' @name eq_clean_data
#'
#' @title Clean the raw NOAA earthquake data
#'
#' @description This function takes the raw NOAA data and cleans it
#'
#' @param earthquake_data_raw
#'
#' @return Returns a cleaned dataframe
#'
#' @importFrom dplyr filter, mutate
#'
#' @export
#'
#' @examples
#' #' \dontrun{
#'   earthquake_data <- load_data() %>% dplyr::filter(YEAR >= 0) %>%
#'        dplyr::mutate(date = get_date(DAY, MONTH, YEAR))
#'   print(class(earthquake_data))
#'   head(earthquake_data)
#' }
#'
eq_clean_data <- function(earthquake_data_raw){
  earthquake_data_raw <- earthquake_data
  earthquake_data <- earthquake_data_raw %>%
    select(col_names) %>%
    dplyr::mutate(Date =paste(YEAR, MONTH, DAY)) %>%
    dplyr::mutate(LATITUDE = as.numeric(LATITUDE), LONGITUDE = as.numeric(LONGITUDE))%>%
    eq_location_clean()
    return(earthquake_data)
}
#----------------------------------------------------------------------------------------------
#' @name eq_location_clean
#'
#' @title Location_NAME column
#'
#' @description eq_clean_data () function also needs functionality of clean LOCATION_NAME that is
#' devoid of country name
#'
#' @param earthquake_data an object data frame
#'
  #' @return Returns a cleaned dataframe as described
#'
#' @importFrom dplyr filter, mutate
#' @importFrom stringr stri_trim, str_to_title
#'
#' @export
#' @examples
#' \dontrun{
#'   earthquake_data <- load_data() %>% eq_location_clean()
#'   print(class(earthquake_data))
#'   head(earthquake_data)
#' }
#'

eq_location_clean <- function(earthquake_data){
  earthquake_data <- earthquake_data %>% dplyr::mutate(CLEAN_LOCATION_NAME = stringr::str_trim(gsub(".*:","", LOCATION_NAME))) %>%
    dplyr::mutate(CLEAN_LOCATION_NAME = stringr::str_to_title(CLEAN_LOCATION_NAME))
  return(earthquake_data)
}
