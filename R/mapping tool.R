# Leaflet for R website : https://rstudio.github.io/leaflet/
#m = leaflet(earthquake_data) %>% addTiles()
#m %>% addCircleMarkers(~lng, ~lat, radius = ~size)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#' @title Plot a leaflet map with annotations
#'
#' @description We take a subset of our earthquake data and plot this on a map
#' using leaflet. See the leaflet docs (?leaflet) for a few relevant examples.
#' The input dataframe requires columns including LONGITUDE, LATITUDE, EQ_PRIMARY.
#' The annotation column ('annot_col') is also a required argument (no defaults).
#'
#' @param earthquake_data The dataframe of data that we wish to plot
#' @param annot_col The column from our input dataframe that we'll use as an
#' annotation column
#'
#' @return Returns an interactive leaflet map that you can print to the viewer.
#'
#' @import leaflet, dplyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  map1 <-load_data() %>%
#'    eq_clean_data() %>%
#'    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(date) >= 2000) %>%
#'    eq_map(annot_col = "date")
#'  print(map1)
#' }

library(leaflet)
library(dplyr)
library(lubridate)

#' Title
#'
#' @param earthquake_data
#' @param annot_col
#'
#' @return
#' @export
#'
#' @examples
eq_map <- function(earthquake_data, annot_col){
  lmap <- earthquake_data %>% leaflet::leaflet() %>% leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lng=earthquake_data$LONGITUDE,
                              lat=earthquake_data$LATITUDE,
                              radius=earthquake_data$EQ_PRIMARY,
                              popup=earthquake_data[[annot_col]],
                              color="blue",
                              weight=1,
                              opacity=0.5)
}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#' @title Create an earthquake label column
#'
#' @description This function takes a dataframe (earthquake_data) and returns a character vector
#' of HTML column names. Note that the dataframe must have columns
#' CLEAN_LOCATION_NAME, EQ_PRIMARY and DEATHS.
#'
#' @param earthquake_data The dataframe that we'll be using to create our annotation column.
#'
#' @return Returns a character vector of earthquake details that we can use for
#' map annotations.
#'
#' @import dplyr, lubridate
#'
#' @export
#'
#' @examples
#' \dontrun{
#' map2 <-load_data() %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(date) >= 2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
#' print(map2)
#' }
eq_create_label <- function(earthquake_data){
  len <- length(earthquake_data$CLEAN_LOCATION_NAME)
  locations <- earthquake_data$CLEAN_LOCATION_NAME
  magnitude <- earthquake_data$EQ_PRIMARY
  deaths <- earthquake_data$DEATHS

  ptxt <- rep("", len)
  for(i in 1:len){
    txt <- paste0("<b>Location: </b>", locations[i], "</br>",
                  "<b>Magnitude: </b>", magnitude[i], "</br>",
                  "<b>Total Deaths: </b>", deaths[i])
    ptxt[i] <- txt
  }
  return(ptxt)
}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

map1 <-load_data() %>%
        eq_clean_data() %>%
        dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(date) >= 2000) %>%
        eq_map(annot_col = "date")
print(map1)

map2 <-load_data() %>%
        eq_clean_data() %>%
        dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(date) >= 2000) %>%
        dplyr::mutate(popup_text = eq_create_label(.)) %>%
        eq_map(annot_col = "popup_text")
print(map2)
