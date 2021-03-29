#' @title getMapData
#' @description get the data for the map plot
#' @param events (dataframe)
#' @param con (DBI)
#' @return dataframe
#' @export
getMapData <- function(con, events){
  # population data does not have date
  # with this we add a column to population
  # so we can use it in this function
  if(!'date' %in% colnames(events)){
    events$date <- 1
  }
  events <- events %>%
    group_by(Country) %>%
    filter(date == max(date))
  countries <- unique(events$Country)
  countriesDf <- getAvailableCountries(con)
  countriesDf <- countriesDf %>%
    dplyr::select(-one_of('groups')) %>%
    filter(Country %in% countries) %>%
    distinct()
  events <- left_join(events, countriesDf, by='Country')
  colnames(events)[colnames(events) == 'iso3'] <- 'gu_a3'
  
  
  # add the sf object for mapping plot
  world <- ne_countries(scale = "medium", returnclass = "sf")
  # this can be slow for many countries
  # chosenCountriesiso3c <- countrycode(countries,
  #                                     origin = 'country.name',
  #                                     destination = 'iso3c')
  world <- world %>% filter(gu_a3 %in% events$gu_a3)
  events <- left_join(world, events, by='gu_a3')
  
  return(events)
}


#' @title getAvailableCountries
#' @describe get a character vector of countires that can
#' be plotted on the map
#' @param con (DBI)
#' @return dataframe
getAvailableCountries <- function(con){
  sqltxt <- "SELECT * FROM groups"
  # get results from database
  countries <- dbSendQuery(con, sqltxt)
  countries <- dbFetch(countries)
  
  return(countries)
}

#' @title doMap
#' @description plot in amp chart
#' @param df (data.frame) the dataframe from getEventsMapDb()
#' @param doFacet (logical) TRUE if faceting the plot
#' @return tmap object
#' @export
doMap <- function(df, doFacet){
  g <- tm_shape(df) +
    tm_borders() 
  
  g <- g + tm_fill('value', title = '')
  
  if(doFacet){
    g <- g + tm_facets(by = "name_long")
  }
  
  return(g)
}