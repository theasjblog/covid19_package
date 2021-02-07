#' @title getEventsMapDb
#' @description get the data for the map
#' @param countries (character) vector of countries to plot
#' @param metric (character) the metric to plot
#' @param date (character) the date to plot in
#' the format "YYYY-MM-DD"
#' @param normBy (character) name of the column to use in population
#' @param multiplyFactor (numeric) positive integer
#' @return normalised events data.frame
#' @export
getEventsMapDb <- function(countries, metric, date,
                           normBy, multiplyFactor){
  # connect to the database
  con <- dbConnect(RSQLite::SQLite(), "testdb")
  # query string
  countriesDf <- getAvailableCountries()
  if(is.null(countries)){
    countries <- countriesDf$Country
    iso3 <- countriesDf$iso3
  } else {
    iso3 <- countriesDf$iso3[countriesDf$Country %in% countries]
  }
  sqltxt <- sprintf("SELECT * FROM events WHERE Country IN('%s') AND variable IN('%s') AND date='%s'",
                    paste(countries, collapse = "','"),
                    paste(metric, collapse = "','"),
                    date)
  # get results from database
  res <- dbSendQuery(con, sqltxt)
  res <- dbFetch(res)
  idx <- which(countries %in% res$Country)
  countries <- countries[idx]
  iso3 <- iso3[idx]
  # disconnect from database
  dbDisconnect(con)
  # set column type
  res$date <- as.Date(res$date)
  res$value <- as.numeric(res$value)
  # normalise
  if (!is.null(normBy)){
    population <- getPopulationDb(countries)
    res <- normaliseEvents(res, population,
                           normBy,
                           multiplyFactor = 1)
  }
  
  # add the sf object for mapping plot
  world <- ne_countries(scale = "medium", returnclass = "sf")
  # this can be slow for many countries
  # chosenCountriesiso3c <- countrycode(countries,
  #                                     origin = 'country.name',
  #                                     destination = 'iso3c')
  world <- world %>% filter(gu_a3 %in% iso3)
  res$gu_a3 <- iso3
  res <- left_join(world, res, by='gu_a3')
  
  return(res)
}

#' @title getAvailableCountries
#' @describe get a character vector of countires that can
#' be plotted on the map
getAvailableCountries <- function(){
  # connect to the database
  con <- dbConnect(RSQLite::SQLite(), "testdb")
  sqltxt <- "SELECT * FROM groups"
  # get results from database
  countries <- dbSendQuery(con, sqltxt)
  countries <- dbFetch(countries)
  # disconnect from database
  dbDisconnect(con)
  
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