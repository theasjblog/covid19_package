#' @title getMapGBQuarantine_binary
#' @description get the data to plot on a map of total number of cases
#' @param plotData (covidData): S4 object
#' @param filterByCountry (character): The countries to plot in the map. If NULL all
#' countries are included
#' @param chosenDay (numeric): the day to plot
#' @param ... not used
#' @return data frame of cases, deaths and recovered
getMapGBQuarantine_binary <- function(plotData, filterByCountry = NULL, 
                               chosenDay = NULL, ...){
  plotMetric <- 'cases'
  GBth <- 20
  df <- slot(plotData, 'JHUData_diffRaw')
  populationDf <- slot(plotData, 'populationDf')
  
  #convert the given index to the corresponding column index in the daraframe
  idxchosenDay <- getIdxChosenDay(df, chosenDay)
  #convert colnames  to date, used in the colnames of the returned results
  chosenDate <- getChosenDate(df, idxchosenDay-1)
  #get the country to plot
  if(is.null(filterByCountry)){
    filterByCountry <- as.character(unique(populationDf$Country))
  }
  # re
  populationDf <- populationDf %>% 
    filter(Country %in% filterByCountry & type == plotMetric)
  # filter out the countries we do not map
  df <- df %>% filter(ID %in% populationDf$ID)
  # sum countries
  df <- sumCountries(df, populationDf)
  #normalize by population
  df <- normalizeByPop(df)
  
  df <- sumPast7(df, idxchosenDay)
  
  #convert the values to two categories
  df[which(df[,2]<GBth),2] <- GBth-10
  df[which(df[,2]>=GBth),2] <- GBth+10
  
  #melt
  val <- melt(data = df, id.vars = c("Country"))
  val$variable <- getDates(val$variable)
  val <- val %>% filter(variable == chosenDate)
  val$Country <- countrycode(val$Country,
                             origin = 'country.name',
                             destination = 'iso3c')
  colnames(val)[colnames(val) == 'Country'] <- 'gu_a3'
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  chosenCountriesiso3c <- countrycode(filterByCountry,
                                      origin = 'country.name',
                                      destination = 'iso3c')
  world <- world %>% filter(gu_a3 %in% chosenCountriesiso3c)
  #newData <- data.frame(plotValues = values,
  #                      gu_a3 = chosenCountriesiso3c)
  # potenital issue: newData has countries not in world
  world <- merge(world, val, by  = 'gu_a3', all = TRUE)
  
  return(world)
  
}

#' @title doMapGBQuarantine_binary
#' @description plot on a map
#' @param world (sf): sf object to plot
#' @param filterByCountry (character): The countries to plot in the map. If NULL all
#' countries are included
#' @param plotMetric (character) One of "cases", "deaths", "recovered"
#' @param chosenDay (numeric): the day to plot
#' @return a ggplot
doMapGBQuarantine_binary <- function(world, filterByCountry = NULL,
                              plotMetric = 'cases',
                              chosenDay = NULL){
  
  g <- tm_shape(world) +
    tm_borders() +
    tm_fill('value', title = '',
            breaks = c(-Inf, 20, Inf),
            palette = c("blue", "red"))
  if(!is.null(filterByCountry)){
    g <- g + tm_facets(by = "name_long")
  }
  
  return(g)
}
