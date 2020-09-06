#' @title getMapData_normalised
#' @description get the data to plot on a map of total number of cases every 100e3
#' people
#' @param plotData (covidData): S4 object
#' @param filterByCountry (character): The countries to plot in the map. If NULL all
#' countries are included
#' @param plotMetric (character) One of "cases", "deaths", "recovered"
#' @param chosenDay (numeric): the day to plot
#' @return data frame of cases, deaths and recovered
getMapData_normalised <- function(plotData, filterByCountry = NULL, 
                           plotMetric = 'cases',
                           chosenDay = NULL){
  
  df <- slot(plotData, 'JHUData_smooth')
  
  if(is.null(chosenDay)){
    #subtracting to so that I can the right date by
    #adding the min date to the idxChosenDay
    idxChosenDay <- ncol(df)-2
  } else {
    idxChosenDay <- chosenDay - 1
  }
  
  idx <- which(str_detect(colnames(df), 'X'))
  dateMin <- min(getDates(colnames(df)[idx]))
  chosenDate <- as.character(as.Date(dateMin)+idxChosenDay)
  
  #get the country to plot
  populationDf <- slot(plotData, 'populationDf')
  if(is.null(filterByCountry)){
    filterByCountry <- as.character(unique(populationDf$Country))
  }
  populationDf <- populationDf %>% 
    filter(Country %in% filterByCountry & type == plotMetric)
  
  df <- df %>% filter(ID %in% populationDf$ID)
  # sum countries
  df <- merge(df, populationDf, by='ID', all=TRUE)
  sp <- split(df, df$Country)
  sp <- lapply(sp, function(d){
    idx <- which(str_detect(colnames(d), 'X'))
    totoPop <- sum(d$Population, na.rm = TRUE)
    res <- as.data.frame(t(round(colSums(d[,idx])/totoPop*100e3)))
    res$Country <- unique(d$Country)
    return(res)
  })
  df <- bind_rows(sp)
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

#' @title doMapData_normalised
#' @description plot on a map
#' @param world (sf): sf object to plot
#' @param filterByCountry (character): The countries to plot in the map. If NULL all
#' countries are included
#' @param plotMetric (character) One of "cases", "deaths", "recovered"
#' @param chosenDay (numeric): the day to plot
#' @return a ggplot
doMapData_normalised <- function(world, filterByCountry = NULL,
                          plotMetric = 'cases',
                          chosenDay = NULL){
  
  g <- tm_shape(world) +
    tm_borders() +
    tm_fill('value', title = '')
  
  if(!is.null(filterByCountry)){
    g <- g + tm_facets(by = "name_long")
  }
  
  return(g)
}

