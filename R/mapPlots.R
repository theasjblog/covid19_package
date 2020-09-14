#' @title getIdxChosenDay
#' @description return the dataframe index for the chosen day
#' @param chosenDay (numeric): the day to plot
#' @param df (dataframe) JHU dataframe
#' @return numeric index
getIdxChosenDay <- function(df, chosenDay){
  if(is.null(chosenDay)){
    #subtracting to so that I can the right date by
    #adding the min date to the idxChosenDay
    idxChosenDay <- ncol(df)-1
  } else {
    idxChosenDay <- chosenDay
  }
  return(idxChosenDay)
}

#' @title getChosenDate
#' @description return character form of the day to plot given as index
#' @param idxChosenDay (numeric): index of he column with the name to covert to character date
#' @return a character date
getChosenDate <- function(df, idxchosenDay){
  idx <- which(str_detect(colnames(df), 'X'))
  dateMin <- min(getDates(colnames(df)[idx]))
  chosenDate <- as.character(as.Date(dateMin)+idxchosenDay)
  return(chosenDate)
}

#' @title sumCountriesDf
#' @description summarise the rows of a JHU dataframe by country.
#' @param df (dataframe) the JHU dataframe
#' @param populationDf (dataframe) the population dataframe from the covidData object
#' @return dataframe
sumCountries <- function(df, populationDf){
  df <- merge(df, populationDf, by='ID', all=TRUE)
  idx <- which(!colnames(df) %in% colnames(populationDf))
  b1 <- df %>% group_by(Country) %>% summarise_at(idx,sum, na.rm = TRUE)
  b2 <- df %>% group_by(Country) %>% summarise_at('Population',sum, na.rm = TRUE)
  df <- merge(b1, b2, by = 'Country', all = TRUE)
  return(df)
}

#' @title normalizeByPop
#' @description normalize data by populations
#' @param idxChosenDay (numeric): index of he column with the name to covert to character date
#' @return a dataframe
normalizeByPop <- function(df){
  idx <- which(str_detect(colnames(df), 'X.'))
  df[,idx] <- df[,idx]*100e3/df$Population
  return(df)
}

#' @title sumPast7
#' @description normalize data by populations
#' @param df (dataframe)
#' @param idxchosenDay (numeric)
#' @return a dataframe
sumPast7 <- function(df, idxchosenDay){
  pastWeek <- df[,seq(idxchosenDay-5, idxchosenDay+1)]
  nameDay <- colnames(pastWeek)[7]
  pastWeek <- rowSums(pastWeek)
  df <- data.frame(Country = df$Country,
                   X = pastWeek,
                   Population <- df$Population)
  colnames(df)[2] <- nameDay
  return(df)
}


#' @title getMapTitle
#' @description Get the map title. Used by the frontend app
#' @param world (worldMap): S4 object of class worldMap
#' @param plotType (character): one of doMapTrend_normalise, doMapTrend,
#' doMapDataRate_raw, doMapDataRate_normalised,
#' doMapGBQuarantine_binary, doMapGBQuarantine, doMapData_raw,
#' doMapData_normalised
#' @param plotMetric (character) One of "cases", "deaths", "recovered"
#' @param chosenDay (numeric): the day to plot
#' @return character string of the map title
#' @export
getMapTitle <- function(worldMap, 
                        plotMetric = 'cases',
                        chosenDay = NULL, plotType){
  world <- slot(worldMap, 'world')
  filterByCountry <- slot(worldMap, 'filterByCountry')
  datesRange <- function(world, plotMetric, normalised){
    dayStop <- unique(world$variable)
    dayStart <- as.character(as.Date(unique(world$variable))-7)
    plotTitle <- paste0('Trend of ', plotMetric,
                        '\nfrom ', dayStart, ' to ', dayStop,
                        normalised)
    return(plotTitle)
  }
  
  mapGBQ <- function(world, plotMetric){
    dayStop <- unique(world$variable)
    dayStart <- as.character(as.Date(unique(world$variable))-7)
    
    plotTitle <- paste0('Cumulative number of ', plotMetric,
                        ' from ', dayStart, ' to ', dayStop,
                        '\nnormalised every 100,000 individuals')
    return(plotTitle)
  }
  
  mapGBQ_binary <- function(world){
    dayStop <- unique(world$variable)
    dayStart <- as.character(as.Date(unique(world$variable))-7)
    GBth <- 20
    plotTitle <- paste0('GB quarantine limit = ', GBth)
  }
  
  res <- switch (plotType,
                 'doMapTrend_normalise' = datesRange(world, plotMetric, '\nnormalised every 100,000 individuals'),
                 'doMapTrend' = datesRange(world, plotMetric, NULL),
                 'doMapDataRate_raw' = paste0('New ', plotMetric, ' on day ', unique(world$variable)),
                 'doMapDataRate_normalised' = paste0('New ', plotMetric, ' on day ',
                                                     unique(world$variable),
                                                     '\nnormalised every 100,000 individuals'),
                 'doMapGBQuarantine_binary' = mapGBQ_binary(world),
                 'doMapGBQuarantine' = mapGBQ(world, plotMetric),
                 'doMapData_raw' = paste0(plotMetric, ' on day ', unique(world$variable)),
                 'doMapData_normalised' = paste0(plotMetric, ' on day ',
                                                 unique(world$variable),
                                                 '\nnormalised every 100,000 individuals')
  )
  return(res)
}

#' @title getWorld
#' @description single entry point to plot map
#' @param plotData (covidData): S4 object
#' @param plotType (character): one of doMapTrend_normalise, doMapTrend,
#' doMapDataRate_raw, doMapDataRate_normalised,
#' doMapGBQuarantine_binary, doMapGBQuarantine, doMapData_raw,
#' doMapData_normalised
#' @param filterByCountry (character): The countries to plot in the map. If NULL all
#' countries are included
#' @param plotMetric (character) One of "cases", "deaths", "recovered"
#' @param chosenDay (numeric): the day to plot
#' @return data frame of cases, deaths and recovered
#' @export
getWorld <- function(plotData, filterByCountry = NULL, 
                     plotMetric = 'cases',
                     chosenDay = NULL, plotType){
  switch (plotType,
          'doMapTrend_normalise' = allMaps(slot(plotData, 'JHUData_diffSmooth'), 
                                           slot(plotData, 'populationDf'),
                                           chosenDay, filterByCountry, plotMetric,
                                           normalizePop = TRUE, quarantinePlot = FALSE,
                                           binaryPlot = FALSE, doTrend = FALSE,
                                           GBth = NULL),
          'doMapTrend' = world <- allMaps(slot(plotData, 'JHUData_diffSmooth'),
                                          slot(plotData, 'populationDf'),
                                          chosenDay, filterByCountry, plotMetric,
                                          normalizePop = FALSE, quarantinePlot = FALSE,
                                          binaryPlot = FALSE, doTrend = FALSE,
                                          GBth = NULL),
          'doMapDataRate_raw' = allMaps(slot(plotData, 'JHUData_diffSmooth'),
                                        slot(plotData, 'populationDf'),
                                        chosenDay, filterByCountry, plotMetric,
                                        normalizePop = FALSE, quarantinePlot = FALSE,
                                        binaryPlot = FALSE, doTrend = TRUE,
                                        GBth = NULL),
          'doMapDataRate_normalised' = allMaps(slot(plotData, 'JHUData_diffSmooth'),
                                               slot(plotData, 'populationDf'),
                                               chosenDay, filterByCountry, plotMetric,
                                               normalizePop = TRUE,
                                               quarantinePlot = FALSE,
                                               binaryPlot = FALSE, doTrend = TRUE,
                                               GBth = NULL),
          'doMapGBQuarantine_binary' = allMaps(slot(plotData, 'JHUData_diffSmooth'),
                                               slot(plotData, 'populationDf'), 
                                               chosenDay, filterByCountry,
                                               plotMetric = 'cases',
                                               normalizePop = TRUE,
                                               quarantinePlot = TRUE,
                                               binaryPlot = TRUE, doTrend = FALSE,
                                               GBth = 20),
          'doMapGBQuarantine' = allMaps(slot(plotData, 'JHUData_diffSmooth'),
                                        slot(plotData, 'populationDf'),
                                        chosenDay, filterByCountry, plotMetric,
                                        normalizePop = TRUE, quarantinePlot = TRUE,
                                        binaryPlot = FALSE, doTrend = FALSE,
                                        GBth = NULL),
          'doMapData_raw' = allMaps(slot(plotData, 'JHUData_smooth'),
                                    slot(plotData, 'populationDf'),
                                    chosenDay, filterByCountry, plotMetric,
                                    normalizePop = FALSE, quarantinePlot = FALSE,
                                    binaryPlot = FALSE, doTrend = FALSE, GBth = NULL),
          'doMapData_normalised' = allMaps(slot(plotData, 'JHUData_smooth'),
                                           slot(plotData, 'populationDf'),
                                           chosenDay, filterByCountry, plotMetric,
                                           normalizePop = TRUE, quarantinePlot = FALSE,
                                           binaryPlot = FALSE, doTrend = FALSE,
                                           GBth = NULL)
  )
}

#' @title plotMap
#' @description single entry point to plot map
#' @param world (world): S4 object of class "worldMap"
#' @return tmap plot
#' @export
plotMap <- function(world){
  df <- slot(world, 'world')
  th <- slot(world, 'th')
  filterByCountry <- slot(world, 'filterByCountry')
  
  g <- tm_shape(df) +
    tm_borders() 
  
  if(!is.null(th)){
    g <- g + tm_fill('value', title = '',
                     breaks = c(-Inf, 20, Inf),
                     palette = c("blue", "red"))
  } else {
    g <- g + tm_fill('value', title = '')
  }
  
  if(!is.null(filterByCountry)){
    g <- g + tm_facets(by = "name_long")
  }
  
  return(g)
}
