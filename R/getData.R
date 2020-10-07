#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import zoo
#' @import here
#' @import rnaturalearth
#' @import rnaturalearthdata
#' @import countrycode
#' @import rgeos
#' @import methods
#' @import stringr
#' @import raster
#' @import reshape2
#' @import tmap

#' @title getJHU
#' @description create full data for the visualizer form JHU data
#' @return data frame 
#' @export
getJHU <- function(){
  rawData <- assembleAllData()
  rawData <- getCovidDf(rawData)
  populationDf <- rawData$populationDf
  keys <- rawData$keys
  rawData <- rawData$dataDf
  
  idx <- which(colnames(rawData) != 'ID')
  smoothData <- cbind(data.frame(ID = rawData$ID), smoothDf(rawData[, idx]))
  diffRaw <- cbind(data.frame(ID = rawData$ID), diffRawDf(rawData[, idx]))
  diffSmooth <- cbind(data.frame(ID = rawData$ID), diffSmoothDf(rawData[, idx]))
  
  fullSet <- new('covidData')
  slot(fullSet, 'JHUData_raw') <- rawData
  slot(fullSet, 'JHUData_smooth') <- smoothData
  slot(fullSet, 'JHUData_diffRaw') <- diffRaw
  slot(fullSet, 'JHUData_diffSmooth') <- diffSmooth
  slot(fullSet, 'populationDf') <- populationDf
  slot(fullSet, 'keys') <- keys
  return(fullSet)
}

#' @title getCovidDf
#' @description create a covidDf object
#' @param df (data.frame) the data frame to convert to a list
#' @return list with keys, populationDf and dataDf
getCovidDf <- function(df){
  idx <- which(str_detect(colnames(df), 'X.'))[1]
  populationDf <- df[, seq(1, idx-1)]
  dataDf <- df[, seq(idx, ncol(df))]
  populationDf$ID <- seq(1, nrow(populationDf))
  dataDf$ID <- seq(1, nrow(dataDf))
  
  keys <- getKeys(populationDf)
  
  return(list(keys = keys,
              dataDf = dataDf,
              populationDf = populationDf))
  
}

#' @title getKeys
#' @description Get all the possible keys for country/state/city
#' @param populationDf (data.frame) the populationDf of the covidData object
#' @return character vector
getKeys <- function(populationDf){
  popCountry <- unique(populationDf$Country)
  popState <- populationDf %>% filter(!is.na(State))
  popState <- unique(paste(popState$Country, popState$State, sep = ', '))
  
  popCity <- populationDf %>% filter(!is.na(State)) %>% filter(!is.na(City))
  popCity <- unique(paste(popCity$Country, popCity$State, popCity$City, sep = ', '))
  
  keys <- sort(c(popCountry, popState, popCity))
  
  return(keys)
}

#' @title refreshJHU
#' @description Download JHU data if no data or data older than 1 day is present
#' @return Nothing
#' @export
refreshJHU <- function(){
  if (file.exists("JHUData-master.zip")){
    download.file(url = "https://github.com/CSSEGISandData/COVID-19/archive/master.zip"
                  , destfile = "JHUData-master.zip")
    unzip(zipfile = "JHUData-master.zip")
  } else {
    download.file(url = "https://github.com/CSSEGISandData/COVID-19/archive/master.zip"
                  , destfile = "JHUData-master.zip")
    unzip(zipfile = "JHUData-master.zip")
  }
}

#' @title assembleType
#' @description Create a dataframe for one of the COVID variables: cases, deaths or recovered
#' @param type (character) One of "cases", "deaths" or "recovered"
#' @return data frame 
#' @export
assembleType <- function(type){
  if(type == 'cases'){
    pathGlobal <- 'time_series_covid19_confirmed_global.csv'
    pathUS <- 'time_series_covid19_confirmed_US.csv'
  } else if (type == 'deaths'){
    pathGlobal <- 'time_series_covid19_deaths_global.csv'
    pathUS <- 'time_series_covid19_deaths_US.csv'
  } else if (type == 'recovered'){
    pathGlobal <- 'time_series_covid19_recovered_global.csv'
  }
  
  popData <- getPopData()
  globalData <- getGlobal(pathGlobal)
  if(type != 'recovered'){
    USData <- getUS(pathUS)
    allData <- suppressWarnings(bind_rows(USData, globalData))
  } else {
    allData <- globalData
    allData <- cbind(data.frame(City = rep(NA, nrow(allData))),
                         allData)
    allData$City <- as.character(NA)
  }
  
  if('Population' %in% colnames(allData)){
    idx <- which(colnames(allData) != 'Population')
    allData <- allData[, idx]
  }
  
  fullDataset <- full_join(popData, allData)
  fullDataset <- fullDataset[complete.cases(fullDataset[ , seq(5,ncol(fullDataset))]),]
  fullDataset <- cbind(data.frame(type = rep(type, nrow(fullDataset))),
                       fullDataset)
  
  return(fullDataset)
}

#' @title assembleAllData
#' @description create full raw data for cases/recovered/death
#' @return data frame 
assembleAllData <- function(){
  cases <- assembleType('cases')
  deaths <- assembleType('deaths')
  recovered <- assembleType('recovered')
  fullDataset <- suppressWarnings(bind_rows(cases, deaths))
  fullDataset <- suppressWarnings(bind_rows(fullDataset, recovered))
  fullDataset$Country[fullDataset$Country == 'US'] <- 'United States of America'
  
  return(fullDataset)
}

#' @title getpopData
#' @description get the population data
#' @return data frame 
getPopData <- function(){
  popData <- suppressWarnings(read.csv(here::here('COVID-19-master',
                                                  'csse_covid_19_data',
                                                  'UID_ISO_FIPS_LookUp_Table.csv')))
  popData <- popData[, colnames(popData) %in% c('Admin2',
                                                'Province_State',
                                                'Country_Region',
                                                'Population')]
  colnames(popData)[colnames(popData) == 'Admin2'] <- 'City'
  colnames(popData)[colnames(popData) == 'Province_State'] <- 'State'
  colnames(popData)[colnames(popData) == 'Country_Region'] <- 'Country'
  popData$City <- as.character(popData$City)
  popData$State <- as.character(popData$State)
  popData$Country <- as.character(popData$Country)
  popData$City[popData$City == ''] <- NA
  popData$State[popData$State == ''] <- NA
  
  return(popData)
}


#' @title getGlobal
#' @description get the global data
#' @param pathGlobal (character) the path to where the global data is
#' @return data frame 
getGlobal <- function(pathGlobal){
  globalCases <- suppressWarnings(read.csv(here::here('COVID-19-master',
                                                      'csse_covid_19_data',
                                                      'csse_covid_19_time_series',
                                                      pathGlobal)))
  colnames(globalCases)[colnames(globalCases) == 'Country.Region'] <- 'Country'
  colnames(globalCases)[colnames(globalCases) == 'Province.State'] <- 'State'
  globalCases <- globalCases[, !colnames(globalCases) %in% c('Lat', 'Long')]
  globalCases <- globalCases %>% filter (Country != 'US')
  globalCases$State <- as.character(globalCases$State)
  globalCases$Country <- as.character(globalCases$Country)
  globalCases$State[globalCases$State ==''] <- NA
  
  return(globalCases)
}

#' @title getUS
#' @description get the US data
#' @param pathGlobal (character) the path to where the US data is
#' @return data frame 
getUS <- function(pathUS){
  USCases <- suppressWarnings(read.csv(here::here('COVID-19-master',
                                                  'csse_covid_19_data',
                                                  'csse_covid_19_time_series',
                                                  pathUS)))
  colnames(USCases)[colnames(USCases) == 'Admin2'] <- 'City'
  colnames(USCases)[colnames(USCases) == 'Province_State'] <- 'State'
  colnames(USCases)[colnames(USCases) == 'Country_Region'] <- 'Country'
  USCases <- USCases[, !colnames(USCases) %in% c('UID', 'iso2', 'iso3',
                                                 'code3', 'FIPS', 'Lat', 'Long_',
                                                 'Combined_Key')]
  USCases$City <- as.character(USCases$City)
  USCases$State <- as.character(USCases$State)
  USCases$Country <- as.character(USCases$Country)
  
  return(USCases)
}


#' @title saveAllData
#' @description save a fresh allData covid object as RData
#' This is used in the app to refresh the stored data used to speed
#' up app loading
#' @return nothing
#' @export
saveAllData <- function(){
  refreshJHU()
  allData <- getJHU()
  saveRDS(allData, 'allData.rds')
}