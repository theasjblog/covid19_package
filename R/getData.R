#' @import here
#' @import rnaturalearth
#' @import rnaturalearthdata
#' @import countrycode
#' @import rgeos
#' @import methods

#' @title getJHU
#' @description create full data for the visualizer form JHU data
#' @return data frame 
#' @export
getJHU <- function(){
  refreshJHU()
  rawData <- assembleAllData()
  smoothData <- smoothDf(rawData)
  diffSmooth <- diffSmoothDf(rawData)
  
  fullSet <- new('covidData')
  slot(fullSet, 'JHUData_raw') <- rawData
  slot(fullSet, 'JHUData_smooth') <- smoothData
  slot(fullSet, 'JHUData_diffSmooth') <- diffSmooth
  return(fullSet)
}

#' @title refreshJHU
#' @description Download JHU data if no data or data older than 1 day is present
#' @return Nothing
refreshJHU <- function(){
  if (file.exists("JHUData-master.zip")){
    if (as.Date(file.info("JHUData-master.zip")$atime) != Sys.Date()){
      download.file(url = "https://github.com/CSSEGISandData/COVID-19/archive/master.zip"
                    , destfile = "JHUData-master.zip")
      unzip(zipfile = "JHUData-master.zip")
    }
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
    pathUS <- 'time_series_covid19_deaths_global.csv'
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
                                                  'time_series_covid19_confirmed_US.csv')))
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
