source('./R/getData.R')
library('dplyr')
countryOnly <- function(df){
  myList <- split(df, df$Country)
  myList <- lapply(myList, function(d){
    if(nrow(d)==1){
      res <- d
    } else{
      idx <- which(is.na(d$State))
      if (length(idx)==1){
        res <- d[idx,]
      } else {
        popIs <- sum(d$Population, na.rm = TRUE)
        res <- data.frame(City = '',
                          State = '',
                          Country = unique(d$Ccountry),
                          Population = popIs)
      }
    }
    return(res)
  })
  myList <- bind_rows(myList)
  myList <- myList %>% 
    select(-one_of('City', 'State'))
  myList$Country[myList$Country %in% c('US', 'United States')] <- 'United Stets of America'
  return(myList)  
}

popData <- getPopData()
popData <- countryOnly(popData)

assembleType2 <- function(type){
  if(type == 'cases'){
    pathGlobal <- 'time_series_covid19_confirmed_global.csv'
    pathUS <- 'time_series_covid19_confirmed_US.csv'
  } else if (type == 'deaths'){
    pathGlobal <- 'time_series_covid19_deaths_global.csv'
    pathUS <- 'time_series_covid19_deaths_US.csv'
  } else if (type == 'recovered'){
    pathGlobal <- 'time_series_covid19_recovered_global.csv'
  }
  
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
  
  myList <- split(allData, allData$Country)
  myList <- lapply(myList, function(d){
    if(nrow(d)==1){
      res <- d
    } else{
      idx <- which(is.na(d$State))
      if (length(idx)==1){
        res <- d[idx,]
      } else {
        res <- colSums(d[,seq(4,ncol(d))], na.rm = TRUE)
        res$City <- ''
        res$State <- ''
        res$Country <- unique(d$Ccountry)
      }
    }
    return(res)
  })
  myList <- bind_rows(myList)
  myList <- myList %>% 
    select(-one_of('City', 'State'))
  myList$Country[myList$Country %in% c('US', 'United States')] <- 'United Stets of America'
  colnames(myList)[seq(2,ncol(myList))] <- covid19Visualizer::getDates(colnames(myList)[seq(2,ncol(myList))])
  
  return(myList)
}

cases <- assembleType2('cases')
deaths <- assembleType2('deaths')
recovered <- assembleType2('recovered')
library('readr')
library(tidyr)
getVacc <- function(){
  vaccinations <- read_csv("covid-19-data-master/public/data/vaccinations/vaccinations.csv")
  vaccinations <- vaccinations[order(vaccinations$date), ]
  vaccinations$date <- as.character(vaccinations$date)
  vaccinations <- vaccinations %>%
    select(location, date, total_vaccinations,
    people_vaccinated, people_fully_vaccinated,
    daily_vaccinations)
  total_vaccinations <- vaccinations %>% 
    select(location, date, total_vaccinations) %>%
    pivot_wider(names_from = date, values_from = total_vaccinations)
  people_vaccinated <- vaccinations %>% 
    select(location, date, people_vaccinated) %>%
    pivot_wider(names_from = date, values_from = people_vaccinated)
  people_fully_vaccinated <- vaccinations %>% 
    select(location, date, people_fully_vaccinated) %>%
    pivot_wider(names_from = date, values_from = people_fully_vaccinated)
  allData <- bind_rows(total_vaccinations, people_vaccinated)
  allData <- bind_rows(allData, people_fully_vaccinated)
  colnames(allData)[colnames(allData)=='location'] <- 'Country'
  allData$Country[allData$Country %in% c('US', 'United States')] <- 'United Stets of America'
  return(allData)
}
vaccinations <- getVacc()
#to do
# melt all table so that dates is a column
# make sure there is a type column