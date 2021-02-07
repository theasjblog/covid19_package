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
#' @import data.table
#' @import DBI
#' @import readxl
#' @import ggplot2

#' @title getAllData
#' @description download data from 'https://covid.ourworldindata.org/data/owid-covid-data.xlsx'
#' and import it as a dataframe 
#' @return data frame 
getAllData <- function(){
  # download pre-curated data
  download.file(url='https://covid.ourworldindata.org/data/owid-covid-data.xlsx',
                destfile='owid-covid-data.xlsx', mode = 'wb')
  # read the file
  allData <- read_excel("owid-covid-data.xlsx",
                        col_types = c("text", "text", "text",
                                      "text", "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "text", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric"))
  # tidy up
  colnames(allData)[colnames(allData)=='location'] <- 'Country'
  allData$Country[allData$Country=="Cote d'Ivoire"] <- 'Ivory Coast'
  allData$date <- as.character(allData$date)
  allData <- as.data.frame(allData)
  
  return(allData)
}

#' @title getgetAreasForMapPopulation
#' @description remove aggregated areas from the map as we plot
#' only at country level 
#' @param possibleCountries (character) vector of getAllData()$Country
#' @return character vector of countries only 
getAreasForMap <- function(possibleCountries){
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  allCountries <- unique(possibleCountries)
  iso3 <- countrycode(allCountries,
                      origin = 'country.name',
                      destination = 'iso3c')
  
  idx <- which(!is.na(iso3) & iso3 %in% world$gu_a3)
  allCountries <- allCountries[idx]
  iso3 <- iso3[idx]
  allCountries <- data.frame(Country = allCountries,
                             iso3 = iso3)
  
  return(allCountries)
}

#' @title getPopulation
#' @description prepare the population table for the database 
#' @param allData (dataframe) obtained from getAllData()
#' @return data frame 
getPopulation <- function(allData){
  # select column with country metrics that do not change
  # with the time series
  populationData <- allData %>%
    dplyr::select(Country,
           population,
           population_density,
           median_age,
           aged_65_older,
           aged_70_older,
           gdp_per_capita,
           extreme_poverty,
           cardiovasc_death_rate,
           diabetes_prevalence,
           female_smokers,
           male_smokers,
           handwashing_facilities,
           hospital_beds_per_thousand,
           life_expectancy,
           human_development_index)
  
  # remove duplicates
  populationData <- split(populationData, populationData$Country)
  populationData <- lapply(populationData, function(d){
    d <- d %>% distinct()
    d
  })
  populationData <- bind_rows(populationData)
  
  return(populationData)
}

#' @title getEvents
#' @description prepare the events table for the database 
#' @param allData (dataframe) obtained from getAllData()
#' @return data frame 
getEvents <- function(allData){
  # select column with time-related events
  eventsData <- allData %>%
    dplyr::select(Country,
           date,
           total_cases,
           new_cases,
           new_cases_smoothed,
           total_deaths,
           new_deaths,
           new_deaths_smoothed,
           reproduction_rate,
           icu_patients,
           hosp_patients,
           weekly_icu_admissions,
           weekly_hosp_admissions,
           total_tests,
           new_tests,
           new_tests_smoothed,
           positive_rate,
           tests_per_case,
           tests_units,
           total_vaccinations,
           people_vaccinated,
           people_fully_vaccinated,
           new_vaccinations,
           new_vaccinations_smoothed)
  
  # make the table long
  eventsData <- melt(eventsData, id=seq(1,2),
                     measure=seq(3,ncol(eventsData)))
  eventsData$value <- as.numeric(eventsData$value)
  
  return(eventsData)
}


#' @title updateDb
#' @description fetch new data from 'https://covid.ourworldindata.org/data/owid-covid-data.xlsx'
#' and update the database
#' @return nothing 
#' @export 
updateDb <- function(){
  # get the data from the web
  allData <- getAllData()
  # get the countries that can be mapped
  mappableCountries <- getAreasForMap(allData$Country)
  # get population data
  populationData <- getPopulation(allData)
  # get events data
  eventsData <- getEvents(allData)
  # remove nas
  eventsData <- fillNAs(eventsData)
  # connect to the database
  con <- dbConnect(RSQLite::SQLite(), "testdb")
  # add to the database the mappable countries
  dbWriteTable(con, "mappable",
               mappableCountries, overwrite = TRUE)
  # add to the database the population data
  dbWriteTable(con, "population",
               populationData, overwrite = TRUE)
  # add to the database the events data
  dbWriteTable(con, "events",
               eventsData, overwrite = TRUE)
  # close database connection
  dbDisconnect(con)
}

#' @title getEventsDb
#' @description retrive events from the database
#' @param countries (character) vector of countries to retrive
#' @param metrics (character) vector of metrics to retrive
#' @return events data frame 
#' @export 
getEventsDb <- function(countries, metrics){
  # connect to the database
  con <- dbConnect(RSQLite::SQLite(), "testdb")
  # query string
  sqltxt <- sprintf("SELECT * FROM events WHERE Country IN('%s') AND variable IN('%s')",
                    paste(countries, collapse = "','"),
                    paste(metrics, collapse = "','"))
  # get results from database
  res <- dbSendQuery(con, sqltxt)
  res <- dbFetch(res)
  # set column type
  res$date <- as.Date(res$date)
  res$value <- as.numeric(res$value)
  # disconnect from database
  dbDisconnect(con)
  
  return(res)
}

#' @title getPopulationDb
#' @description retrive population data from the database
#' @param countries (character) vector of countries to retrive
#' @return population data frame 
#' @export 
getPopulationDb <- function(countries){
  # connect to the database
  con <- dbConnect(RSQLite::SQLite(), "testdb")
  if(is.null(countries)){
    sqltxt <- "SELECT DISTINCT Country FROM population"
    # get results from database
    countries <- dbSendQuery(con, sqltxt)
    countries <- dbFetch(countries)$Country
    
  }
  # query string
  sqltxt <- sprintf("SELECT * FROM population WHERE Country IN('%s')",
                    paste(countries, collapse = "','"))
  # get results from database
  res <- dbSendQuery(con, sqltxt)
  res <- dbFetch(res)
  # close connection
  dbDisconnect(con)
  
  return(res)
}

#' @title fillNAs
#' @description replace NAs in numerical columns. Used with the events data.frame. NAS
#' are replacing using zoo::na.approx. Leading NAs are replaced
#' with 0s, trailing NAs are replaced with the last non-NA value.
#' @param df (data.frame) dataframe
#' @return population data frame 
fillNAs <- function(df){
  # make sure that the df is ordered by date
  df <- df[order(df$date),]
  df$value <- as.numeric(df$value)
  # split by country
  sp <- split(df, df$Country)
  res <- lapply(sp, function(d){
    # split by variable
    spInt <- split(d, d$variable)
    resInt <- lapply(spInt, function(dInt){
      if(all(is.na(dInt$value))){
        newVal <- rep(0, nrow(dInt))
      } else {
        if (sum(is.na(dInt$value))==1){
          newVal <- dInt$value
        } else {
          # replace inner NAs
          newVal <- na.approx(dInt$value, na.rm=FALSE)
        }
      }
      # check if we still have nas
      idx <- which(!is.na(newVal))
      if (length(idx) > 0 & length(idx) < nrow(dInt)){
        if(idx[1]!=1){
          # replace leading nas
          newVal[seq(1,idx[1])] <- 0
        }
        if(idx[length(idx)]!=nrow(dInt)){
          # replace trailing nas
          newVal[seq(idx[length(idx)]+1, nrow(dInt))] <- newVal[idx[length(idx)]]
        }
      }
      dInt$value <- newVal
      return(dInt)
    })
    resInt <- bind_rows(resInt)
    return(resInt)
  })
  res <- bind_rows(res)
  
  return(res)
}

#' @title normaliseEvents
#' @description divide the value in the events by the specified
#' normBy and multiply by a factor. For instance to normalise the data
#' by every 100 inhabitants, set normBy='population' and
#' multiplyFactor=100
#' @param events (data.frame) events dataframe
#' @param population (data.frame) population dataframe
#' @param normBy (character) name of the column to use in population
#' @param multiplyFactor (numeric) positive integer
#' @return normalised events data.frame
normaliseEvents <- function(events, population, normBy, multiplyFactor = 1){
  # split by country
  sp <- split(events, events$Country)
  res <- lapply(sp, function(d){
    # get country
    thisCountry <- unique(d$Country)
    # get normalization value
    thisNormVal <- population[[normBy]][population$Country==thisCountry]
    # normalise
    d$value <- multiplyFactor*d$value/thisNormVal
    return(d)
  })
  res <- bind_rows(res)
  
  return(res)
}
