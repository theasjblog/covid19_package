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

#' @title createConnection
#' @description connect to the demo database
#' @return DBI database connection
#' @export
createConnection <- function(){
  con <- dbConnect(RSQLite::SQLite(), "testdb")
  
  return(con)
}

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
  allData$Country[allData$Country=='European Union'] <- 'Europe'
  allData$date <- as.character(allData$date)
  allData <- as.data.frame(allData)
  
  return(allData)
}

#' @title getGroups
#' @description get groups of countries, i.e. europe vs oceania etc
#' @param df (data.frame) from getAllData()
#' @return data frame
getGroups <- function(df){
  world <- ne_countries(scale = "medium", returnclass = "sf")
  nonStandard <- c("International","Micronesia (country)",
                   "Timor", 'Kosovo')
  areGroups <- c('Africa', 'Asia', 'Europe', 'North America', 
                 'Oceania', 'South America', 'World')
  df <- df %>% filter(!Country %in% nonStandard)
  
  allCountries <- unique(df$Country[!df$Country %in% areGroups])
  iso3 <- countrycode(allCountries,
                      origin = 'country.name',
                      destination = 'iso3c')
  
  
  idx <- which(!is.na(iso3) & iso3 %in% world$gu_a3)
  allCountries <- allCountries[idx]
  iso3 <- iso3[idx]
  
  allCountries <- data.frame(Country = allCountries,
                             iso3 = iso3)
  
  world <- data.frame(iso3 = world$gu_a3,
                      continent = world$continent)
  res <- full_join(allCountries, world, by = 'iso3')
  res <- res %>%
    tidyr::pivot_longer(
      cols = c('continent'),
      names_to = "cols",
      values_to = "groups")
  res <- res[,-which(colnames(res)=='cols')]
  we <- res %>% dplyr::select(Country, iso3) %>%
    distinct()
  we$groups <- 'World'
  res <- bind_rows(res, we)
  
  return(res)
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
  populationData <- melt(populationData, id='Country')
  
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

#' @title removeGroups
#' @description remove entries like asia, africa,
#' europe etc from the list of countries
#' @param allData (dataframe) from getAllData()
#' @param groups (dataframe) from
removeGroups <- function(allData, groups){
  allData <- allData %>% 
    filter(!Country %in% groups$groups)
  return(allData)
}


#' @title updateDb
#' @description fetch new data from 'https://covid.ourworldindata.org/data/owid-covid-data.xlsx'
#' and update the database
#' @param con (DBI)
#' @return nothing 
#' @export 
updateDb <- function(con){
  # get the data from the web
  allData <- getAllData()
  # get groups
  groups <- getGroups(allData)
  # remove groups from allData
  allData <- removeGroups(allData, groups)
  # get population data
  populationData <- getPopulation(allData)
  # get events data
  eventsData <- getEvents(allData)
  # remove nas
  eventsData <- fillNAs(eventsData)
  # add to the database the mappable countries
  dbWriteTable(con, "groups",
               groups, overwrite = TRUE)
  # add to the database the population data
  dbWriteTable(con, "population",
               populationData, overwrite = TRUE)
  # add to the database the events data
  dbWriteTable(con, "events",
               eventsData, overwrite = TRUE)
}

#' @title getEventsDb
#' @description get the events data from the database
#' @param con (DBI) database connection
#' @param groups (character)
#' @param countries (character)
#' @param date (character)
#' @param metrics (character)
#' @return dataframe
#' @export
getEventsDb  <- function(con, groups, countries, date, metrics){
  if(!is.null(groups)){
    # use either countries or groups
    countries <- getCountriesFromGroups(con, groups)$Country
  }
  # query string
  sqltxt <- sprintf("SELECT * FROM events WHERE Country IN('%s') AND variable IN('%s')",
                    paste(countries, collapse = "','"),
                    paste(metrics, collapse = "','"))
  if (!is.null(date)){
    sqltxt <- paste0(sqltxt,
                     sprintf(" AND date IN('%s')"),
                     date)
  }
  # get results from database
  res <- dbSendQuery(con, sqltxt)
  res <- dbFetch(res)
  # set column type
  res$date <- as.Date(res$date)
  res$value <- as.numeric(res$value)
  
  return(res)
}


#' @title getPopulationDb
#' @description retrive population data from the database
#' @param con (DBI) database connection
#' @param groups (character) a geographical group of countries
#' @param countries (character) vector of countries to retrive
#' @return population data frame 
#' @export 
getPopulationDb <- function(con, groups, countries){
  if(!is.null(groups)){
    # use either countries or groups
    countries <- getCountriesFromGroups(con, groups)$Country
  }
  # query string
  sqltxt <- sprintf("SELECT * FROM population WHERE Country IN('%s')",
                    paste(countries, collapse = "','"))
  # get results from database
  res <- dbSendQuery(con, sqltxt)
  res <- dbFetch(res)
  
  return(res)
}

#' @title aggregateCountries
#' @description aggregate countries by group
#' @param df (dataframe) events or population
#' @param con (DBI)
#' @param groups (dataframe) a country-group mapping dataframe
#' @export
aggregateCountries <- function(con, df, groups){
  
  countries <- getCountriesFromGroups(con, groups)
  df <- left_join(df, countries, by='Country')
  df$Country <- df$groups
  
  if ('date' %in% colnames(df)){
    a <- df %>% 
      group_by(Country,variable, date)
    toMean <- c('positive_rate',
                'reproduction_rate',
                'tests_per_case')
    meanDf <- a[which(a$variable %in% toMean),]
    sumDf <- a[which(!a$variable %in% toMean),]
    if (nrow(meanDf)>0){
      meanDf <- meanDf %>%
        summarize(mean(value, na.rm = TRUE))
      colnames(meanDf)[colnames(meanDf)=="mean(value, na.rm = TRUE)"] <- 'value'
    }
    if (nrow(sumDf)>0){
      sumDf <- sumDf %>%
        summarize(sum(value, na.rm = TRUE))
      colnames(sumDf)[colnames(sumDf)=="sum(value, na.rm = TRUE)"] <- 'value'
    }
    
    a <- bind_rows(meanDf, sumDf)
    
  } else {
    # population df
    a <- df %>% 
      group_by(Country,variable)
    toMean <- c("population_density",
                "median_age",
                "aged_65_older",
                "aged_70_older",
                "gdp_per_capita",
                "extreme_poverty",
                "cardiovasc_death_rate",
                "diabetes_prevalence",
                "female_smokers",
                "male_smokers",
                "hospital_beds_per_thousand",
                "life_expectancy",
                "human_development_index")
    meanDf <- a[which(a$variable %in% toMean),]
    sumDf <- a[which(!a$variable %in% toMean),]
    meanDf <- meanDf %>%
      summarize(mean(value, na.rm = TRUE))
    colnames(meanDf)[colnames(meanDf)=="mean(value, na.rm = TRUE)"] <- 'value'
    sumDf <- sumDf %>%
      summarize(sum(value, na.rm = TRUE))
    colnames(sumDf)[colnames(sumDf)=="sum(value, na.rm = TRUE)"] <- 'value'
    a <- bind_rows(meanDf, sumDf)
  }
  
  return(a)
}

#' @title getCountriesFromGroups
#' @description map groups to countries
#' @param con (DBI)
#' @param groups (character) vector of groups to retrive
#' @return data frame
#' @export
getCountriesFromGroups <- function(con, groups){
  sqltxt <- sprintf("SELECT * FROM groups WHERE groups IN('%s')",
                    paste0(groups, collapse = "','"))
  # get results from database
  countries <- dbSendQuery(con, sqltxt)
  countries <- dbFetch(countries)
  
  return(countries)
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
#' @export
normaliseEvents <- function(events, population, normBy, multiplyFactor = 1){
  # split by country
  sp <- split(events, events$Country)
  res <- lapply(sp, function(d){
    # get country
    thisCountry <- unique(d$Country)
    # get normalization value
    thisNormVal <- population$value[population$variable == normBy & population$Country==thisCountry]
    # normalise
    d$value <- multiplyFactor*d$value/thisNormVal
    return(d)
  })
  res <- bind_rows(res)
  
  return(res)
}

#' @title getOptions
#' @description get all the unique values in the specified
#' field of the specified table
#' @param table (character) the name of the database table
#' @param field (character) the name of the field in the table
#' @param con (DBI)
#' @return a character vector
#' @export
getOptions <- function(con, table, field){
  
  # query string
  sqltxt <- sprintf("SELECT DISTINCT %s FROM %s",
                    field,
                    table)
  # get results from database
  res <- dbSendQuery(con, sqltxt)
  res <- dbFetch(res)
  res <- res[,1]
  
  return(res)
}


#' @title getLastDate
#' @description get the latest date in the dataset
#' @param con (DBI)
#' @return a character
#' @export
getLastDate <- function(con){
  
  # query string
  sqltxt <- "SELECT DISTINCT date FROM events"
  # get results from database
  res <- dbSendQuery(con, sqltxt)
  res <- dbFetch(res)
  res <- res[,1]
  res <- as.character(max(as.Date(res)))
  
  return(res)
}

#' @title displayEvents
#' @description format events table to be displayed in the app
#' @param events (dataframe)
#' @return dataframe
#' @export
displayEvents <- function(events){
  events <- events %>%
    group_by(Country) %>%
    # only display the latest date
    filter(date == max(date))
  return(events)
}

#' @title displayPopulation
#' @description format population table to be displayed in the app
#' @param events (dataframe)
#' @param cols (character) variables to be included. If NULL,
#' all variables will be included
#' @return dataframe
#' @export
displayPopulation <- function(population, cols){
  if(is.null(cols)){
    cols <- unique(population$variable)
  }
  population <- population %>%
    filter(variable %in% cols)
  
  return(population)
}