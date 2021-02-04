library('dplyr')
library('data.table')
library('DBI')
library('readxl')

getAllData <- function(){
  #load pre-curated data
  #download.file(url='https://covid.ourworldindata.org/data/owid-covid-data.xlsx',
  #              destfile='owid-covid-data.xlsx')
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
  colnames(allData)[colnames(allData)=='location'] <- 'Country'
  allData$date <- as.character(allData$date)
  return(allData)
}

getPopulation <- function(allData){
  populationData <- allData %>%
    select(Country,
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
  
  populationData <- split(populationData, populationData$Country)
  populationData <- lapply(populationData, function(d){
    d <- d %>% distinct()
    d
  })
  populationData <- bind_rows(populationData)
  return(populationData)
}

getEvents <- function(allData){
  eventsData <- allData %>%
    select(Country,
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
  eventsData <- melt(eventsData, id=seq(1,2),
                     measure=seq(3,ncol(eventsData)))
  return(eventsData)
}

updateDb <- function(){
  
  allData <- getAllData()
  populationData <- getPopulation(allData)
  eventsData <- getEvents(allData)
  con <- dbConnect(RSQLite::SQLite(), "testdb")
  
  dbWriteTable(con, "population",
               populationData, overwrite = TRUE)
  dbWriteTable(con, "events",
               eventsData, overwrite = TRUE)
  
  dbDisconnect(con)
  
}

updateDb()

getEventsDb <- function(countries, metrics){
  con <- dbConnect(RSQLite::SQLite(), "testdb")
  sqltxt <- sprintf("SELECT * FROM events WHERE Country IN('%s') AND variable IN('%s')",
                   paste(countries, collapse = "','"),
                   paste(metrics, collapse = "','"))
                   
  res <- dbSendQuery(con, sqltxt)
  res <- dbFetch(res)
  res$date <- as.Date(res$date)
  dbDisconnect(con)
  return(res)
}

getPopulationDb <- function(countries){
  con <- dbConnect(RSQLite::SQLite(), "testdb")
  sqltxt <- sprintf("SELECT * FROM population WHERE Country IN('%s')",
                    paste(countries, collapse = "','"))
  
  res <- dbSendQuery(con, sqltxt)
  res <- dbFetch(res)
  dbDisconnect(con)
  return(res)
}

events <- getEventsDb(c('Canada', 'Italy'),
                      c('total_cases', 'total_vaccinations'))
population <- getPopulationDb(c('Canada', 'Italy'))

# to do
#plotting function