#' @title getNewGADM
#' @description refresh GADM map data for all needed couontries
#' @return save country rds to the data folder
#' @export
getNewGADM <- function(){
  a <- getJHU()
  aa <- a@populationDf %>% 
    filter (type == 'cases') %>%
    group_by(Country) %>%
    tally()
  
  okCountry <- NULL
  for (i in aa$Country){
    print(i)
    tryCatch({
      ic <- countrycode(sourcevar = i, origin = 'country.name', destination = 'iso3c')
      aaa <- getData("GADM",country=ic,level=0)
      okCountry <- c(okCountry, i)
      if (ic %in% c('USA', 'CAN', 'AUS')){
        aaa <- getData("GADM",country=ic,level=1)
      } 
    },error = function(e){})
  }
  
  dr <- list.files()
  dr <- dr[str_detect(dr, '.rds')]
  
  for (i in dr){
    file.copy(from = i, to = paste0('./data/', i), overwrite = TRUE)
  }
  
}


#' @title loadAllCountries
#' @description save all country polygons rds to memory
#' @return list of country polygons
#' @export
loadAllCountryes <- function(){
  
  dr <- list.files('./data')
  #remove the state level maps
  dr <- dr[str_detect(dr, '_0_')]
  #get the downloaded countries
  downCountries <- str_split(dr, '_')
  downCountries <- unlist(lapply(downCountries, function(d){
    return(d[2])
  }))
  res <- list()
  for(i in dr){
    res[[length(res)+1]] <- readRDS(paste0('./data/',i))
  }
  names(res) <- downCountries
  
  return(res)
  
}
