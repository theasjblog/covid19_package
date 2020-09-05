#' @title getNewGADM
#' @description refresh GADM map data for all needed couontries
#' @param dataObj (coviData)
#' @return save country rds to the data folder
#' @export
getNewGADM <- function(dataObj){
  aa <- dataObj@populationDf %>% 
    filter (type == 'cases') %>%
    group_by(Country) %>%
    tally()
  
  okCountry <- NULL
  for (i in aa$Country){
    tryCatch({
      ic <- countrycode(sourcevar = i, origin = 'country.name', destination = 'iso3c')
      aaa <- raster::getData("GADM",country=ic,level=0)
      okCountry <- c(okCountry, i)
      if (ic %in% c('USA', 'CAN', 'AUS')){
        aaa <- getData("GADM",country=ic,level=1)
      }
    },error = function(e){print(e)})
  }
  
  dr <- list.files()
  dr <- dr[str_detect(dr, '.rds')]
  
  if(!dir.exists('./data')){
    dir.create('./data')
  }
  for (i in dr){
    file.copy(from = i, to = paste0('./data/', i), overwrite = TRUE)
    file.remove(i)
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
