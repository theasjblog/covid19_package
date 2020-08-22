#' @import here
#' @import rnaturalearth
#' @import rnaturalearthdata
#' @import countrycode
#' @import rgeos

#' @title refreshData
#' @description refresh data from https://github.com/CSSEGISandData/COVID-19.git
#' @return data frame of cases, deaths and recovered
#' @export
refreshData <- function(){
  download.file(url = "https://github.com/CSSEGISandData/COVID-19/archive/master.zip"
                , destfile = "JHUData-master.zip")
  unzip(zipfile = "JHUData-master.zip")
  rootData <- here::here('COVID-19-master',
                         'csse_covid_19_data',
                         'csse_covid_19_time_series')
  
  cases <- getData(rootData, 'confirmed', 'cases')
  deaths <- getData(rootData, 'deaths', 'deaths')
  recovered <- getData(rootData, 'recovered', 'recovered')
  
  allData <- rbind(cases, deaths, recovered)
  
  return(allData)
}

#' @title getData
#' @description prep data by reading in thhe downloaded repo,
#' joining coubtries with mopre than one area and aggregating
#' @return data frame of cases, deaths and recovered
getData <- function(rootData, fileName, typeIs){
  fullFile <- paste0(rootData,
                     paste0('/time_series_covid19_',
                            fileName,
                            '_global.csv')
  )
  df <- read.csv2(fullFile, sep = ',')
  df$Province.State <- as.character(df$Province.State)
  df$Country.Region <- as.character(df$Country.Region)
  df$Province.State[df$Province.State==''] <- df$Country.Region[df$Province.State=='']
  
  sp <- split(df, df$Country.Region)
  
  sp <- lapply(sp, function(d){
    if (!unique(d$Country.Region) %in% d$Province.State){
      tmpDf <- aggregate(cbind(d[,seq(5,ncol(d))]),
                         by=list(Country.Region=d$Country.Region),
                         FUN=sum)
      tmpDf$Province.State <- tmpDf$Country.Region
      tmpDf$Lat <- NA
      tmpDf$Long <- NA
      d <- rbind(d, tmpDf)
    }
    return(d)
  })
  
  df <- do.call(rbind, sp)
  df$country <- paste0(df$Country.Region, ', ', df$Province.State)
  df$country[df$Country.Region == df$Province.State] <- df$Country.Region[df$Country.Region == df$Province.State]
  
  df$type <- typeIs
  
  return(df)
}

#' @title getDates
#' @description convert JHU dates to R dates
#' @param jhuDates a vector of dates
#' @return vector of dates
getDates <- function(jhuDates){
  dates <- NULL
  for (i in jhuDates){
    tempDate <- as.character(as.Date(i, format('X%m.%d.%y')))
    dates <- c(dates, tempDate)
  }
  return(dates)
}

#' @import zoo
#' @title smoothValues
#' @description smooth time series
#' @param myVal (numeric vectro): the numbers to smooth
#' @return A numeric vector
smoothValues <- function(myVal){
  smoothed <- zoo::rollmean(myVal,21)
  meanSlope <- mean(diff(smoothed[seq(length(smoothed)-10,length(smoothed))]))
  smoothed <- c(rep(0, 10),
                smoothed,
                seq(smoothed[length(smoothed)]+meanSlope,
                    smoothed[length(smoothed)]+meanSlope*10, meanSlope))
  return(smoothed)
}
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @title doPlot
#' @description plot data for a single country
#' @param data df of data from JHU
#' @param typePlot cases, deaths or recovered
#' @param countryPlot the country to plot
#' @param scale if to plot in linear or log scale
#' @param plotDiff logical if we should plot the raw number (false)
#' @param plotLim Dates max an min limits for the plot
#' @param align logical If we should align by date of min number of cases/deaths/recovered
#' or the rate of change (true)
#' @export
doPlot <- function(df, typePlot, countryPlot = NULL, scale = 'linear',
                   plotDiff = FALSE, plotLim = NULL, align = FALSE){
  if(!typePlot %in% c('cases', 'deaths', 'recovered')){
    stop('Invalid "type"')
  }
  if(!is.null(countryPlot) && !countryPlot %in% df$country){
    stop('Invalid "country"')
  }
  
  #filter by type
  df <- df %>% 
    filter(type == typePlot)
  
  #filter out the requested country
  if (!is.null(countryPlot)){
    df <- df %>%
      filter(country %in% countryPlot)
  }
  
  df <- df[,-ncol(df)]
  if (nrow(df)==0){
    return(NULL)
  }
  
  values <- data.frame(dates = rep(colnames(df)[seq(5,ncol(df)-1)],
                                   nrow(df)))
                                 
  values$country <- rep(df$country, each=ncol(df)-5)
  res <- NULL
  for (i in 1:nrow(df)){
    res <- c(res,df[i, seq(5, ncol(df)-1)])
  }
  values$values <- as.numeric(res)
  
  values$dates <- as.Date(getDates(values$dates))
  
  values$country <- factor(values$country)
  if(plotDiff){
    sp <- split(values, values$country)
    for(i in 1:length(sp)){
      d <- sp[[i]]
      d$values <- c(0, diff(d$values))
      d$values <- smoothValues(d$values)
      sp[[i]] <- d
    }
    values <- bind_rows(sp)
  }
  
  xLabel <- "Date"
  if(align){
    xLabel <- '# days'
    th <- switch(typePlot,
                 'cases' = 100,
                 'deaths' = 50,
                 'recovered' = 100)
    sp <- split(values, values$country)
    for(i in 1:length(sp)){
      d <- sp[[i]]
      d <- d %>%
        filter(values >= th)
      d$dates <- seq(0,nrow(d)-1)
      sp[[i]] <- d
    }
    values <- bind_rows(sp)
  }
  
  
  if(scale == 'log'){
    values$values <- log10(values$values)
  }
  
  if (!is.null(plotLim)){
    values$dateAsNum <- as.numeric(values$dates)-min(as.numeric(values$dates))
    values <- values %>%
      filter(dateAsNum >= plotLim[1])
    values <- values %>%
      filter(dateAsNum <= plotLim[2])
    
  }

    p <- ggplot(data = values, aes(x = dates,
                                 y = values,
                                 group = country)) +
    geom_line(aes(color = country)) +
    labs(x = xLabel,
         y = "# cases",
         title = toupper(typePlot)) +
    theme_minimal()
  
  return(p)
  
}

#' @title plotAllMetrics
#' @description plot cases, deaths and recovered in a single plot
#' @param allDf list of all Df metrics
#' @param countryPlot character of a country
#' @param scale character for plot in linear or log scale
#' @param plotDiff logical if we should plot the raw number (false) (NOT IMPLEMETED YET)
#' #' @param plotLim Dates max an min limits for the plot (NOT IMPLEMETED YET)
#' @param align logical If we should align by date of min number of cases/deaths/recovered
#' or the rate of change (true)
#' @export
plotAllMetrics <- function(allDf, countryPlot, scale = 'linear', plotDiff = FALSE,
                            plotLim = NULL, align = FALSE){
  #filter out the requested country
  df <- allDf %>%
    filter(country %in% countryPlot)
  countries <- as.character(df$country)
  df$country <- as.character(df$country)
  sp <- split(df, df$type)
  for (i in 1:length(sp)){
    d <- sp[[i]]
    thisType <- names(sp)[i]
    sp1 <- split(d, d$country)
    for (ii in 1:length(sp1)){
      dd <- sp1[[ii]]
      thisCountry <- unique(dd$country)
      values <- data.frame(dates = rep(colnames(dd)[seq(5,ncol(dd)-2)],
                                       nrow(dd)))
      
      values$country <- rep(dd$country, each=ncol(dd)-6)
      res <- NULL
      for (iii in 1:nrow(dd)){
        res <- c(res,dd[iii, seq(5, ncol(dd)-2)])
      }
      values$values <- as.numeric(res)
      values$dates <- as.Date(getDates(values$dates))
      values$type <- thisType
      sp1[[ii]] <- values
    }
    sp[[i]] <- bind_rows(sp1)
  }
  df <- bind_rows(sp)
  countries <- as.character(df$country)
  df$country <- factor(countries)
  types <- as.character(df$type)
  df$type <- factor(types)
  
  if(scale == 'log'){
    df$values <- log10(df$values)
  }
  
  p <- ggplot(data = df, aes(x = dates,
                             y = values,
                             group = type)) +
    geom_line(aes(color = type)) +
    labs(x = "Date",
         y = "#",
         title = toupper(paste(countryPlot, collapse = ' - '))) +
    theme_minimal() +
    facet_grid(rows = vars(country))
  
  return(p)
}

#' @title getSummaryTable
#' @description summary table for all data obtained from JHU
#' @param df JHU data.frame
#' @return a summary data.frame
#' @export
getSummaryTable <- function(df){
  res <- data.frame(country = df$country,
                    type = df$type,
                    total = df[, ncol(df)-2])
  return(res)
}


#' @title getMapData
#' @description get the data to plot on a map
#' @param rawData (dataframe): the data obtaind from refresData()
#' @param normalizeByPopulation (logical): if TRUE, divide the number of cases by the
#' country population
#' @param removeCountries (character): countries to remove
#' @param filterByCountry (character): The countries to plot in the map. If NULL all
#' countries are included
#' @param chosenDay (numeric): the day to plot
#' @param showTrend (logical): colour the map by the variation of the last 5 days vs the previous
#' 5 days
#' @return data frame of cases, deaths and recovered
getMapData <- function(rawData, normalizeByPopulation = FALSE,
                       filterByCountry = NULL,
                       removeCountries = NULL,
                       chosenDay = NULL, showTrend = FALSE){
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  if(showTrend){
    chosenDay <- NULL
    normalizeByPopulation <- FALSE
  }
  if(!is.null(filterByCountry)){
    chosenCountries <- suppressWarnings(countrycode(sourcevar = filterByCountry,
                                                    origin = 'country.name',
                                                    destination = 'iso3c'))
    world <- world[world$gu_a3 %in% chosenCountries,]
    
  }
  if(!is.null(removeCountries)){
    chosenCountries <- suppressWarnings(countrycode(sourcevar = removeCountries,
                                                    origin = 'country.name',
                                                    destination = 'iso3c'))
    world <- world[!world$gu_a3 %in% chosenCountries,]
  }
  if(!is.null(chosenDay)){
    idxChosenDay <- chosenDay+4
  } else {
    idxChosenDay <- ncol(rawData)-2
  }
  world$cases <- NA
  countries <- unique(rawData$Country.Region)
  rawData$Province.State <- gsub('\\*', '', rawData$Province.State)
  for (i in 1:length(countries)){
    countries[i] <- gsub('\\*', '', countries[i])
    idxCC <- which(rawData$Province.State == countries[i])[1]
    cCode <- suppressWarnings(countrycode(sourcevar = countries[i],
                                          origin = 'country.name',
                                          destination = 'iso3c'))
    idx <- which(world$gu_a3 == cCode)
    if (length(idx) == 1){
      if(showTrend){
        smoothData <- smoothValues(diff(as.numeric(rawData[idxCC, seq(5,idxChosenDay)])))
        lastTen <- mean(smoothData[seq(idxChosenDay-10, idxChosenDay)],
                         na.rm = TRUE)
        if(mean(diff(smoothData[seq(idxChosenDay-10, idxChosenDay)]),
                na.rm = TRUE)<0)
          lastTen <- lastTen*-1
        
        world$cases[idx] <- lastTen
      } else {
        world$cases[idx] <- rawData[idxCC, idxChosenDay]
      }
    }
  }
  
  if (normalizeByPopulation){
    world$cases <- suppressWarnings(world$cases/world$pop_est)
  }
  idx <- which(!is.na(world$cases))
  world <- world[idx, ]
  
  return(world)
  
}

#' @title doData
#' @description plot on a map
#' @param rawData (dataframe): the data obtaind from refresData()
#' @param normalizeByPopulation (logical): if TRUE, divide the number of cases by the
#' country population
#' @param filterByCountry (character): The countries to plot in the map. If NULL all
#' countries are included
#' @param removeCountries (character): countries to remove
#' @param chosenDay (numeric): the day to plot
#' @param showTrend (logical): colour the map by the variation of the last 5 days vs the previous
#' 5 days
#' @return a ggplot
#' @export
doMap <- function(rawData, normalizeByPopulation = FALSE,
                  filterByCountry = NULL,
                  removeCountries = NULL,
                  chosenDay = NULL, showTrend = FALSE){
  world <- getMapData(rawData, normalizeByPopulation, filterByCountry,
                      removeCountries, chosenDay, showTrend)
  if(is.null(chosenDay)){
    chosenDay <- ncol(rawData)-6
  }
  dateChar <- as.Date(getDates(colnames(rawData[seq(5,ncol(rawData)-2)])))
  dateChar <- dateChar[chosenDay]
  if (normalizeByPopulation){
    plotTitle <- paste0('Cases normalized by population \n Day: ', dateChar)
  } else {
    plotTitle <- paste0('Cases \n Day: ', dateChar)
  }
  if (showTrend){
    plotTitle <- 'Average daily trend'
  }
  g <- ggplot(data = world) +
    geom_sf(aes(fill = cases)) +
    scale_fill_viridis_c(option = "plasma")+
    ggtitle(plotTitle) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'aliceblue'),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.ticks = element_blank()) +
    labs(fill = "")
  return(g)
}



       