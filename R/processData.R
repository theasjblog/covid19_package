#' @title smoothDf
#' @description smooth a df with time series
#' @param df (data.frame) a data frame with rows of time series
#' @return smoothed data frame
smoothDf <- function(df){
  timeS <- df[,seq(6, ncol(df))]
  timeS <- apply(timeS, 1, function(x) smoothValues(as.numeric(x)))
  timeS <- t(timeS)
  df[,seq(6,ncol(df))] <- timeS
  
  return(df)
}

#' @title diffSmoothDf
#' @description diff a df with time series
#' @param df (data.frame) a data frame with rows of time series
#' @return diff'ed data frame
diffSmoothDf <- function(df){
  timeS <- df[,seq(6, ncol(df))]
  timeS <- apply(timeS, 1, function(x) smoothValues(c(0, diff(as.numeric(x)))))
  timeS <- t(timeS)
  df[,seq(6,ncol(df))] <- timeS
  
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
                      smoothed[length(smoothed)]+meanSlope*10, length.out = 10))
  return(smoothed)
}

#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @title doPlot
#' @description plot data for a single country
#' @param df df of data from JHU
#' @param typePlot cases, deaths or recovered
#' @param countryPlot the country to plot
#' @param scale if to plot in linear or log scale
#' @param plotLim Dates max an min limits for the plot
#' @param align logical If we should align by date of min number of cases/deaths/recovered
#' or the rate of change (true)
#' @export
doPlot <- function(df, typePlot, countryPlot = NULL, scale = 'linear',
                   plotLim = NULL, align = FALSE){
  if(!typePlot %in% c('cases', 'deaths', 'recovered')){
    stop('Invalid "type"')
  }
  if(!is.null(countryPlot) && !countryPlot %in% df$country){
    stop('Invalid "country"')
  }
  
  #filter by type
  df <- df %>% 
    filter(type == typePlot)
  
  #filter out the not requested country
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
#' @param align logical If we should align by date of min number of cases/deaths/recovered
#' or the rate of change (true)
#' @export
plotAllMetrics <- function(allDf, countryPlot, scale = 'linear', align = FALSE){
  #filter out the requested country
  df <- allDf %>% filter(country %in% countryPlot)
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
#' @param plotData (covidData): S4 object
#' @param normalizeByPopulation (logical): if TRUE, divide the number of cases by the
#' country population
#' @param removeCountries (character): countries to remove
#' @param filterByCountry (character): The countries to plot in the map. If NULL all
#' countries are included
#' @param chosenDay (numeric): the day to plot
#' @param showTrend (logical): colour the map by the variation of the last 5 days vs the previous
#' 5 days
#' @return data frame of cases, deaths and recovered
getMapData <- function(plotData, normalizeByPopulation = FALSE,
                       filterByCountry = NULL,
                       removeCountries = NULL,
                       chosenDay = NULL, showTrend = FALSE){
  world <- slot(plotData, 'demographic')
  rawData <- slot(plotData, 'JHUData_diffSmooth')
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
        smoothData <- as.numeric(rawData[idxCC, seq(5,idxChosenDay)])
        lastTen <- mean(diff(smoothData[seq(idxChosenDay-10, idxChosenDay-5)]),
                        na.rm = TRUE)
        meanVal <- round(mean(smoothData[seq(idxChosenDay-10, idxChosenDay)],
                        na.rm = TRUE))
        if(!is.na(meanVal)){
          if(lastTen<0){
            meanVal <- meanVal*-1
          }
          world$cases[idx] <- meanVal
        }
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
#' @param plotData (covidData): S4 object
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
doMap <- function(plotData, normalizeByPopulation = FALSE,
                  filterByCountry = NULL,
                  removeCountries = NULL,
                  chosenDay = NULL, showTrend = FALSE){
  world <- getMapData(plotData, normalizeByPopulation, filterByCountry,
                      removeCountries, chosenDay, showTrend)
  rawData <- slot(plotData, 'JHUData_diffSmooth')
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


#' @title doQMap
#' @description plot on a map the increase in number of cases over 100k subjects
#' over the past 7 days
#' @param plotData (covidData): S4 object
#' @param plotCountry (character): The countries to plot in the map. If NULL all
#' countries are included
#' @param categoricalPlot (logical): colour the countries in red if they had more than
#' 20 cases, blue otherwise
#' @return a ggplot
#' @export
doQMap <- function(plotData, plotCountry = NULL, categoricalPlot = FALSE){
  
  
  world <- slot(plotData, 'demographic')
  newData <- slot(plotData, 'JHUData_diffSmooth')
  newData <- newData %>% filter(Province.State == Country.Region & type == 'cases')
  if (!is.null(plotCountry)){
    newData <- newData %>% filter(country %in% plotCountry)
  }
  
  for (i in 1:nrow(newData)){
    newData$increase[i] <- suppressWarnings(sum(as.numeric(
      newData[i, seq(ncol(newData)-9, ncol(newData)-2)]), na.rm = TRUE))
  }
  
  newData$countryCode <- suppressWarnings(countrycode(sourcevar = newData$country,
                                                      origin = 'country.name',
                                                      destination = 'iso3c'))
  world <- world[world$gu_a3 %in% newData$countryCode,]
  for (i in 1:nrow(world)){
    idx <- which(newData$countryCode == world$gu_a3[i])
    world$increase[i] <- round(newData$increase[idx]*100e3/world$pop_est[i])
  }
  
  
  g <- plotQMap(world, categoricalPlot)
  
  return(g)
  
}


#' @title plotQMap
#' @description plot on a map the increase in number of cases over 100k subjects
#' over the past 7 days
#' @param world (data.frame): The countries with population data, from the package rnaturalearthdata
#' countries are included
#' @param categoricalPlot (logical): colour the countries in red if they had more than
#' 20 cases, blue otherwise
#' @return a ggplot
plotQMap <- function(world, categoricalPlot){
  if (categoricalPlot){
    world$increase[world$increase>=20] <- 30
    world$increase[world$increase<20] <- 10
  }
  
  plotTitle <- 'Number of cases every 100K subjects \nover the past 7 days'
  g <- ggplot(data = world) +
    geom_sf(aes(fill = increase)) +
    ggtitle(plotTitle) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'aliceblue'),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.ticks = element_blank()) +
    labs(fill = "")
  
  if(categoricalPlot){
    g <- g + 
      scale_fill_gradient2(low = "blue",
                           mid = "white",
                           high = "red",
                           midpoint = 20,
                           limits = c(10, 30))+
      theme(legend.position = "none")
  } else {
    g <- g + scale_fill_viridis_c(option = "plasma")
  }
  
  return(g)
}
