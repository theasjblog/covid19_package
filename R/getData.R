#' @import here

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
  cases <- read.csv2(paste0(rootData,
                            '/time_series_covid19_confirmed_global.csv'),
                     sep = ',')
  cases <- aggregate(cbind(cases[,seq(5,ncol(cases))]),
                  by=list(country=cases$Country.Region),
                  FUN=sum)
  cases$type <- 'cases'
  deaths <- read.csv2(paste0(rootData,
                                 '/time_series_covid19_deaths_global.csv'),
                      sep = ',')
  deaths <- aggregate(cbind(deaths[,seq(5,ncol(deaths))]),
                     by=list(country=deaths$Country.Region),
                     FUN=sum)
  deaths$type <- 'deaths'
  recovered <- read.csv2(paste0(rootData,
                                    '/time_series_covid19_recovered_global.csv'),
                         sep = ',')
  recovered <- aggregate(cbind(recovered[,seq(5,ncol(recovered))]),
                      by=list(country=recovered$Country.Region),
                      FUN=sum)
  recovered$type <- 'recovered'
  
  allData <- rbind(cases, deaths, recovered)
  return(allData)
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
#' or the rate of change (true)
#' @export
doPlot <- function(df, typePlot, countryPlot = NULL, scale = 'linear', plotDiff = FALSE){
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
  values <- gather(df, dates, values, 2:ncol(df))
  values$dates <- as.Date(getDates(values$dates))
  values$country <- factor(values$country)
  if(plotDiff){
    sp <- split(values, values$country)
    for(i in 1:length(sp)){
      d <- sp[[i]]
      d$values <- c(0, diff(d$values))
      sp[[i]] <- d
    }
    values <- bind_rows(sp)
  }
  
  if(scale == 'log'){
    values$values <- log10(values$values)
  }
  
  p <- ggplot(data = values, aes(x = dates,
                                 y = values,
                                 group = country)) +
    geom_line(aes(color = country)) +
    labs(x = "Date", 
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
#' @param plotDiff logical if we should plot the raw number (false)
#' or the rate of change (true)
#' @export
plotAllMetrics <- function(allDf, countryPlot, scale = 'linear', plotDiff = FALSE){
  #filter out the requested country
  df <- allDf %>%
      filter(country %in% countryPlot)
  countries <- as.character(df$country)
  df$country <- factor(countries)
  sp <- split(df, df$type)
  for (i in 1:length(sp)){
    d <- sp[[i]]
    thisType <- names(sp)[i]
    sp1 <- split(d, d$country)
    for (ii in 1:length(sp1)){
      dd <- sp1[[ii]]
      thisCountry <- dd[1,1]
      dd <- dd[,-1]
      values <- gather(dd, dates, values, 1:ncol(dd)-1)
      values$dates <- as.Date(getDates(values$dates))
      values$country <- thisCountry
      values$type <- thisType
      if (plotDiff){
        values$values <- c(0, diff(values$values))
      }
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
         title = toupper(paste(countryPlot, collapse = ', '))) +
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
                    total = df[, ncol(df)-1])
  return(res)
}



       