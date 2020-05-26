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
  dfOR <- read.csv2(fullFile, sep = ',')
  df <- dfOR
  for (i in 1:nrow(df)){
    if (df$Province.State[i] != ''){
      df$country[i] <- paste0(df$Country.Region[i], ', ',
                           df$Province.State[i])
    } else {
      df$country[i] <- as.character(df$Country.Region[i])
    }
    
    
  }
  df <- df[, seq(5, ncol(df))]
  
  df_aggregate <- aggregate(cbind(dfOR[,seq(5,ncol(dfOR))]),
                            by=list(country=dfOR$Country.Region),
                            FUN=sum)
  df_aggregate$country <- as.character(df_aggregate$country)
  
  df <- rbind.data.frame(df_aggregate, df)
  
  df$type <- typeIs
  
  df <- df[!duplicated(df),]
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
doPlot <- function(df, typePlot, countryPlot = NULL, scale = 'linear', plotDiff = FALSE, plotLim = NULL, align = FALSE){
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
    #assume they are between 0 an 100, so need to adapt to date or values
    values$dateAsNum <- as.numeric(values$dates)-min(as.numeric(values$dates))
    values$dateAsNum <- values$dateAsNum*100/max(values$dateAsNum)
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
  #df$country <- factor(countries)
  df$country <- as.character(df$country)
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
      if (nrow(values) > 0 ){
        values$dates <- as.Date(getDates(values$dates))
      } else {
        values <- data.frame(dates = NA,
                             values = NA)
      }
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
                    total = df[, ncol(df)-1])
  return(res)
}



       