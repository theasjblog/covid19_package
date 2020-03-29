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
  cases$type <- 'cases'
  deaths <- read.csv2(paste0(rootData,
                                 '/time_series_covid19_deaths_global.csv'),
                      sep = ',')
  deaths$type <- 'deaths'
  recovered <- read.csv2(paste0(rootData,
                                    '/time_series_covid19_recovered_global.csv'),
                         sep = ',')
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
#' @param country the country to plot
#' @export
doPlot <- function(df, typePlot, country = NULL){
  if(!typePlot %in% c('cases', 'deaths', 'recovered')){
    stop('Invalid "type"')
  }
  if(!is.null(country) && !country %in% df$Country.Region){
    stop('Invalid "country"')
  }
  
  #filter by type
  df <- df %>% 
    filter(type == typePlot)
  
  #filter out the requested country
  if (!is.null(country)){
    df <- df %>%
      filter(Country.Region %in% country)
  }
  #summarise countries with multiple regions
  df <- aggregate(cbind(df[,seq(5,ncol(df)-1)]),
                    by=list(country=df$Country.Region),
                    FUN=sum)
  
  values <- gather(df, dates, values, 2:ncol(df))
  values$dates <- as.Date(getDates(values$dates))
  values$country <- factor(values$country)
  values <- values[order(values$country),]
  
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
#' @param country character of a country
#' @export
plotAllMetrics <- function(allDf, country){
  #filter out the requested country
  df <- allDf %>%
      filter(Country.Region %in% country)
  
  sp <- split(df, df$type)
  for (i in 1:length(sp)){
    d <- sp[[i]]
    thisType <- names(sp)[i]
    #summarise countries with multiple regions
    d <- aggregate(cbind(d[,seq(5,ncol(d)-1)]),
                    by=list(country=d$Country.Region),
                    FUN=sum)
    values <- gather(d, dates, values, 2:ncol(d))
    values$dates <- as.Date(getDates(values$dates))
    values$country <- factor(values$country)
    values <- values[order(values$country),]
    values$type <- thisType
    sp[[i]] <- values
  }
  df <- bind_rows(sp)
  
  p <- ggplot(data = df, aes(x = dates,
                             y = values,
                             group = type)) +
    geom_line(aes(color = type)) +
    labs(x = "Date", 
         y = "#",
         title = toupper(paste(country, collapse = ', '))) +
    theme_minimal() +
  facet_grid(rows = vars(country))
  
  return(p)
}



       