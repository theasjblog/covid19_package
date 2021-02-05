#' @title getPlotData
#' @description get plot data for the events plot
#' @param events (data.frame) events dataframe
#' @param population (data.frame) population dataframe
#' @param normBy (character) name of the column to use in population
#' @param multiplyFactor (numeric) positive integer
#' @return events data frame ready for plot 
getPlotData <- function(events, population, normBy=NULL, multiplyFactor = 1){
  # fill nas
  events <- fillNAs(events)
  # add text for plotly
  events$text <- paste0('Date: ', as.character(events$date), '\n',
                        'Country: ', events$Country, '\n',
                        str_to_title(events$variable), ': ', events$value)
  # do we normalise?
  if(!is.null(normBy)){
    # normalise
    events <- normaliseEvents(events, population,
                              normBy, multiplyFactor)
    # if we normalise, use that info as plot title
    events$normalisation <- paste0('Normalized by: ', normBy,'*',multiplyFactor)
  }
  
  return(events)
}

#' @title doPlot
#' @description plot events data
#' @param df (data.frame) events dataframe from getPlotData
#' @return ggplot 
doPlot <- function(df){
  if ('normalisation' %in% colnames(df)){
    myTitle <- unique(df$normalisation)
  } else {
    myTitle <- ''
  }
  p <- ggplot(df, aes(x = date,
                      y = value,
                      group = interaction(Country, variable),
                      colour = interaction(Country, variable),
                      text = text)) +
    geom_line() +
    labs(x = '',
         y = '',
         title = myTitle) +
    theme_minimal()
  return(p)
}

#' @title eventsPlot
#' @description plot events data
#' @param events (data.frame) events dataframe
#' @param population (data.frame) population dataframe
#' @param normBy (character) name of the column to use in population
#' @param multiplyFactor (numeric) positive integer
#' @return ggplot 
#' @export
eventsPlot <- function(events, population, normBy=NULL, multiplyFactor = 1){
  # prepare data
  df <- getPlotData(events, population, normBy, multiplyFactor)
  # get plot
  p <- doPlot(df)
  
  return(p)
}

#' @title populationForDisplay
#' @description remove columns of all nas
#' @param population (data.frame) population dataframe
#' @return population data frame 
#' @export
populationForDisplay <- function(population){
  population <- population[,colSums(is.na(population))<nrow(population)]
  return(population)
}

#' @title populationPlot
#' @description plot population data
#' @param population (data.frame) population dataframe
#' @param variables (character) vector of column names to plot
#' @return ggplot 
#' @export
populationPlot <- function(population, variables){
  # columns to keep
  idx <- which(colnames(population) %in% c('Country',variables))
  a <- population[,idx]
  # long table
  a <- melt(a, id='Country')
  # the text to use in plotly tooltips
  a$text <- paste0('Country: ', a$Country, '\n',
                   a$variable, ': ', a$value)
  # variables might have massively different ranges,
  # for instance age vs population. To ensure everything
  # is visible in the plot we normalise each variable
  # in the range 0-1
  a <- split(a, a$variable)
  a <- lapply(a, function(d){
    #normalize up to 1
    d$value <- d$value/max(d$value)
    return(d)
  })
  a <- bind_rows(a)
  
  # the actual plot
  p <- ggplot(data=a, aes(x=variable, y=value, fill=Country, text = text)) +
    geom_bar(stat="identity", position=position_dodge()) +
    labs(title = '') +
    theme_minimal() +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x = element_text(size=8, angle=45))
  
  return(p)
}
