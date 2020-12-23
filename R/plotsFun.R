#' @title prepareDataPlot
#' @description prepare data to plot for a single typePlot
#' @param dataObj covidData object
#' @param geographyFilter the geography to plot. In the form of "Country, State, City".
#' To see all available combinations use showGeographyFilter(dataObj)
#' @param typePlot cases, deaths or recovered
#' @param plotRate (logical) if to plot cumulative increase (FALSE) or rate (TRUE)
#' @param smooth (logical) if to smooth the curve
#' @param scale if to plot in linear or log scale
#' @param normalizeByPop (logical), if true, report metric y 100K people
#' @param plotLim Dates max an min limits for the plot
prepareDataPlot <- function(dataObj, geographyFilter, typePlot, plotRate,
                            smooth, scale, normalizeByPop, plotLim){
  if (plotRate){
    if (smooth){
      df <- slot(dataObj, 'JHUData_diffSmooth')
    } else {
      df <- slot(dataObj, 'JHUData_diffRaw')
    }
  } else {
    if (smooth){
      df <- slot(dataObj, 'JHUData_smooth')
    } else {
      df <- slot(dataObj, 'JHUData_raw')
    }
  }
  populationDf <- slot(dataObj, 'populationDf')
  
  #filter by type
  populationDf <- populationDf %>% 
    filter(type == typePlot)
  
  if(!is.null(geographyFilter)){
    geographyFilterSp <- str_split(geographyFilter, ', ')
    idx <- which(colnames(df) == 'ID')
    res <- lapply(geographyFilterSp, function(d){
      if(length(d) == 1){
        tmp <- populationDf %>% filter(Country==d[1])
      }
      if(length(d) == 2){
        tmp <- populationDf %>% filter(Country==d[1] & State==d[2])
      }
      if(length(d) == 3){
        tmp <- populationDf %>% filter(Country==d[1] & State==d[2] & City==d[3])
      }
      tmpDf <- data.frame(df[df$ID %in% tmp$ID, -idx])
      tmpDf <- colSums(tmpDf, na.rm = TRUE)
      # extract population
      populationArea <- sum(tmp$Population, na.rm = TRUE)
      if(normalizeByPop){
        tmpDf <- tmpDf*100e3/populationArea
      }
      tmpDf <- as.data.frame(as.list(tmpDf))
      return(tmpDf)
    })
    df <- bind_rows(res)
    df$Area <- geographyFilter
    values <- melt(df, id.vars = 'Area')
  } else {
    df <- df %>% filter(ID %in% populationDf$ID)
    values <- data.frame(variable = colnames(df)[colnames(df) != 'ID'],
                         value = colSums(df[,colnames(df) != 'ID'],
                                         na.rm = TRUE),
                         Area = 'World'
                         )
  }
  
  
  values$variable <- getDates(values$variable)
  
  if(scale == 'log'){
    values$value <- log10(values$value)
  }
  
  if (!is.null(plotLim)){
    values$dateAsNum <- as.Date(values$variable)-min(as.Date(values$variable))
    values <- values %>%
      filter(dateAsNum >= plotLim[1])
    values <- values %>%
      filter(dateAsNum <= plotLim[2])
    
  }
  
  
  return(values)
}


#' @title doPlot
#' @description plot data for a single country
#' @param dataObj covidData object
#' @param geographyFilter the geography to plot. In the form of "Country, State, City".
#' To see all available combinations use showGeographyFilter(dataObj)
#' @param typePlot cases, deaths or recovered
#' @param plotRate (logical) if to plot cumulative increase (FALSE) or rate (TRUE)
#' @param smooth (logical) if to smooth the curve
#' @param scale if to plot in linear or log scale
#' @param normalizeByPop (logical), if true, report metric y 100K people
#' @param plotLim Dates max an min limits for the plot
#' @export
doPlot <- function(dataObj, geographyFilter = NULL, typePlot = 'cases',
                   plotRate = FALSE, smooth = TRUE, scale = 'linear',
                   normalizeByPop = FALSE,
                   plotLim = NULL){
  # add validation
  values <- prepareDataPlot(dataObj = dataObj, geographyFilter = geographyFilter,
                            typePlot = typePlot, plotRate = plotRate, smooth = smooth,
                            scale = scale, normalizeByPop = normalizeByPop,
                            plotLim = plotLim)
  
  xLabel <- "Date"
  
  values$variable <- as.Date(values$variable)
  colnames(values)[colnames(values) == 'variable'] <- 'Date'
  colnames(values)[colnames(values) == 'value'] <- typePlot
  values$text <- paste0('Date: ', values$Date, '\n',
                        'Area: ', values$Area, '\n',
                        str_to_title(typePlot), ': ', round(values[[typePlot]], digits = 0))
    
  p <- ggplot(data = values, aes(x = Date,
                                 y = .data[[typePlot]],
                                 group = Area,
                                 text = text)) +
    geom_line(aes(color = Area)) +
    labs(x = xLabel,
         y = paste0("# ", typePlot),
         title = toupper(typePlot)) +
    theme_minimal()
  
  return(p)
  
}

#' @title plotAllMetrics
#' @description plot cases, deaths and recovered in a single plot
#' @param dataObj covidData object
#' @param geographyFilter the geography to plot. In the form of "Country, State, City".
#' To see all available combinations use showGeographyFilter(dataObj)
#' @param plotRate (logical) if to plot cumulative increase (FALSE) or rate (TRUE)
#' @param smooth (logical) if to smooth the curve
#' @param scale if to plot in linear or log scale
#' @param normalizeByPop (logical), if true, report metric y 100K people
#' @param plotLim Dates max an min limits for the plot
#' @export
plotAllMetrics <- function(dataObj, geographyFilter = NULL,
                           plotRate = FALSE, smooth = TRUE, scale = 'linear',
                           normalizeByPop = FALSE, plotLim = NULL){
  valuesCases <- prepareDataPlot(dataObj = dataObj, geographyFilter = geographyFilter,
                                 plotRate = plotRate, smooth = smooth, scale = scale,
                                 normalizeByPop = normalizeByPop, plotLim = plotLim,
                                 typePlot = 'cases')
  valuesDeaths <- prepareDataPlot(dataObj = dataObj, geographyFilter = geographyFilter,
                                  plotRate = plotRate, smooth = smooth, scale = scale,
                                  normalizeByPop = normalizeByPop, plotLim = plotLim,
                                  typePlot = 'deaths')
  valuesRecovered <- prepareDataPlot(dataObj = dataObj, geographyFilter = geographyFilter,
                                     plotRate = plotRate, smooth = smooth, scale = scale,
                                     normalizeByPop = normalizeByPop, plotLim = plotLim,
                                     typePlot = 'recovered')
  values <- bind_rows(valuesCases, valuesDeaths, valuesRecovered)
  values$type <- c(rep('cases', nrow(valuesCases)),
                   rep('deaths', nrow(valuesDeaths)),
                   rep('recovered', nrow(valuesRecovered)))
  values <- values %>% filter(!is.na(values))
  
  values$variable <- as.Date(values$variable)
  colnames(values)[colnames(values) == 'variable'] <- 'Date'
  values$text <- paste0('Date: ', values$Date, '\n',
                        'Area: ', values$Area, '\n',
                        'Type: ', values$type, '\n',
                        str_to_title(values$type), ': ',
                        round(values$value, digits = 0))
  
  p <- ggplot(data = values, aes(x = Date,
                             y = value,
                             group = type,
                             text = text)) +
    geom_line(aes(color = type)) +
    labs(x = "Date",
         y = "#",
         title = toupper(paste(geographyFilter, collapse = ' - '))) +
    theme_minimal() +
    facet_grid(rows = vars(Area))
  
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

