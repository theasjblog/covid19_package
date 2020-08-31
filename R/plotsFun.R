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
#' @param align logical If we should align by date of min number of cases/deaths/recovered
#' or the rate of change (true)
prepareDataPlot <- function(dataObj, geographyFilter, typePlot, plotRate,
                            smooth, scale, normalizeByPop, plotLim, align){
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
  }
  
  idx <- which(colnames(df) %in% c('Population', 'Area'))
  values <- data.frame(dates = rep(colnames(df)[-idx],
                                   nrow(df)))
  
  values$area <- rep(df$Area, each=ncol(df)-length(idx))
  res <- NULL
  for (i in 1:nrow(df)){
    res <- c(res,df[i, -idx])
  }
  values$values <- as.numeric(res)
  
  values$dates <- as.Date(getDates(values$dates))
  
  values$area <- factor(values$area)
  
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
  
  if(align){
    th <- switch(typePlot,
                 'cases' = 100,
                 'deaths' = 50,
                 'recovered' = 100)
    sp <- split(values, values$area)
    for(i in 1:length(sp)){
      d <- sp[[i]]
      d <- d %>%
        filter(values >= th)
      d$dates <- seq(0,nrow(d)-1)
      sp[[i]] <- d
    }
    values <- bind_rows(sp)
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
#' @param align logical If we should align by date of min number of cases/deaths/recovered
#' or the rate of change (true)
#' @export
doPlot <- function(dataObj, geographyFilter = NULL, typePlot = 'cases',
                   plotRate = FALSE, smooth = TRUE, scale = 'linear', normalizeByPop = FALSE,
                   plotLim = NULL, align = FALSE){
  # add validation
  values <- prepareDataPlot(dataObj = dataObj, geographyFilter = geographyFilter,
                            typePlot = typePlot, plotRate = plotRate, smooth = smooth,
                            scale = scale, normalizeByPop = normalizeByPop,
                            plotLim = plotLim, align = align)
  
  if(align){
    xLabel <- '# days'
  } else {
    xLabel <- "Date"
  }
  
  
  
  p <- ggplot(data = values, aes(x = dates,
                                 y = values,
                                 group = area)) +
    geom_line(aes(color = area)) +
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
#' @param align logical If we should align by date of min number of cases/deaths/recovered
#' or the rate of change (true)
#' @export
plotAllMetrics <- function(dataObj, geographyFilter = NULL,
                           plotRate = FALSE, smooth = TRUE, scale = 'linear',
                           normalizeByPop = FALSE, plotLim = NULL, align = FALSE){
  valuesCases <- prepareDataPlot(dataObj = dataObj, geographyFilter = geographyFilter,
                                 plotRate = plotRate, smooth = smooth, scale = scale,
                                 normalizeByPop = normalizeByPop, plotLim = plotLim,
                                 align = align, typePlot = 'cases')
  valuesDeaths <- prepareDataPlot(dataObj = dataObj, geographyFilter = geographyFilter,
                                  plotRate = plotRate, smooth = smooth, scale = scale,
                                  normalizeByPop = normalizeByPop, plotLim = plotLim,
                                  align = align, typePlot = 'deaths')
  valuesRecovered <- prepareDataPlot(dataObj = dataObj, geographyFilter = geographyFilter,
                                     plotRate = plotRate, smooth = smooth, scale = scale,
                                     normalizeByPop = normalizeByPop, plotLim = plotLim,
                                     align = align, typePlot = 'recovered')
  values <- bind_rows(valuesCases, valuesDeaths, valuesRecovered)
  values$type <- c(rep('cases', nrow(valuesCases)),
                   rep('deaths', nrow(valuesDeaths)),
                   rep('recovered', nrow(valuesRecovered)))
  values <- values %>% filter(!is.na(values))
  
  p <- ggplot(data = values, aes(x = dates,
                             y = values,
                             group = type)) +
    geom_line(aes(color = type)) +
    labs(x = "Date",
         y = "#",
         title = toupper(paste(geographyFilter, collapse = ' - '))) +
    theme_minimal() +
    facet_grid(rows = vars(area))
  
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

