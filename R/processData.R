#' @title smoothDf
#' @description smooth a df with time series
#' @param df (data.frame) a data frame with rows of time series
#' @return smoothed data frame
smoothDf <- function(df){
  dfOr <- df
  df <- apply(df, 1, function(x) smoothValues(as.numeric(x)))
  df <- t(df)
  dfOr[,] <- df
  
  return(dfOr)
}

#' @title diffSmoothDf
#' @description smooth diff a df with time series
#' @param df (data.frame) a data frame with rows of time series
#' @return smoothed diff'ed data frame
diffSmoothDf <- function(df){
  dfOr <- df
  df <- apply(df, 1, function(x) smoothValues(c(0, diff(as.numeric(x)))))
  df <- t(df)
  dfOr[,] <- df
  
  return(dfOr)
}

#' @title diffRawDf
#' @description diff a df with time series
#' @param df (data.frame) a data frame with rows of time series
#' @return diff'ed data frame
diffRawDf <- function(df){
  dfOr <- df
  df <- apply(df, 1, function(x) c(0, diff(as.numeric(x))))
  df <- t(df)
  dfOr[,] <- df
  
  return(dfOr)
}

#' @title getDates
#' @description convert JHU dates to R dates
#' @param jhuDates a vector of dates
#' @return vector of dates
#' @export
getDates <- function(jhuDates){
  dates <- NULL
  for (i in jhuDates){
    tempDate <- as.character(as.Date(i, format('X%m.%d.%y')))
    dates <- c(dates, tempDate)
  }
  return(dates)
}


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

#' @title showGeographyFilter
#' @description return the avilable geography filters
#' @param plotData (covidData): S4 object
#' @return character vector
#' @export
showGeographyFilter <- function(dataObj){
  return(slot(dataObj, 'keys'))
}
