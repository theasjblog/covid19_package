allMaps <- function(df, populationDf, chosenDay, filterByCountry, plotMetric,
                    normalizePop, quarantinePlot, binaryPlot, doTrend, GBth){
  
  #convert the given index to the corresponding column index in the daraframe
  idxchosenDay <- getIdxChosenDay(df, chosenDay)
  #convert colnames  to date, used in the colnames of the returned results
  chosenDate <- getChosenDate(df, idxchosenDay-1)
  filterByCountryOriginal <- filterByCountry
  #get the country to plot
  if(is.null(filterByCountry)){
    filterByCountry <- as.character(unique(populationDf$Country))
  }
  # re
  populationDf <- populationDf %>% 
    filter(Country %in% filterByCountry & type == plotMetric)
  # filter out the countries we do not map
  df <- df %>% filter(ID %in% populationDf$ID)
  # sum countries
  df <- sumCountries(df, populationDf)
  
  if (normalizePop){
    #normalize by population
    df <- normalizeByPop(df)
  }
  
  if (quarantinePlot){
    df <- sumPast7(df, idxchosenDay)
    if(binaryPlot){
      df[which(df[,2]<GBth),2] <- GBth-10
      df[which(df[,2]>=GBth),2] <- GBth+10
    }
  }
  
  countriesAvailable <- df$Country
  
  # trend
  if (doTrend){
    df <- apply(df, 1, trendFunction, idxchosenDay)
    val <- data.frame(Country = countriesAvailable,
                      value = df,
                      variable = chosenDate)
  } else {
    # melt
    val <- melt(data = df, id.vars = c("Country"))
    val$variable <- getDates(val$variable)
    val <- val %>% filter(variable == chosenDate)
  }
  
  val$Country <- countrycode(val$Country,
                             origin = 'country.name',
                             destination = 'iso3c')
  colnames(val)[colnames(val) == 'Country'] <- 'gu_a3'
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  chosenCountriesiso3c <- countrycode(filterByCountry,
                                      origin = 'country.name',
                                      destination = 'iso3c')
  world <- world %>% filter(gu_a3 %in% chosenCountriesiso3c)
  
  world <- merge(world, val, by  = 'gu_a3', all = TRUE)
  
  res <- new('worldMap')
  
  slot(res, 'world') <- world
  if(!is.null(filterByCountryOriginal)){
    slot(res, 'filterByCountry') <- filterByCountry
  }
  if(!is.null(GBth)){
    slot(res, 'th') <- GBth
  }
  return(res)
}


trendFunction <- function(x, idxchosenDay){
  res <- median(diff(as.numeric(x[seq(idxchosenDay-5, idxchosenDay+1)])),
                na.rm = TRUE)
  return(res)
}
