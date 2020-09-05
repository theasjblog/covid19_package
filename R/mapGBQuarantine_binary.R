#' @title getMapGBQuarantine_binary
#' @description get the data to plot on a map of total number of cases
#' @param plotData (covidData): S4 object
#' @param filterByCountry (character): The countries to plot in the map. If NULL all
#' countries are included
#' @param plotMetric (character) One of "cases", "deaths", "recovered"
#' @param chosenDay (numeric): the day to plot
#' @return data frame of cases, deaths and recovered
getMapGBQuarantine_binary <- function(plotData, filterByCountry = NULL, 
                               plotMetric = 'cases',
                               chosenDay = NULL){
  GBth <- 20
  df <- slot(plotData, 'JHUData_diffSmooth')
  
  if(is.null(chosenDay)){
    #subtracting to so that I can the right date by
    #adding the min date to the idxChosenDay
    idxChosenDay <- ncol(df)-2
  } else {
    idxChosenDay <- chosenDay - 1
  }
  
  idx <- which(str_detect(colnames(df), 'X'))
  dateMin <- min(getDates(colnames(df)[idx]))
  chosenDate <- as.character(as.Date(dateMin)+idxChosenDay)
  
  #get the country to plot
  populationDf <- slot(plotData, 'populationDf')
  if(is.null(filterByCountry)){
    filterByCountry <- as.character(unique(populationDf$Country))
  }
  populationDf <- populationDf %>% 
    filter(Country %in% filterByCountry & type == plotMetric)
  
  df <- df %>% filter(ID %in% populationDf$ID)
  # sum countries
  df <- merge(df, populationDf, by='ID', all=TRUE)
  sp <- split(df, df$Country)
  sp <- lapply(sp, function(d){
    idx <- which(str_detect(colnames(d), 'X'))
    totoPop <- sum(d$Population, na.rm = TRUE)
    res <- colSums(d[,idx])/totoPop*100e3
    nameDay <- names(res)[idxChosenDay+1]
    res <- round(sum(res[seq(idxChosenDay-5, idxChosenDay+1)],na.rm = TRUE))
    if (res >= GBth){res <- GBth+10} else {res <- GBth-10}
    res <- data.frame(X = res,
                      Country = unique(d$Country))
    colnames(res) <- c(nameDay, 'Country')
    return(res)
  })
  df <- bind_rows(sp)
  val <- melt(data = df, id.vars = c("Country"))
  val$variable <- getDates(val$variable)
  val <- val %>% filter(variable == chosenDate)
  val$Country <- countrycode(val$Country,
                             origin = 'country.name',
                             destination = 'iso3c')
  colnames(val)[colnames(val) == 'Country'] <- 'gu_a3'
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  chosenCountriesiso3c <- countrycode(filterByCountry,
                                      origin = 'country.name',
                                      destination = 'iso3c')
  world <- world %>% filter(gu_a3 %in% chosenCountriesiso3c)
  #newData <- data.frame(plotValues = values,
  #                      gu_a3 = chosenCountriesiso3c)
  # potenital issue: newData has countries not in world
  world <- merge(world, val, by  = 'gu_a3', all = TRUE)
  
  return(world)
  
}

#' @title doMapGBQuarantine_binary
#' @description plot on a map
#' @param plotData (covidData): S4 object
#' @param filterByCountry (character): The countries to plot in the map. If NULL all
#' countries are included
#' @param plotMetric (character) One of "cases", "deaths", "recovered"
#' @param chosenDay (numeric): the day to plot
#' @return a ggplot
doMapGBQuarantine_binary <- function(plotData, filterByCountry = NULL,
                              plotMetric = 'cases',
                              chosenDay = NULL){
  world <- getMapGBQuarantine_binary(plotData, filterByCountry, plotMetric, chosenDay)
  dayStop <- unique(world$variable)
  dayStart <- as.character(as.Date(unique(world$variable))-7)
  GBth <- 20
  plotTitle <- paste0('GB quarantine limit = ', GBth)
  
  g <- ggplot(data = world) +
    geom_sf(aes(fill = value)) +
    scale_fill_gradient2(low = "blue",
                         mid = "white",
                         high = "red",
                         midpoint = GBth,
                         limits = c(GBth-10, GBth+10))+
    theme(legend.position = "none")+
    ggtitle(plotTitle) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'aliceblue'),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.ticks = element_blank()) +
    labs(fill = "")
  return(g)
}
