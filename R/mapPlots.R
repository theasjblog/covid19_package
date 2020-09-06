#' @title getMapTitle
#' @description Get the map title. Used by the frontend app
#' @param world (sf): results of getMap[...]()
#' @param plotType (character): one of doMapTrend_normalise, doMapTrend,
#' doMapDataRate_raw, doMapDataRate_normalised,
#' doMapGBQuarantine_binary, doMapGBQuarantine, doMapData_raw,
#' doMapData_normalised
#' @param filterByCountry (character): The countries to plot in the map. If NULL all
#' countries are included
#' @param plotMetric (character) One of "cases", "deaths", "recovered"
#' @param chosenDay (numeric): the day to plot
#' @return character string of the map title
#' @export
getMapTitle <- function(world, filterByCountry = NULL, 
                        plotMetric = 'cases',
                        chosenDay = NULL, plotType){
  datesRange <- function(world, plotMetric, normalised){
    dayStop <- unique(world$variable)
    dayStart <- as.character(as.Date(unique(world$variable))-7)
    plotTitle <- paste0('Trend of ', plotMetric,
                        '\nfrom ', dayStart, ' to ', dayStop,
                        normalised)
    return(plotTitle)
  }
  
  mapGBQ <- function(world, plotMetric){
    dayStop <- unique(world$variable)
    dayStart <- as.character(as.Date(unique(world$variable))-7)
    
    plotTitle <- paste0('Cumulative number of ', plotMetric,
                        ' from ', dayStart, ' to ', dayStop,
                        '\nnormalised every 100,000 individuals')
    return(plotTitle)
  }
  
  mapGBQ_binary <- function(world){
    dayStop <- unique(world$variable)
    dayStart <- as.character(as.Date(unique(world$variable))-7)
    GBth <- 20
    plotTitle <- paste0('GB quarantine limit = ', GBth)
  }
  
  res <- switch (plotType,
                 'doMapTrend_normalise' = datesRange(world, plotMetric, '\nnormalised every 100,000 individuals'),
                 'doMapTrend' = datesRange(world, plotMetric, NULL),
                 'doMapDataRate_raw' = paste0('New ', plotMetric, ' on day ', unique(world$variable)),
                 'doMapDataRate_normalised' = paste0('New ', plotMetric, ' on day ',
                                                     unique(world$variable),
                                                     '\nnormalised every 100,000 individuals'),
                 'doMapGBQuarantine_binary' = mapGBQ_binary(world),
                 'doMapGBQuarantine' = mapGBQ(world, plotMetric),
                 'doMapData_raw' = paste0(plotMetric, ' on day ', unique(world$variable)),
                 'doMapData_normalised' = paste0(plotMetric, ' on day ',
                                                 unique(world$variable),
                                                 '\nnormalised every 100,000 individuals')
  )
  return(res)
}

#' @title getWorld
#' @description single entry point to plot map
#' @param plotData (covidData): S4 object
#' @param plotType (character): one of doMapTrend_normalise, doMapTrend,
#' doMapDataRate_raw, doMapDataRate_normalised,
#' doMapGBQuarantine_binary, doMapGBQuarantine, doMapData_raw,
#' doMapData_normalised
#' @param filterByCountry (character): The countries to plot in the map. If NULL all
#' countries are included
#' @param plotMetric (character) One of "cases", "deaths", "recovered"
#' @param chosenDay (numeric): the day to plot
#' @return data frame of cases, deaths and recovered
#' @export
getWorld <- function(plotData, filterByCountry = NULL, 
                     plotMetric = 'cases',
                     chosenDay = NULL, plotType){
  switch (plotType,
          'doMapTrend_normalise' = getMapTrend_normalise(plotData, filterByCountry, 
                                                        plotMetric, chosenDay),
          'doMapTrend' = getMapTrend(plotData, filterByCountry, 
                                    plotMetric, chosenDay),
          'doMapDataRate_raw' = getMapDataRate_raw(plotData, filterByCountry, 
                                                  plotMetric, chosenDay),
          'doMapDataRate_normalised' = getMapDataRate_normalised(plotData, filterByCountry, 
                                                                plotMetric, chosenDay),
          'doMapGBQuarantine_binary' = getMapGBQuarantine_binary(plotData, filterByCountry, 
                                                                plotMetric, chosenDay),
          'doMapGBQuarantine' = getMapGBQuarantine(plotData, filterByCountry, 
                                                  plotMetric, chosenDay),
          'doMapData_raw' = getMapData_raw(plotData, filterByCountry, 
                                          plotMetric, chosenDay),
          'doMapData_normalised' = getMapData_normalised(plotData, filterByCountry, 
                                                        plotMetric, chosenDay)
  )
}

#' @title plotMap
#' @description single entry point to plot map
#' @param world (sf): S4 object
#' @param plotType (character): one of doMapTrend_normalise, doMapTrend,
#' doMapDataRate_raw, doMapDataRate_normalised,
#' doMapGBQuarantine_binary, doMapGBQuarantine, doMapData_raw,
#' doMapData_normalised
#' @param filterByCountry (character): The countries to plot in the map. If NULL all
#' countries are included
#' @param plotMetric (character) One of "cases", "deaths", "recovered"
#' @param chosenDay (numeric): the day to plot
#' @return data frame of cases, deaths and recovered
#' @export
plotMap <- function(world, filterByCountry = NULL, 
                             plotMetric = 'cases',
                             chosenDay = NULL, plotType){
  switch (plotType,
    'doMapTrend_normalise' = doMapTrend_normalise(world, filterByCountry, 
                                                  plotMetric, chosenDay),
    'doMapTrend' = doMapTrend(world, filterByCountry, 
                              plotMetric, chosenDay),
    'doMapDataRate_raw' = doMapDataRate_raw(world, filterByCountry, 
                                            plotMetric, chosenDay),
    'doMapDataRate_normalised' = doMapDataRate_normalised(world, filterByCountry, 
                                                          plotMetric, chosenDay),
    'doMapGBQuarantine_binary' = doMapGBQuarantine_binary(world, filterByCountry, 
                                                          plotMetric, chosenDay),
    'doMapGBQuarantine' = doMapGBQuarantine(world, filterByCountry, 
                                            plotMetric, chosenDay),
    'doMapData_raw' = doMapData_raw(world, filterByCountry, 
                                    plotMetric, chosenDay),
    'doMapData_normalised' = doMapData_normalised(world, filterByCountry, 
                                                  plotMetric, chosenDay)
  )
}



#' @title getMapData
#' @description get the data to plot on a map
#' @param plotData (covidData): S4 object
#' @param plotType (character): one of avgPast7_raw, avgPast7_normalised,
#' cumulativeAll_raw, cumulativeAll_normalised, cumulativePast7_raw,
#' cumulativePast7_normalised, avgToday_raw, avgToday_normalised
#' @param filterByCountry (character): The countries to plot in the map. If NULL all
#' countries are included
#' @param plotMetric (character) One of "cases", "deaths", "recovered"
#' @param chosenDay (numeric): the day to plot
#' @return data frame of cases, deaths and recovered
getMapData <- function(plotData, plotType,
                       filterByCountry = NULL, plotMetric = 'cases',
                       chosenDay = NULL){
  
  if(str_detect(plotType, '_raw')){
    df <- slot(plotData, 'JHUData_raw')
    dfSmooth <- slot(plotData, 'JHUData_smooth')
  } else if(str_detect(plotType, '_normalised')){
    df <- slot(plotData, 'JHUData_diffRaw')
    dfSmooth <- slot(plotData, 'JHUData_diffSmooth')
  }
  
  #get the country to plot
  populationDf <- slot(plotData, 'populationDf')
  if(is.null(filterByCountry)){
    filterByCountry <- as.character(unique(populationDf$Country))
  }
  populationDf <- populationDf %>% 
    filter(Country %in% filterByCountry & type == plotMetric)
  
  df <- df %>% filter(ID %in% populationDf$ID)
  dfSmooth <- dfSmooth %>% filter(ID %in% populationDf$ID)
  
  if(is.null(chosenDay)){
    idxChosenDay <- ncol(df)-1
  }
  
  if(str_detect(plotType, '_normalised')){
    df[,colnames(df)!='ID'] <- df[,colnames(df)!='ID']*100e3/populationDf$Population
    dfSmooth[,colnames(dfSmooth)!='ID'] <- dfSmooth[,colnames(dfSmooth)!='ID']*100e3/populationDf$Population
  }
  
  f <- function(x){
    sum(diff(x[seq(idxChosenDay-6, idxChosenDay)]))
  }
  
  if (str_detect(plotType, 'Past7')){
    values <- apply(df,1,f)
    valuesSmooth <- apply(dfSmooth,1,f)
  } else {
    values <- as.numeric(df[,idxChosenDay])
    valuesSmooth <- as.numeric(dfSmooth[,idxChosenDay])
  }
  
  # if we show the trend, we need ot show if it is increasing or decreasing
  #get trends
  if(str_detect(plotType, 'avg') & str_detect(plotType, 'Past7')){
    for(i in 1:nrow(dfSmooth)){
      lastTen <- mean(diff(as.numeric(dfSmooth[i,
                                        seq(idxChosenDay-7,# breaks if idx<7
                                            idxChosenDay)])),
                      na.rm = TRUE)
      
       

      if (lastTen<0){
        values[i] <- values[i]/-7
      } else {
        values[i] <- values[i]/7
      }
    }
  }
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  chosenCountriesiso3c <- countrycode(filterByCountry,
                                      origin = 'country.name',
                                      destination = 'iso3c')
  world <- world %>% filter(gu_a3 %in% chosenCountriesiso3c)
  newData <- data.frame(plotValues = values,
                        gu_a3 = chosenCountriesiso3c)
  # potenital issue: newData has countries not in world
  world <- merge(world, newData, by  = 'gu_a3', all = TRUE)
  
  return(world)
  
}

#' @title doData
#' @description plot on a map
#' @param plotData (covidData): S4 object
#' @param plotType (character): one of avgPast7_raw, avgPast7_normalised,
#' cumulativeAll_raw, cumulativeAll_normalised, cumulativePast7_raw,
#' cumulativePast7_normalised, avgToday_raw, avgToday_normalised
#' @param filterByCountry (character): The countries to plot in the map. If NULL all
#' countries are included
#' @param plotMetric (character) One of "cases", "deaths", "recovered"
#' @param chosenDay (numeric): the day to plot
#' @return a ggplot
#' @export
doMap <- function(plotData, plotType,
                  filterByCountry = NULL, plotMetric = 'cases',
                  chosenDay = NULL){
  world <- getMapData(plotData, plotType,
                      filterByCountry, plotMetric,
                      chosenDay)
  
  rawData <- slot(plotData, 'JHUData_diffSmooth')
  if(is.null(chosenDay)){
    chosenDay <- ncol(rawData)-1
  }
  dateChar <- as.Date(getDates(colnames(rawData[seq(1,ncol(rawData)-1)])))
  dateChar <- dateChar[chosenDay]
  #if (normalizeByPopulation){
    plotTitle <- paste0('Cases normalized by population \n Day: ', dateChar)
  #} else {
    plotTitle <- paste0('Cases \n Day: ', dateChar)
  #}
  #if (showTrend){
    plotTitle <- 'Average daily trend'
  #}
  g <- ggplot(data = world) +
    geom_sf(aes(fill = plotValues)) +
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



#' @import raster

# us <- getData("GADM",country="USA",level=1)
# canada <- getData("GADM",country="CAN",level=1)
# it <- getData('GADM', country = "ITA", level=0)
# us.states <- us[us$NAME_1 %in% states,]
# ca.provinces <- canada[canada$NAME_1 %in% provinces,]
# 
# over.col <- colorRampPalette(c("white", "black"))
# allS <- getData("GADM",country='Spain', level=0)
# allF <- getData("GADM",country='France', level=0)
# allSbbox <- bbox(allS)
# allFbbox <- bbox(allF)
# xlim <- c(min(allSbbox[1,1],allFbbox[1,1]),max(allSbbox[1,2],allFbbox[1,2]))
# ylim <- c(min(allSbbox[2,1],allFbbox[2,1]),max(allSbbox[2,2],allFbbox[2,2]))
# 
# library('RColorBrewer')
# palette(brewer.pal(n = 8, name = "Set2"))
# 
# plot(allS, xlim=xlim, ylim=ylim, col=2)
# plot(allF, xlim=xlim, ylim=ylim, add=T, col = 'green')
# 
# a <- do.call(bind, list(allS,allF))
# abbox <- bbox(a)
# ggplot(a,aes(x=long,y=lat,group=group), color='red')+
#   geom_path()+
#   geom_path(data=a)+
#   coord_map()
# 
# 
# us.bbox <- bbox(us.states)
# ca.bbox <- bbox(ca.provinces)
# xlim <- c(min(us.bbox[1,1],ca.bbox[1,1]),max(us.bbox[1,2],ca.bbox[1,2]))
# ylim <- c(min(us.bbox[2,1],ca.bbox[2,1]),max(us.bbox[2,2],ca.bbox[2,2]))
# plot(us.states, xlim=xlim, ylim=ylim)
# plot(ca.provinces, xlim=xlim, ylim=ylim, add=T)
# 
# 
# ggplot(canada,aes(x=long,y=lat,group=group))+
#   geom_path()+
#   geom_path(data=canada)+
#   coord_map()
# 
# 
# library('viridis')
# library('ggplot2')
# ggplot(a) +
#   geom_polygon(data=a, aes(x=long, y=lat, group=group),
#                fill=NAME_0, color="grey50", size=0.25) +
#   scale_fill_viridis(na.value="white") +
#   coord_equal()
# 
# 
# +
# 
# 
#   theme_map() +
#   theme(legend.position="bottom") +
#   theme(legend.key.width=unit(2, "cm"))
# 
# 
# 
# library('maps')
# 
# # Some EU Contries
# some.eu.countries <- c(
#   "Spain", "France"
# )
# # Retrieve the map data
# some.eu.maps <- map_data("world", region = some.eu.countries)
# 
# 
# # Compute the centroid as the mean longitude and latitude
# # Used as label coordinate for country's names
# region.lab.data <- some.eu.maps %>%
#   group_by(region) %>%
#   summarise(long = mean(long), lat = mean(lat))
# 
# 
# 
# ggplot(some.eu.maps, aes(x = long, y = lat)) +
#   geom_polygon(aes( group = group, fill = region))+
#   #geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
#   scale_fill_viridis_d()+
#   theme_void()+
#   theme(legend.position = "none")
# 
# 
# aa <- allCases %>% filter(Country_Region == "US")
# state_prov <- rnaturalearth::ne_states(c("italy"))
# plot(state_prov)
# ggplot(aa, aes(x = long, y = lat)) +
#   geom_polygon(aes( group = Province_State, fill = Province_State))+
#   #geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
#   scale_fill_viridis_d()+
#   theme_void()+
#   theme(legend.position = "none")
# 
