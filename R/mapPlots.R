#' @title getMapData
#' @description get the data to plot on a map
#' @param plotData (covidData): S4 object
#' @param gadm (list) list with data of all the available countries
#' @param normalizeByPopulation (logical): if TRUE, divide the number of cases by the
#' country population
#' @param filterByCountry (character): The countries to plot in the map. If NULL all
#' countries are included
#' @param plotMatric (character) One of "cases", "deaths", "recovered"
#' @param chosenDay (numeric): the day to plot
#' @param showTrend (logical): colour the map by the variation of the last 5 days vs the previous
#' 5 days
#' @return data frame of cases, deaths and recovered
getMapData <- function(plotData, gadm, normalizeByPopulation = FALSE,
                       filterByCountry = NULL, plotMetric = 'cases',
                       chosenDay = NULL, showTrend = FALSE){
  
  #get the country to plot
  if(is.null(filterByCountry)){
    filterByCountry <- as.character(unique(slot(plotData,
                                                'populationDf')$Country))
  }
  filterByCountryiso3c <- countrycode(filterByCountry, origin = 'country.name',
                                      destination = 'iso3c')
  
  
  idx <- which(names(gadm) %in% filterByCountryiso3c)
  gadm <- gadm[idx]
  
  
  if(showTrend){
    df <- slot(plotData, 'JHUData_raw')
    chosenDay <- NULL
  } else {
    df <- slot(plotData, 'JHUData_diffRaw')
  }
  
  toInclude <- slot(plotData, 'populationDf') %>% 
    filter(type == plotMetric & Country %in% filterByCountry)
  OKids <- toInclude$ID
  df <- df[OKids,seq(1,ncol(df))]
  
  
  
  if(is.null(chosenDay)){
    idxChosenDay <- ncol(df)-1
  }
  
  resMetric <- list()
  resPop <- list()
  for (i in unique(toInclude$Country)){
    idx <- which(toInclude$Country == i)
    resPop[[length(resPop)+1]] <- c(i,
                                    sum(toInclude$Population[idx],
                                        na.rm = TRUE))
    IDs <- toInclude$ID[idx]
    tmp <- df %>% filter(ID %in% IDs)
    tmp <- df[, which(colnames(tmp) != 'ID')]
    tmp <- colSums(tmp, na.rm = TRUE)
    resMetric[[length(resMetric)+1]] <- tmp
  }
  
  resMetric <- as.data.frame(do.call(rbind, resMetric))
  resPop <- as.data.frame(do.call(rbind, resPop))
  colnames(resPop) <- c('Country', 'Population')
  
  #get trends
  if(showTrend){
    for(i in 1:nrow(resPop)){
      lastTen <- mean(diff(as.numeric(resMetric[i,
                                                seq(ncol(resMetric)-10,
                                                    ncol(resMetric))])),
                      na.rm = TRUE)
      meanVal <- round(mean(as.numeric(resMetric[i,
                                      seq(ncol(resMetric)-10,
                                          ncol(resMetric))]),
                            na.rm = TRUE))
    }}
        
    #     if(!is.na(meanVal)){
    #       if(lastTen<0){
    #         meanVal <- meanVal*-1
    #       }
    #       world$cases[idx] <- meanVal
    #     }
    #   } else {
    #     world$cases[idx] <- rawData[idxCC, idxChosenDay]
    #   }
    # }
    # 

  if (normalizeByPopulation){
    world$cases <- suppressWarnings(world$cases/world$pop_est)
  }

  idx <- which(!is.na(world$cases))
  world <- world[idx, ]

  return(world)

}

#' @title doData
#' @description plot on a map
#' @param plotData (covidData): S4 object
#' @param normalizeByPopulation (logical): if TRUE, divide the number of cases by the
#' country population
#' @param filterByCountry (character): The countries to plot in the map. If NULL all
#' countries are included
#' @param removeCountries (character): countries to remove
#' @param chosenDay (numeric): the day to plot
#' @param showTrend (logical): colour the map by the variation of the last 5 days vs the previous
#' 5 days
#' @return a ggplot
#' @export
doMap <- function(plotData, normalizeByPopulation = FALSE,
                  filterByCountry = NULL,
                  removeCountries = NULL,
                  chosenDay = NULL, showTrend = FALSE){
  world <- getMapData(plotData, normalizeByPopulation, filterByCountry,
                      removeCountries, chosenDay, showTrend)
  rawData <- slot(plotData, 'JHUData_diffSmooth')
  if(is.null(chosenDay)){
    chosenDay <- ncol(rawData)-6
  }
  dateChar <- as.Date(getDates(colnames(rawData[seq(5,ncol(rawData)-2)])))
  dateChar <- dateChar[chosenDay]
  if (normalizeByPopulation){
    plotTitle <- paste0('Cases normalized by population \n Day: ', dateChar)
  } else {
    plotTitle <- paste0('Cases \n Day: ', dateChar)
  }
  if (showTrend){
    plotTitle <- 'Average daily trend'
  }
  g <- ggplot(data = world) +
    geom_sf(aes(fill = cases)) +
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
