# library(raster)
# states    <- c('California', 'Nevada', 'Utah', 'Colorado', 'Wyoming', 'Montana', 'Idaho', 'Oregon', 'Washington')
# provinces <- c("British Columbia", "Alberta")
# 
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
