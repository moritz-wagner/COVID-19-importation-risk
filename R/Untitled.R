library(rnaturalearth)
library(mapview)

library(RColorBrewer)
library(leaflet)
library(ggplot2)
library(ggmap)

library(raster)
library(htmltools)
library(sf)

library(rgeos)

##Get MOBS data on relative risk of importation from China (https://www.mobs-lab.org/2019ncov.html)----------
data_flights <- read.csv('epirisk.csv')
data_flights$Country <- countrycode(data_flights$label,origin = 'country.name',destination = 'country.name')

spdf_africa <- ne_countries(returnclass = "sf")
spdf_africa$Country <- countrycode(spdf_africa$name,origin = 'country.name',destination = 'country.name')

spdf_africa <- full_join(spdf_africa,data_flights)
mapview(spdf_africa,zcol = "risk")
spdf_africa %>% subset(continent=="Africa") -> Africa

mapviewOptions(# raster.palette = heat.colors,
               vector.palette = heat.colors,
               # na.color = "grey",
               # layers.control.pos = "topright"
               )
mapviewOptions(default = TRUE)
Africa$logrisk <- log(Africa$risk)
Africa %>% mutate(risk=ifelse(is.na(risk),"missing",risk))
mapview(Africa,zcol = "risk",col.regions=colorRampPalette(c('green','orange', 'red')),legend=FALSE) -> m

mapshot(m,file = 'africa_map.png')

