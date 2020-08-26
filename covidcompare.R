library(data.table)
library(raster)
library(sf)
library(zoo)
library(geojsonio)
library(geojsonsf)
library(rgdal)
library(ggplot2)
library(RColorBrewer)
library(rgeos)
library(ggmap)
library(scales)
library(RColorBrewer)
library(maptools)
library(gtools)
library(data.table)
library(plotly)
library(plotrix)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(leaflet)
library(leaflet.mapboxgl)
library(dplyr)


distlat <- <PATH TO LAT LONG>
distpts <- <PATH TO CENTROIDS FILE>
distshp <- <PATH TO SHAPE FILE>
cases <- <PATH TO CASES FILE>
cases2 <- <PATH TO ALTERNATIVE CASES FILE>

distmap <- unique(cases2[,.(DISTNAME=distname,State,District)])
caseapr <- cases[Date=='2020-04-30'] # Apr Cases (Substitute any month)
casejun <- cases[Date=='2020-06-30'] # Jun Cases (Substitute any month)

# Merge the two 
caseapr <- merge(caseapr,distmap,by=c('State','District'))
casejun <- merge(casejun,distmap,by=c('State','District'))

# Find the districts common to both
totaldists <- distmap[DISTNAME %in% c(caseapr$DISTNAME, casejun$DISTNAME)]

# Re-merge
caseapr <- merge(totaldists, caseapr[,-c('State','District'),with=F], by = 'DISTNAME', all.x=T)
casejun <- merge(totaldists, casejun[,-c('State','District'),with=F], by = 'DISTNAME', all.x=T)

# Carry forward last obs if NA (by group = DISTNAME)
valcols <- c('Confirmed','Recovered','Deceased')
caseapr[, (valcols) := na.locf(.SD, na.rm = F), by = DISTNAME, .SDcols = valcols]
casejun[, (valcols) := na.locf(.SD, na.rm = F), by = DISTNAME, .SDcols = valcols]

# If Confirmed = NA, make all 0
caseapr[, (valcols):= ifelse(is.na(Confirmed),0,Confirmed)]
casejun[, (valcols):= ifelse(is.na(Confirmed),0,Confirmed)]

caseapr <- merge(caseapr, distlat, by = 'DISTNAME')
casejun <- merge(casejun, distlat, by = 'DISTNAME')
totaldists <- merge(totaldists, distlat, by = 'DISTNAME')

distpts <- totaldists %>% st_as_sf(coords = c("Longitude", "Latitude"),agr = "constant",crs = 4326, stringsAsFactors = FALSE,remove = TRUE)
distpts <- st_join(distpts, distshp, join = st_within)
mapdist <- data.table(distpts[c("DISTNAME","ID_2")])

caseapr <- merge(caseapr, mapdist[,.(DISTNAME,ID_2)], by = 'DISTNAME')
casejun <- merge(casejun, mapdist[,.(DISTNAME,ID_2)], by = 'DISTNAME')

# caseapr <- caseapr %>% st_as_sf(coords = c("Longitude", "Latitude"),agr = "constant",crs = 4326, stringsAsFactors = FALSE,remove = TRUE)

caseapr <- rbindlist(list(data.table(ID_2=setdiff(distshp$ID_2,caseapr$ID_2)),caseapr), fill = TRUE)
casejun <- rbindlist(list(data.table(ID_2=setdiff(distshp$ID_2,casejun$ID_2)),casejun), fill = TRUE)

# If Confirmed = NA, make all 0
caseapr[, (valcols):= ifelse(is.na(Confirmed),0,Confirmed)]
casejun[, (valcols):= ifelse(is.na(Confirmed),0,Confirmed)]


# Add ID_2
aprshape <- sp::merge(distshp, caseapr, by = 'ID_2', all.x = T, all.y = F)
aprshape[aprshape$NAME_1 %like% 'And' & aprshape$Confirmed==0,]$Confirmed <- 100

junshape <- sp::merge(distshp, casejun, by = 'ID_2', all.x = T, all.y = F)
junshape[junshape$NAME_1 %like% 'And' & junshape$Confirmed==0,]$Confirmed <- 1000
junshape[junshape$Confirmed==0,]$Confirmed <- 15


# st_write(aprshape,"aprshape_new.shp")
# st_write(junshape,"junshape_new.shp")

# geojsonio::geojson_write(aprshape, file = "aprshape.geojson")
# geojsonio::geojson_write(junshape, file = "junshape.geojson")


# india_sh_1 <- st_as_sf(getData('GADM', country='IND', level=1))
# india_sh_2 <- st_as_sf(getData('GADM', country='IND', level=2))
# india_sh_3 <- st_as_sf(getData('GADM', country='IND', level=3))

# ggplot(subset(aprshape,NAME_1=='Andhra Pradesh'), aes(fill = Confirmed)) +
#   geom_sf() +
#   scale_fill_viridis_c() +
#   ggthemes::theme_map()

# Mapbox Styles
style_url_1 <- "mapbox://styles/xbsd/ck0v1nhc40jzx1cmw74h9f02l"
style_url_2 <- "mapbox://styles/xbsd/ck0v1s2ddh3u71dmvn90e42rq"
style_url_3 <- "mapbox://styles/mapbox/streets-v9"
style_url_4 <- "mapbox://styles/mapbox/dark-v10"
style_url_5 <- "mapbox://styles/mapbox/navigation-preview-night-v4"
style_url_6 <- "mapbox://styles/xbsd/ck0v217pn0eom1coe3styunw7"
style_url_7 <- "mapbox://styles/xbsd/cke970sjp0p4r19ny1jk9j9gz"
style_url_8 <- "mapbox://styles/xbsd/ckea0xpqv37yb19qujus2avi5"
style_url_9 <- "mapbox://styles/xbsd/ckea1kbrp0zn119pkdg7z5rvx"
style_url_10 <- "mapbox://styles/xbsd/ckeb8usr1112n1aql2wejg15p"
style_url_bg <- "mapbox://styles/xbsd/ckeba4el10o3f19rmobslsusp"
style_url_lines <- "mapbox://styles/xbsd/ckeb9il250e1s19qrjh0ezea5"
custom_mapbox_url <- "mapbox://styles/xbsd/ck0v1nhc40jzx1cmw74h9f02l/draft"

# April Shape
aprx <- aprshape

aprbins <- c(0, 5, 10, 25, 50, 100, 250, 500, 1000, Inf)
aprpal <- colorBin("Reds", domain = aprx$Confirmed, bins = aprbins)

labels <- sprintf(
  "<strong>%s</strong><br/>District: %s, Confirmed: %.1f",
  aprx$State, aprx$District, aprx$Confirmed
) %>% lapply(htmltools::HTML)

leaflet(aprx) %>%
  # addMapboxGL(style = custom_mapbox_url) %>%
  # addTiles() %>%
  addMapPane("chorodata", zIndex = 420) %>%
  addMapPane("background", zIndex = 410) %>%
  addMapPane("lines", zIndex = 430) %>%
  addPolygons(
    options = pathOptions(pane = "chorodata"),
    fillColor = ~aprpal(Confirmed),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 1,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>% 
  addMapboxGL(style = style_url_bg, options = pathOptions(pane = "background")) %>%
  addMapboxGL(style = style_url_lines, options = pathOptions(pane = "lines")) %>%
  addLegend("bottomright", pal = aprpal, values = ~Confirmed,
            title = "Confirmed Cases",
            opacity = 1, 
            labFormat = function(type, cuts, p) {
              n = length(cuts)
              paste0(sprintf("%.2f", round(cuts[-n],1)), " &ndash; ", sprintf("%.2f", round(cuts[-1],1)))
            }
  )



# June Shape
jun1 <- junshape

junbins <- c(0, 1, 29,62,97,138,203,272,400,7830,87361)
junpal <- colorBin("Reds", domain = jun1$Confirmed, bins = junbins)

labels <- sprintf(
  "<strong>%s</strong><br/>District: %s, Confirmed: %.1f",
  jun1$State, jun1$District, jun1$Confirmed
) %>% lapply(htmltools::HTML)



leaflet(jun1) %>%
  # addMapboxGL(style = custom_mapbox_url) %>%
  # addTiles() %>%
  addMapPane("chorodata", zIndex = 420) %>%
  addMapPane("background", zIndex = 410) %>%
  addMapPane("lines", zIndex = 430) %>%
  addPolygons(
    options = pathOptions(pane = "chorodata"),
    fillColor = ~aprpal(Confirmed),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 1,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>% 
  addMapboxGL(style = style_url_bg, options = pathOptions(pane = "background")) %>%
  addMapboxGL(style = style_url_lines, options = pathOptions(pane = "lines")) %>%
  addLegend("bottomright", pal = aprpal, values = ~Confirmed,
            title = "Confirmed Cases",
            opacity = 1, 
            labFormat = function(type, cuts, p) {
              n = length(cuts)
              paste0(sprintf("%.2f", round(cuts[-n],1)), " &ndash; ", sprintf("%.2f", round(cuts[-1],1)))
            }
  )

