library(osmdata)
library(tidyverse)
library(sf)
library(osmdata)
library(raster)
library(tidyverse)
library(lwgeom)

# To explore keys and values use: https://taginfo.openstreetmap.org/keys
# For coordinates use: http://bboxfinder.com/

#-------------------- Map for Guilherand Granges--------------------------

getbb("guilherand granges")

streets <- getbb("guilherand granges") %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

small_streets <- getbb("guilherand granges") %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

river <- getbb("guilherand granges") %>%
  opq() %>%
  add_osm_feature(key = "waterway", 
                  value = "river") %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "grey",
          size = .8,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#ffbe7f",
          size = .4,
          alpha = .6) +
  geom_sf(data = river$osm_lines,
          inherit.aes = F,
          color = "#7fc0ff",
          size = 1,
          alpha = 0.2) +
  coord_sf(xlim = c(4.845, 4.892), 
           ylim = c(44.907, 44.95),
           expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#282828")
  )

ggsave("GGmap.pdf", width = 6, height = 6)


#---------------- Usk ---------------


getbb("Usk")

streets <- getbb("Usk")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()
streets

small_streets <- getbb("Usk")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

river <- getbb("Usk")%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .8,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .4,
          alpha = .6) +
  geom_sf(data = river$osm_lines,
          inherit.aes = F,
          color = "white",
          size = 1,
          alpha = 0.2) +
  coord_sf(xlim = c(-2.96, -2.85),
           ylim = c(51.65, 51.77),
           expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#ffbe7f")
  )

ggsave("USKmap.pdf", width = 6, height = 6)


#----------------Silverthorne------------

dill <- structure(c(-106.16,  39.53, -106.02, 39.69), .Dim = c(2L,  2L),
                  .Dimnames = list(c("x", "y"), c("min", "max")))


streets <- dill %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()
streets

streets <-  dill %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

small_streets <- dill %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

river <- dill %>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

lake <- dill %>%
  opq()%>%
  add_osm_feature(key = "water") %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .8,
          alpha = .6) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .4,
          alpha = .6) +
  geom_sf(data = river$osm_lines,
          inherit.aes = F,
          color = "grey",
          size = 0.6,
          alpha = 0.2) +
  geom_sf(data = lake$osm_multipolygons,
          inherit.aes = T,
          color = "grey",
          fill="grey",
          size = 0.2,
          alpha = 1) +
  coord_sf(xlim = c(-106.14, -105.98), 
           ylim = c(39.50, 39.71),
           expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#ffbe7f")
  )

ggsave("Silverthornemap.pdf", width = 6, height = 6)

#-------- Durban --------------

getbb("Durban south africa")

streets <- getbb("Durban south africa")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()
streets

small_streets <- getbb("Durban south africa")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

river <- getbb("Durban south africa")%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()


ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .4,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          alpha = .6) +
  geom_sf(data = river$osm_lines,
          inherit.aes = F,
          color = "#7fc0ff",
          size = 0.5,
          alpha = 0.2) +
  coord_sf(xlim = c(30.85  ,31.17),
           ylim = c(-30.02, -29.70),
           expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#ffbe7f")
  )

#Matt------

streets <- getbb("hoedspruit")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()
streets

small_streets <- getbb("hoedspruit")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

river <- getbb("hoedspruit")%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()


ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "#ffbe7f",
          size = .6,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "grey",
          size = .2,
          alpha = .6) +
  geom_sf(data = river$osm_lines,
          inherit.aes = F,
          color = "#7fc0ff",
          size = 0.8,
          alpha = 0.2) +
  coord_sf(xlim = c(30.91 , 31.00),
           ylim = c(-24.42, -24.32),
           expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#282828")
  )

ggsave("matt.pdf", width = 6, height = 6)

#------St Andrews


getbb("st andrews canada")

streets <- getbb("st andrews new brunswick")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()
streets

small_streets <- getbb("st andrews new brunswick")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

river <- getbb("st andrews new brunswick")%>%
  opq()%>%
  add_osm_feature(key = "waterway") %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "grey",
          size = .8,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#ffbe7f",
          size = .4,
          alpha = .6) +
  geom_sf(data = river$osm_lines,
          inherit.aes = F,
          color = "#7fc0ff",
          size = 0.4,
          alpha = 0.2) +
  coord_sf(xlim = c(-67.09, -67.03), 
           ylim = c(45.055, 45.12),
           expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#282828")
  )

ggsave("sandews.pdf", width = 6, height = 6)

ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .8,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .4,
          alpha = .6) +
  geom_sf(data = river$osm_lines,
          inherit.aes = F,
          color = "grey",
          size = 1,
          alpha = 0.2) +
  coord_sf(xlim = c(-67.09, -67.03), 
           ylim = c(45.055, 45.12),
           expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#ffbe7f")
  )

ggsave("sandewsgold.pdf", width = 6, height = 6)

# DICK

getbb("tampere")

streets <- getbb("tampere")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

small_streets <- getbb("tampere")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

river <- getbb("lake lempaala") %>%
  opq()%>%
  add_osm_feature(key = "waterway") %>%
  osmdata_sf()

lake <- getbb("lake lempaala")  %>%
  opq()%>%
  add_osm_feature(key = "water") %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "#ffbe7f",
          size = .8,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#ffbe7f",
          size = .4,
          alpha = .6) +
  geom_sf(data = river$osm_multipolygons,
          inherit.aes = FALSE,
          color = "grey",
          size = 1,
          alpha = 0.6) +
  geom_sf(data = river$osm_lines,
          inherit.aes = F,
          color = "grey",
          fill = "grey",
          size = 1,
          alpha = 1) +
  geom_sf(data = lake$osm_multipolygons,
          inherit.aes = F,
          color = "grey",
          fill = "grey",
          size = 1,
          alpha = 0.6) +
  coord_sf(xlim = c(23.73, 23.793), 
           ylim = c(61.315, 61.365),
           expand = FALSE) +
  geom_sf(data = lake$osm_lines,
          inherit.aes = F,
          color = "grey",
          fill = "grey",
          size = 1,
          alpha = 1) +
  coord_sf(xlim = c(23.73, 23.793), 
           ylim = c(61.315, 61.365),
           expand = FALSE)+
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#282828")
  )

#Fresno

fresno <- structure(c(-119.85, 36.68,-119.609, 36.8), .Dim = c(2L, 
2L), .Dimnames = list(c("x", "y"), c("min", "max"))) 

plane <- fresno %>%
  opq()%>%
  add_osm_feature(key = "aeroway") %>%
  osmdata_sf() 

water <- fresno %>%
  opq()%>%
  add_osm_feature(key = "waterway") %>%
  osmdata_sf()

river <- fresno%>%
  opq()%>%
  add_osm_feature(key = "natural") %>%
  osmdata_sf()

streets <- structure(c(-119.85, 36.66,-119.609, 36.78), .Dim = c(2L, 
2L), .Dimnames = list(c("x", "y"), c("min", "max"))) %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "#ffbe7f",
          size = .8,
          alpha = .8)   +
  geom_sf(data = river$osm_multipolygons,
          inherit.aes = F,
          color = "grey",
          fill = "grey",
          size = 0.5,
          alpha = 0.5) +
  geom_sf(data = water$osm_multipolygons,
          inherit.aes = F,
          color = "grey",
          fill = "grey",
          size = 0.5,
          alpha = 0.5) +
  geom_sf(data = plane$osm_lines,
          inherit.aes = F,
          color = "grey",
          fill = "grey",
          size = 0.5,
          alpha = 1) +
  coord_sf(xlim = c(-119.9, -119.6),
           ylim = c( 36.60,36.83),
           expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#282828")
  )

ggsave("fresno.pdf", width = 6, height = 6)

#----- Template-------

map <- function(tt) {
  getbb(tt)
}

plane <- map %>%
  opq()%>%
  add_osm_feature(key = "aeroway") %>%
  osmdata_sf() 

water <- map %>%
  opq()%>%
  add_osm_feature(key = "waterway") %>%
  osmdata_sf()

river <- map%>%
  opq()%>%
  add_osm_feature(key = "natural") %>%
  osmdata_sf()

streets <- map %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

small_streets <- map %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "#ffbe7f",
          size = .8,
          alpha = .8)   +
  geom_sf(data = river$osm_multipolygons,
          inherit.aes = F,
          color = "grey",
          fill = "grey",
          size = 0.5,
          alpha = 0.5) +
  geom_sf(data = water$osm_multipolygons,
          inherit.aes = F,
          color = "grey",
          fill = "grey",
          size = 0.5,
          alpha = 0.5) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#ffbe7f",
          size = .4,
          alpha = .6) +
  geom_sf(data = plane$osm_lines,
          inherit.aes = F,
          color = "grey",
          fill = "grey",
          size = 0.5,
          alpha = 1) +
  coord_sf(xlim = round(map[1:2], 2),
           ylim = round(map[1:2], 2),
           expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#282828")
  )

ggsave("map.pdf", width = 6, height = 6)


# Western Cape
map <- getbb("Western Cape")

plane <- map %>%
  opq()%>%
  add_osm_feature(key = "aeroway") %>%
  osmdata_sf() 

river <- map%>%
  opq()%>%
  add_osm_feature(key = "waterway",
                  value = "river") %>%
  osmdata_sf()

streets <- map %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

coast <- map%>%
  opq()%>%
  add_osm_feature(key = "natural",
                  value=c("coastline")) %>%
  osmdata_sf()


ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "#ffbe7f",
          size = .8,
          alpha = .4)   +
  geom_sf(data = river$osm_multipolygons,
          inherit.aes = F,
          color = "grey",
          fill = "grey",
          size = 0.5,
          alpha = 0.5) +
  geom_sf(data = water$osm_multipolygons,
          inherit.aes = F,
          color = "grey",
          fill = "grey",
          size = 0.5,
          alpha = 0.5) +
  geom_sf(data = coast$osm_lines,
          color = "#ffbe7f",
          size = .4,
          alpha = .6) +
  coord_sf(xlim = c(18.2, 19.12),
           ylim = c(-33.3, -34.4),
           expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#282828")
  )

ggsave("Cape.pdf", width = 6, height = 6)

#Grenoble

map <- getbb("Grenoble France")

water <- map %>%
  opq()%>%
  add_osm_feature(key = "waterway") %>%
  osmdata_sf()

river <- map%>%
  opq()%>%
  add_osm_feature(key = "natural") %>%
  osmdata_sf()

streets <- map %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

small_streets <- map %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "#ffbe7f",
          size = .8,
          alpha = .8)   +
  geom_sf(data = river$osm_multipolygons,
          inherit.aes = F,
          color = "grey",
          fill = "grey",
          size = 0.5,
          alpha = 0.5) +
  geom_sf(data = water$osm_multipolygons,
          inherit.aes = F,
          color = "grey",
          fill = "grey",
          size = 0.5,
          alpha = 0.5) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#ffbe7f",
          size = .4,
          alpha = .6) +
  coord_sf(xlim = c(5.667,  5.758),
           ylim = c(45.146, 45.224),
           expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#282828")
  )

ggsave("Grenoble.pdf", width = 6, height = 6)

#-----------------------London map ---------------------------------
# London coordinates: -0.200894,51.448607,0.046299,51.533686 (using http://bboxfinder.com/)
london_coor <- c(-0.200894,51.448607,0.046299,51.533686)

streets <-  opq(london_coor) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary",
                            "residential")) %>%
  osmdata_sf()

water_osm <- opq(london_coor) %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf() %>% 
  unname_osmdata_sf()

river_osm <- opq(london_coor) %>%
  add_osm_feature(key = "waterway", value = c("river", "riverbank",
                                              "stream", "ditch", "canal",
                                              "dam", "weir")) %>%
  osmdata_sf() %>% 
  unname_osmdata_sf()

water <- c(water_osm, river_osm) %>% 
  .$osm_multipolygons %>% 
  dplyr::select(osm_id, name) %>% 
  mutate(area = st_area(.)) %>% 
  # this filter gets rid of tiny isolated lakes et cetera
  filter(area >= quantile(area, probs = 0.75))

# Add points
dot <- data.frame(x =  c(51.48618435476622,
                         51.47251645371745,
                         51.52035454910605),  
                  y = c(-0.18407765960294045,
                        -0.15971343976518346,
                        -0.09301392574292187))

# Map
ggplot() +
  geom_sf(data = streets$osm_lines %>%
            filter(highway == "primary"),
          inherit.aes = FALSE,
          color = "#ffbe7f",
          size = .6,
          alpha = .8) + 
  geom_sf(data = streets$osm_lines %>%
            filter(highway == "residential"),
          inherit.aes = FALSE,
          color = "white",
          size = .2,
          alpha = .2)   +
  #Need to add multipol and pol for the Themes
  geom_sf(data = water_osm$osm_polygons,
          inherit.aes = F,
          color = "grey",
          fill = "grey",
          size = 0.5,
          alpha = 0.5) +
  geom_sf(data = water_osm$osm_multipolygons,
          inherit.aes = F,
          color = "grey",
          fill = "grey",
          size = 0.5,
          alpha = 0.5) +
  geom_point(data = dot, aes(y, x), col= "darkred", size= 1) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#282828")
  )  +
  coord_sf(xlim  = c(-0.30, 0.14),
           ylim = c(51.42 , 51.565))

#-----------------------Singapore map ---------------------------------
#Get singapore coordinates
singapore_cor <- getbb("Singapore")

#Select objects to map
streets <- opq(singapore_cor) %>%
  add_osm_feature(
    key = "highway", 
    value = c("motorway", "primary", 
              "secondary", "tertiary",
              "residential")
  ) %>%
  osmdata_sf()

island <-  opq(singapore_cor) %>%
  add_osm_feature(key = "place", 
                  value = c("island")) %>%
  osmdata_sf()

water_osm <- opq(singapore_cor) %>%
  add_osm_feature(key = "natural", 
                  value = "water") %>%
  osmdata_sf() %>% 
  unname_osmdata_sf()

river_osm <- opq(singapore_cor) %>%
  add_osm_feature(
    key = "waterway", 
    value = c("river", "riverbank", "stream", 
              "ditch", "canal",
              "dam", "weir")
  ) %>%
  osmdata_sf() %>% 
  unname_osmdata_sf()

water <- c(water_osm, river_osm) %>% 
  .$osm_multipolygons %>% 
  dplyr::select(osm_id, name) %>% 
  mutate(area = st_area(.)) %>% 
  # this filter gets rid of tiny isolated lakes et cetera
  filter(area >= quantile(area, probs = 0.75))

# Add points
dot <- data.frame(x =  1.2951259642020152, 
                  y = 103.84675255327105)

# Map
ggplot() +
  geom_sf(data = island$osm_lines %>%
            filter(name == "Coastline of Singapore"),
          inherit.aes = FALSE,
          color = "#ffbe7f",
          size = .4,
          alpha = .8) +
  geom_sf(data = streets$osm_lines %>%
            filter(highway == "primary"),
          inherit.aes = FALSE,
          color = "#ffbe7f",
          size = .6,
          alpha = .8) + 
  geom_sf(data = streets$osm_lines %>%
            filter(highway == "residential"),
          inherit.aes = FALSE,
          color = "white",
          size = .2,
          alpha = .2)   +
  #Need to add multipol and pol for the Themes
  geom_sf(data = water_osm$osm_polygons,
          inherit.aes = F,
          color = "grey",
          fill = "grey",
          size = 0.5,
          alpha = 0.5) +
  geom_sf(data = water_osm$osm_multipolygons,
          inherit.aes = F,
          color = "grey",
          fill = "grey",
          size = 0.5,
          alpha = 0.5) +
  geom_point(data = dot, 
             aes(x = y, 
                 y = x), 
             col= "darkred",
             size= 2) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#282828")) +
  coord_sf(ylim = c(1.00,   1.7))