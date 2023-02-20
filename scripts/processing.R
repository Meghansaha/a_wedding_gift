#==== "A Wedding Gift" - Creating Circular OSM Maps with osmdata ====#

#=============================================================================#
# Library Load-In--------------------------------------------------------------
#=============================================================================#
library(sf) #For getting "simple features" -It makes my maps go brrr in ggplot
library(tidyverse) #For Everything data (dplyr and ggplot specifically)
library(osmdata) #For geodata and mapping
library(showtext) # For using pretty Google fonts
library(ggtext) #For easier text aesthetics
library(glue) #Also for easier text aesthetics

#=============================================================================#
#Font Import-------------------------------------------------------------------
#=============================================================================#

#Adding a pretty font from Google#
font_add_google("La Belle Aurore")
font_add_google("Inter")
showtext_auto()

#=============================================================================#
#Map Options-------------------------------------------------------------------
#=============================================================================#

#Map's Place of Interest (POI - Center)----------------------------------------
POI = c(
  lat = 32.03825,
  long = -81.1716
)

#Map's bounding Box "buffer" size----------------------------------------------
#Helps create the bounds for the entire map#
bb_buff <- .06000

#Map's Bounding Box (mbb)------------------------------------------------------
mbb = c(
  minlong = POI["long"] - bb_buff, 
  minlat = POI["lat"] - bb_buff,
  maxlong = POI["long"] + bb_buff, 
  maxlat = POI["lat"] + bb_buff
) |>
  unname()

#Specific OSM features desired-------------------------------------------------
feature_names <- c(
  "buildings",
  "landuse",
  "leisure",
  "natural"
)

feature_keys <- c(
  "building",
  "landuse",
  "leisure",
  "natural"
)

#Personally am doing water/rivers separately out of necessity (won't map otherwise for me)#
#My bounding box data has inconsistencies#
#Not sure if this is just because of where I'm mapping though#

#OSM feature grabbing and naming-----------------------------------------------
#Native pipe (|>) does not evaluate this properly like magrittr's#
osm_features <- map(feature_keys, ~opq(bbox = mbb) %>%
                      add_osm_feature(key = .x) %>%
                      osmdata_sf() %>%
                      .$osm_polygons) %>%
  set_names(., feature_names) 

#Road features-----------------------------------------------------------------
roads <- opq(bbox = mbb) %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf() %>%
  .$osm_lines

#Combining all OSM features#
osm_features_all <- prepend(osm_features, list("roads" = roads))

#Updating feature names to include "rivers" and "roads#
feature_names <- names(osm_features_all)


#=============================================================================#
#Circle/Center Calculations----------------------------------------------------
#=============================================================================#

#Pulling the CRS---------------------------------------------------------------
#(Coordinate Reference System) from the "roads" polygon object#
crs_data <- unname(st_crs(osm_features_all$roads)$wkt)

#Defining the circular "view" from POI-----------------------------------------
#...and adding a label for the map to mark it#
center_POI <- tibble(long = POI["long"], 
                     lat = POI["lat"], 
                     label = expression("\U2764")) |>
  st_as_sf(coords = c("long", "lat"), crs = crs_data)

#Setting a distance from the center POI----------------------------------------
#Affects how "big" the circle is#
dist <-  800

#Circle "data" used to crop/filter the original OSM data "down to size"--------
circle <- enframe(POI) |>
  pivot_wider() |>
  st_as_sf(coords = c("long", "lat"), crs = crs_data) |>
  st_buffer(dist = dist) |>
  st_transform(crs = crs_data)

#=============================================================================#
#Cropping OSM data to fit circle-----------------------------------------------
#=============================================================================#

#Calculating intersections-----------------------------------------------------
#Native pipe (|>) does not evaluate this properly like magrittr's##
osm_features_cropped <- map(feature_names, ~st_intersection(circle, 
                                                            osm_features_all[[.x]])) |>
  list_rbind()

#=============================================================================#
#Map aesthetic options---------------------------------------------------------
#=============================================================================#
map_color <- "#e9d66b" #Gold/Yellow color
    map_background <- "#022052" #Dark blue/Navy Blue
      outer_ring_color <- "#e9d66b" #Gold/Yellow color
        inner_ring_color <- "#022052" #Dark blue/Navy Blue
          POI_fill <- '#960503' #Red color
            POI_color <- '#000000' #Black
              
            #=============================================================================#
            #Text Options------------------------------------------------------------------
            #=============================================================================#
            title_text <- "The Mackey House"
            caption_text <- glue("Jessica and Chris<br>",
                                 "06 • 10 • 22<br>","Savannah, Ga<br>",
                                 "Lat:<span style='color:",POI_fill,"'>",POI[["lat"]],
                                 "</span>, Long:<span style='color:",POI_fill,"'>",POI[["long"]],
                                 "</span></b>")
            map_font <- "La Belle Aurore"
            
            #=============================================================================#
            #Map Layering/Building---------------------------------------------------------
            #=============================================================================#
            
            #Adding OSM layers to the ggplot-----------------------------------------------
            
            finished_map <- ggplot() +
              theme_void()+
              theme(
                
                plot.background = element_rect(fill = map_background, 
                                               color = NA),
                plot.title = element_textbox_simple(
                  halign = .5, 
                  color = map_color, 
                  family = map_font, 
                  size = 240, 
                  face = 'bold', 
                  fill = NA,
                  height = .1,
                  width = 1.5))+
              geom_sf(data = osm_features_cropped$geometry, 
                      color = map_color, 
                      fill = map_color) +
              geom_text(data = center_POI, aes(POI["long"], POI["lat"], label = label), 
                        color = POI_color, 
                        size = 31,
                        family = "Inter")+
              geom_text(data = center_POI, aes(POI["long"], POI["lat"], label = label), 
                        color = POI_fill, 
                        size = 30,
                        family = "Inter")+
              geom_point(data = center_POI, aes(POI["long"], POI["lat"]), 
                         color = outer_ring_color, 
                         shape = 21, 
                         size = 399, 
                         stroke = 10) +
              geom_point(data = center_POI, aes(POI["long"], POI["lat"]), 
                         color = inner_ring_color, 
                         shape = 21, 
                         size = 403, 
                         stroke = 4) +
              labs(title = title_text) +
              geom_richtext(
                aes(x = POI["long"],
                    y = POI["lat"] - .011),
                label = caption_text, 
                text.color = map_color,
                family = map_font, 
                lineheight = .1,
                size = 50,
                fill = NA,
                hjust = .5,
                label.color = NA)+
              coord_sf(ylim = c(POI["lat"] + .008,POI["lat"] - .012))
            
            #=============================================================================#
            #Final Image Export------------------------------------------------------------
            #=============================================================================#
            ggsave("finished map.png",
                   width = 16, 
                   height = 20, 
                   dpi = 300, 
                   units = "in")
            
            