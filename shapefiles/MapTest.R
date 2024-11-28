### Mapping Test
library(sf)
library(leaflet)
library(tidyverse)
library(RColorBrewer)


# Load shapefile
precincts <- st_read('Bexar_County_Voter_Precincts.shp')
print(st_geometry(precincts))

# Transform to WGS84
precincts <- st_transform(precincts, crs = 4326)
View(precincts)

# Load City Council Shapefile and transform
districts <- st_read('RedistrictedCouncilDistricts2022.shp')
districts <- st_transform(districts, crs = 4326)
View(districts)

# District color palette
district_palette <- colorFactor(
  palette = brewer.pal(10, "Set3"),
  domain = districts$District
)

# Basic Map
leaflet(data = precincts) %>%
  addTiles()%>%
  # City Council Districts
  addPolygons(
    data = districts,
    color = 'black',
    weight = 1.3,
    opacity = 1,
    fillColor = ~district_palette(District),
    fillOpacity = 0.5,
    label = ~District,
    labelOptions = labelOptions(noHide = TRUE)
    # highlight = highlightOptions(
    #   weight = 4,
    #   color = 'orange',
    #   bringToFront = TRUE
    # )
  ) %>%
  # Voting Precincts
  addPolygons(
    color = 'blue',
    weight = .5,
    opacity = 0.8,
    fillColor = "white",
    fillOpacity = 0.1,
    label = ~NAME,
    #popup = ~NAME,
    highlight = highlightOptions(
      weight = 3,
      color = 'red',
      fillOpacity = 0.9
    )
  ) 
