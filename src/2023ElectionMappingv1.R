### May 2023 General Election Mapping
library(sf)
library(leaflet)
library(tidyverse)
library(RColorBrewer)
getwd()
# Load shapefile
precincts <- st_read('../shapefiles/Bexar_County_Voter_Precincts.shp')

print(st_geometry(precincts))

# Transform to WGS84
precincts <- st_transform(precincts, crs = 4326)
View(precincts)

# Load City Council Shapefile and transform
districts <- st_read('../shapefiles/RedistrictedCouncilDistricts2022.shp')
districts <- st_transform(districts, crs = 4326)
View(districts)

# District color palette
district_palette <- colorFactor(
  palette = brewer.pal(10, "Set3"),
  domain = districts$District
)

# Read in May 2023 election data
may2023election <- read_csv("~/Documents/Repos/COSA-Elections/data/satx2023_generalelection_002.csv")
View(may2023election)
# Aggregate Election Data by Precinct
election_summary <- may2023election %>%
  group_by(Precinct) %>%
  summarise(
    Results = paste(
      Candidate, ": ", `Total Votes`, " votes (", round(`Vote Percentage`,1), "%)",
      collapse = "<br>"
    )
  )
View(election_summary)

## Join Prep
# Ensure cols match
precincts$NAME <- as.integer(precincts$NAME)

# Join election data to precinct shapefile
precincts_with_results <- precincts %>%
  left_join(election_summary, by=c("NAME" = "Precinct"))

# Map
leaflet(data = precincts_with_results) %>%
  addTiles() %>%
  setView(lng = -98.4936,
          lat = 29.4241,
          zoom = 11) %>%
  # City Council Districts
  addPolygons(
    data = districts,
    color = 'black',
    weight = 1,
    opacity = 1,
    #fillColor = 'white',
    fillColor = ~district_palette(District),
    fillOpacity = 0.5,
    label = ~District,
    labelOptions = labelOptions(noHide = TRUE)
  ) %>%
  addPolygons(
    color = 'black',
    weight = 0.5,
    opacity = 0.8,
    fillColor = 'white',
    fillOpacity = 0.5,
    popup = ~paste(
      "<strong>Precinct:</strong>", NAME, "<br>",
      "<strong>Election Resulst:</strong><br>", Results
    ),
    highlight=highlightOptions(
      color = 'red',
      weight = 2,
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~NAME,
    labelOptions = labelOptions(
      style = list('color' = 'black', 'font-weight'='bold',
                   'background-color' = 'white',
                   'padding' = '5px',
                   'border-radius'= '3px',
                   'box-shadow' = '3px 3px rgba(0,0,0,0.25'),
      textOnly = TRUE,
      direction = 'right',
      opacity = 0.9
    ),
    options = pathOptions(
      cursor = 'pointer'
    )
  )

