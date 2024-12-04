### City Council Election Map 2023
## Mayoral Map
### May 2023 General Election Mapping
library(sf)
library(leaflet)
library(tidyverse)
library(RColorBrewer)

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
council_results <- may2023election %>%
  filter(str_detect(Race, 'District')) %>%
  mutate(
    District = as.numeric(str_extract(Race, "\\d+"))
  ) %>%
  arrange(Precinct, desc(`Total Votes`)) %>%
  group_by(Precinct, District) %>%
  summarise(
    Results = paste(
      Candidate, ": ", `Total Votes`, " votes (", round(`Vote Percentage`, 1), "%)",
      collapse = "<br>"
    ), 
    MaxVoteShare = max(`Vote Percentage`, na.rm = TRUE),
    Winner = Candidate[which.max(`Vote Percentage`)],
    .groups = 'drop'
  )
View(council_results)
## Join Prep
# Ensure cols match
precincts$NAME <- as.integer(precincts$NAME)

# Join mayoral results to precincts
council_results_precincts <- precincts %>%
  left_join(council_results, by = c("NAME" = "Precinct")) %>%
  filter(!is.na(MaxVoteShare))
View(council_results_precincts)

# Heatmap color palette
heatmap_palette <- colorNumeric(
  palette = c("blue", "purple", "red"),  # Gradient: Blue -> Purple -> Red
  domain = precincts_with_results$MaxVoteShare
)

# Map
leaflet(data = council_results_precincts) %>%
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
  # Precincts with council results
  addPolygons(
    color = 'black',
    weight = 0.5,
    opacity = 0.8,
    fillColor = ~heatmap_palette(MaxVoteShare),
    fillOpacity = 0.7,
    popup = ~paste(
      "<strong>Precinct:</strong>", NAME, "<br><br>",
      "<strong>Winning Candidate:</strong> ", Winner, "<br>",
      "<strong>Max Vote Share:</strong> ", round(MaxVoteShare, 1), "%", "<br><br>",
      "<strong>All Results:</strong><br>", Results
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
  ) %>%
  addLegend(
    "bottomright",
    pal = heatmap_palette,
    values = mayor_results_precincts$MaxVoteShare,
    title = "Vote Share (%)",
    opacity = 1
  )

