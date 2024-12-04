### Mayor & Council Map
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

# Aggregate Election Data by Precinct - Mayor
mayor_results <- may2023election %>%
  filter(Race == 'Mayor') %>%
  arrange(Precinct, desc(`Total Votes`)) %>%
  group_by(Precinct) %>%
  summarise(
    Results = paste(
      Candidate, ": ", `Total Votes`, " votes (", round(`Vote Percentage`, 1), "%)",
      collapse = "<br>"
    ), 
    MaxVoteShare = max(`Vote Percentage`, na.rm = TRUE),
    Winner = Candidate[which.max(`Vote Percentage`)],
    .groups = 'drop'
  ) %>%
  mutate(
    WinnerColor = candidate_palette(Winner)
  )
View(mayor_results)

# Assign colors to precinct winners - Mayoral
mayoral_winners <- unique(mayor_results$Winner)
winner_palette_mayor <- colorFactor(
  palette = brewer.pal(min(length(mayoral_winners), 12),"Set3"),
  domain = mayoral_winners
)
mayor_results <- mayor_results %>%
  mutate(WinnerColor = winner_palette_mayor(Winner))

# Aggregate Election Data by Precinct - Council
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

# Assign distinct color for each precinct winner
winners <- unique(council_results$Winner)
winner_palette <- colorFactor(
  palette = brewer.pal(min(length(winners), 12), "Set3"),
  domain = winners
)
council_results <- council_results %>%
  mutate(WinnerColor = winner_palette(Winner))

## Join Prep
# Ensure cols match
precincts$NAME <- as.integer(precincts$NAME)

# Join mayoral results to precincts
mayor_results_precincts <- precincts %>%
  left_join(mayor_results, by = c("NAME" = "Precinct")) %>%
  filter(!is.na(WinnerColor))

# Join mayoral results to precincts
council_results_precincts <- precincts %>%
  left_join(council_results, by = c("NAME" = "Precinct")) %>%
  filter(!is.na(WinnerColor))


# Mayor Map
leaflet(data = mayor_results_precincts) %>%
  addTiles() %>%
  setView(lng = -98.4936,
          lat = 29.4241,
          zoom = 11) %>%
  # City Council Districts
  addPolygons(
    data = districts,
    color = 'black',
    weight = 6,
    opacity = 2,
    fillColor = 'white',
    #fillColor =  ~district_palette(District),
    fillOpacity = 0.5,
    label = ~District,
    labelOptions = labelOptions(noHide = TRUE)
  ) %>%
  # Precincts with mayoral results
  addPolygons(
    color = 'black',
    weight = 0.5,
    opacity = 0.8,
    fillColor = ~WinnerColor,
    fillOpacity = 1,
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
    pal = winner_palette_mayor,
    values = unique(mayor_results$Winner),
    title = "Winning Candidate",
    opacity = 1
  )

# Map - City Council
leaflet(data = council_results_precincts) %>%
  addTiles() %>%
  setView(lng = -98.4936,
          lat = 29.4241,
          zoom = 11) %>%
  # City Council Districts
  addPolygons(
    data = districts,
    color = 'black',
    weight = 6,
    opacity = 2,
    #fillColor = 'white',
    fillColor = 'white',
    fillOpacity = 0,
    label = ~District,
    labelOptions = labelOptions(noHide = TRUE)
  ) %>%
  # Precincts with council results
  addPolygons(
    color = 'black',
    weight = 0.5,
    opacity = 0.8,
    fillColor = ~WinnerColor,
    fillOpacity = 0.7,
    popup = ~paste(
      "<strong>District:</strong>", District, "<br>",
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
    pal = winner_palette,
    values = unique(council_results$Winner),
    title = "Winning Candidates",
    opacity = 1
  )
