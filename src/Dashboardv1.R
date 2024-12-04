### 2023 Election Results Dashboard
library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(RColorBrewer)
install.packages("bs4Dash")
library(bs4Dash)

# Load & Pre-process Data
precincts <- st_read('../shapefiles/Bexar_County_Voter_Precincts.shp') %>%
  st_transform(crs = 4326)
View(precincts)

districts <- st_read('../shapefiles/RedistrictedCouncilDistricts2022.shp') %>%
  st_transform(crs = 4326)
View(districts)

may2023election <- read_csv('~/Documents/Repos/COSA-Elections/data/satx2023_generalelection_002.csv') %>%
  mutate(ElectionYear = 2023)
View(may2023election)

# Aggregate Mayor's race results
mayor_results <- may2023election %>%
  filter(Race == "Mayor") %>%
  arrange(Precinct, desc(`Total Votes`)) %>%
  group_by(Precinct, ElectionYear) %>%
  summarise(
    Winner = Candidate[which.max(`Total Votes`)],
    MaxVoteShare = max(`Vote Percentage`, na.rm = TRUE),
    Results = paste(
      Candidate, ": ", `Total Votes`, " votes (", round(`Vote Percentage`,1), "%)",
      collapse = "<br>"
    ),
    .groups = "drop"
  )
mayoral_winners = unique(mayor_results$Winner)
mayoral_palette = colorFactor(
  palette = brewer.pal(min(length(mayoral_winners), 12), "Set3"),
  domain = mayoral_winners
)

# Aggregate City Council results
council_results <- may2023election %>%
  filter(str_detect(Race, "District")) %>%
  mutate(District = as.numeric(str_extract(Race, "\\d+"))) %>%
  arrange(Precinct, desc(`Total Votes`)) %>%
  group_by(Precinct, District, ElectionYear) %>%
  summarise(
    Winner = Candidate[which.max(`Total Votes`)],
    MaxVoteShare = max(`Vote Percentage`, na.rm = TRUE),
    Results = paste(
      Candidate, ": ", `Total Votes`, " votes (", round(`Vote Percentage`,1), "%)",
      collapse = "<br>"
    ),
    .groups = "drop"
  )
council_winners = unique(council_results$Winner)
council_palette = colorFactor(
  palette = brewer.pal(min(length(council_winners), 12), "Set3"),
  domain = council_winners
)

## UI ##
ui <- bs4DashPage(
  title = "San Antonio Election Results",
  header = bs4DashNavbar(
    skin = "dark",
    title = "Election Results Dashboard",
    compact = TRUE
  ),
  sidebar = bs4DashSidebar(
    skin = 'dark',
    status = 'primary',
    title = 'Filters',
    brandColor = 'primary',
    selectInput('electionYear', "Select Election Year:",
                choices = unique(may2023election$ElectionYear), selected = 2023),
    conditionalPanel(
      condition = "input.tab == 'Council Results'",
      selectInput('councilDistrict', "Select Council District:",
                  choices = c("All", 1:10))
    )
  ),
  body = bs4DashBody(
    bs4TabItems(
      bs4TabItem(
        tabName = "mayor",
        bs4Card(
          title = "Mayor's Race Results",
          status = "primary",
          width = 12,
          leafletOutput("mayorMap", height = 600)
        )
      ),
      bs4TabItem(
        tabName = "council",
        bs4Card(
          title = "Council Results",
          status = "primary",
          width = 12,
          leafletOutput("councilMap", height = 600)
        )
      )
    )
  )
)


