### 2023 Election Results Dashboard
library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(shinydashboard)
library(rsconnect)


# Load & Pre-process Data
precincts <- st_read('/Users/jackturek/Documents/Repos/COSA-Elections/shapefiles/Bexar_County_Voter_Precincts.shp') %>%
  st_transform(crs = 4326)
View(precincts)
precincts$NAME <- as.integer(precincts$NAME)

districts <- st_read('/Users/jackturek/Documents/Repos/COSA-Elections/shapefiles/RedistrictedCouncilDistricts2022.shp') %>%
  st_transform(crs = 4326)
View(districts)

may2023election <- read_csv('/Users/jackturek/Documents/Repos/COSA-Elections/data/satx2023_generalelection_002.csv') %>%
  mutate(ElectionYear = 2023)
View(may2023election)

# Spatial join precincts & districts
precincts <- st_join(precincts, districts['District'])
View(precincts)

# Aggregate Mayor's race results
mayor_results <- may2023election %>%
  filter(Race == 'Mayor') %>%
  arrange(Precinct, desc(`Total Votes`)) %>%
  group_by(Precinct, ElectionYear) %>%
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
    WinnerColor = mayoral_palette(Winner)
  )
mayoral_winners = unique(mayor_results$Winner)
mayoral_palette = colorFactor(
  palette = brewer.pal(min(length(mayoral_winners), 12), "Set3"),
  domain = mayoral_winners
)
View(mayor_results)

# Aggregate City Council results
council_results <- may2023election %>%
  filter(str_detect(Race, 'District')) %>%
  mutate(
    District = as.numeric(str_extract(Race, "\\d+"))
  ) %>%
  arrange(Precinct, desc(`Total Votes`)) %>%
  group_by(Precinct, District, ElectionYear) %>%
  summarise(
    Results = paste(
      Candidate, ": ", `Total Votes`, " votes (", round(`Vote Percentage`, 1), "%)",
      collapse = "<br>"
    ), 
    MaxVoteShare = max(`Vote Percentage`, na.rm = TRUE),
    Winner = Candidate[which.max(`Vote Percentage`)],
    .groups = 'drop'
  ) %>%
  mutate(WinnerColor = council_palette(Winner))
council_winners = unique(council_results$Winner)
council_palette = colorFactor(
  palette = brewer.pal(min(length(council_winners), 12), "Set3"),
  domain = council_winners
)
View(council_results)

## UI ##
ui <- dashboardPage(
  dashboardHeader(title = "San Antonio Municipal Election Results"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Mayoral Race", tabName = "mayor", icon = icon("user-tie")),
      menuItem("City Council Race", tabName = "council", icon = icon("users")),
      selectInput("electionYear", "Select Election Year:",
                  choices = unique(mayor_results$ElectionYear), selected = 2023),
      conditionalPanel(
        condition = "input.tabs == 'council'",
        selectInput("councilDistrict", "Select Council District:",
                    choices = c("All", unique(council_results$District)),
                    selected = "All")
      ),
      conditionalPanel(
        condition = "input.tabs == 'mayor'",
        selectInput("mayorDistrict", "Select Council Distsrict:",
                    choices = c("All", unique(districts$District)),
                    selected = "All")
      )
    )
  ),
  dashboardBody(
    tabItems(
      # Mayoral Race Tab
      tabItem(
        tabName = "mayor",
        fluidRow(
          box(
            title = "Mayoral Race Results",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            leafletOutput("mayorMap", height = 600)
          )
        )
      ),
      # Council Results Tab
      tabItem(
        tabName = "council",
        fluidRow(
          box(
            title = "City Council Race Results",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            leafletOutput("councilMap", height = 600)
          )
        )
      )
    )
  )
)

## Server ##
server <- function(input, output, session) {
  
  # Reactive data for Mayor's map
  filteredMayorData <- reactive({
    data <- mayor_results %>%
      filter(ElectionYear == input$electionYear)
    if (input$mayorDistrict != "All") {
      valid_precincts <- precincts %>%
        filter(District == as.numeric(input$mayorDistrict)) %>%
        pull(NAME)
      data <- data %>%
        filter(Precinct %in% valid_precincts) 
    }
    data
  })
  
  # Reactive data for Council map
  filteredCouncilData <- reactive({
    council_data <- council_results %>%
      filter(ElectionYear == input$electionYear)
    if (input$councilDistrict != "All") {
      council_data <- council_data %>%
        filter(District == as.numeric(input$councilDistrict))
    }
    council_data
  })
  
  observe({
  print("Filtered Council Data:")
  print(filteredCouncilData())
  })
  
  # Render Mayoral Map
  output$mayorMap <- renderLeaflet({
    # Join filtered data with precinct shapefile
    map_data <- precincts %>%
      left_join(filteredMayorData(), by=c("NAME" = "Precinct"))%>%
      filter(!is.na(WinnerColor))
    
    leaflet(data=map_data) %>%
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
        fillColor = 'white',
        #fillColor =  ~district_palette(District),
        fillOpacity = 0.1,
        label = ~District,
        labelOptions = labelOptions(noHide = TRUE)
      ) %>%
      # Precincts with mayoral results
      addPolygons(
        color = 'black',
        weight = 1,
        opacity = 1,
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
        values = unique(filteredMayorData()$Winner),
        title = "Winning Candidate",
        opacity = 1
      )
  })
  
  # Render Council Map
  observe({
    print("Filtered Council Data:")
    print(filteredCouncilData())
  })
  
  output$councilMap <- renderLeaflet({
    # Join filtered data with precinct shapefile
    map_data_council <- precincts %>%
      left_join(filteredCouncilData(), by=c("NAME" = "Precinct"))%>%
      mutate(
        District.x = as.numeric(District.x),
        District.y = as.numeric(District.y),
        District = coalesce(District.x, District.y)) %>%
      select(-District.x, District.y) %>%
      filter(!is.na(WinnerColor))
    

    print("Map Data for Council:")
    print(head(map_data_council))
    print(names(map_data_council))

    # Ensure District column exists
    if (!"District" %in% names(map_data_council)) {
      stop("District column missing in map_data_council after the join!")
    }
    
    leaflet(data = map_data_council) %>%
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
        fillColor = 'white',
        #fillColor =  ~district_palette(District),
        fillOpacity = .1,
        label = ~District,
        labelOptions = labelOptions(noHide = TRUE)
      ) %>%
      # Precincts with council results
      addPolygons(
        color = 'black',
        weight = 1,
        opacity = 1,
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
        values = unique(filteredCouncilData()$Winner),
        title = "Winning Candidates",
        opacity = 1
      )
  })
}
shinyApp(ui, server)
