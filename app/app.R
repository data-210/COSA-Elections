### 2023 Election Results Dashboard
library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(shinydashboard)
library(rsconnect)

# Load & Pre-process Data
precincts <- st_read('Bexar_County_Voter_Precincts.shp') %>%
  st_transform(crs = 4326)

precincts$NAME <- as.integer(precincts$NAME)

districts <- st_read('RedistrictedCouncilDistricts2022.shp') %>%
  st_transform(crs = 4326)

districts <- districts %>% arrange(as.numeric(District))

may2023election <- read_csv('satx2023_generalelection_002.csv') %>%
  mutate(ElectionYear = 2023)


# Spatial join precincts & districts
precincts <- st_join(precincts, districts['District'])

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
                    choices = c("All", sort(unique(council_results$District))),
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
        ),
        # Mayoral Candidate Data Table - Vote Share
        fluidRow(
          box(
            title = "Mayoral Race Candidate Vote Share",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            dataTableOutput("mayorTable")
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
        ),
        fluidRow(
          box(
            title = "City Council Candidate Vote Share",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            dataTableOutput("councilTable")
          )
        )
      )
    )
  )
)

## Server ##
server <- function(input, output, session) {
  
  # Mayor's Table
  filteredMayorTableData <- reactive({
    mayor_data <- may2023election %>%
      filter(Race == 'Mayor') %>%
      group_by(Candidate) %>%
      summarise(
        `Total Votes` = sum(`Total Votes`, na.rm = TRUE),
        .groups = "drop"
      )
    total_votes_all_candidates <- sum(mayor_data$`Total Votes`, na.rm=TRUE)
    mayor_data <- mayor_data %>%
      mutate(`Vote Share` = `Total Votes` / total_votes_all_candidates*100) %>%
      arrange(desc(`Total Votes`))
    
    mayor_data
   
   })
  
  # Council Table
  filteredCouncilTableData <- reactive({
    council_data <- may2023election %>%
      filter(str_detect(Race, "District")) %>%
      mutate(
        District = as.numeric(str_extract(Race, "\\d+"))
      )
    # Apply district filter
    if (input$councilDistrict != "All") {
      council_data <- council_data %>%
        filter(District == as.numeric(input$councilDistrict))
    }
    # Aggregate votes and calculate vote share
    council_table <- council_data %>%
      group_by(Candidate) %>%
      summarise(
        `Total Votes` = sum(`Total Votes`, na.rm = TRUE),
        .groups = "drop"
      )
    total_votes_all_candidates <- sum(council_table$`Total Votes`, na.rm = TRUE)
    council_table <- council_table %>%
      mutate(`Vote Share` = `Total Votes` / total_votes_all_candidates*100) %>%
      arrange(desc(`Total Votes`))
    
    council_table
  })
  
  # Render Mayoral Table
  output$mayorTable <- renderDataTable({
    filteredMayorTableData() %>%
      mutate(
        `Total Votes` = scales::comma(`Total Votes`),
        `Vote Share` = scales::percent(`Vote Share`/100)
      )
  })
  
  # Render Council Table
  output$councilTable <- renderDataTable({
    filteredCouncilTableData() %>%
      mutate(
        `Total Votes` = scales::comma(`Total Votes`),
        `Vote Share` = scales::percent(`Vote Share`/100)
      )
  })
  
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
    # Count precincts won by each candidate
    council_data <- council_data %>%
      group_by(Winner) %>%
      mutate(PrecinctsWon = n()) %>%
      ungroup()
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
        fillOpacity = 0.2,
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
    
    # Prepare legend labels with precincts won
    legend_data <- filteredCouncilData() %>%
      group_by(District, Winner) %>%
      summarise(PrecinctsWon = n(), .groups = "drop") %>%
      left_join(
        filteredCouncilData() %>%
          group_by(District) %>%
          summarise(TotalPrecincts = n_distinct(Precinct), .groups = "drop"),
        by = "District"
      ) %>%
      mutate(Label = paste0(Winner, " (", PrecinctsWon, "/", TotalPrecincts, " precincts won)")) 
    
    # legend_labels <- legend_data$Label
    # legend_values <- legend_data$Winner
    
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
        fillOpacity = 0.2,
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
      addControl(
        html = createCustomLegend(legend_data),
        position = "bottomright")
      })
  # Custom HTML legend function
  createCustomLegend <- function(legend_data) {
    html <- '<div style="background: white; padding: 10px; border-radius: 5px;">'
    html <- paste0(html, '<strong>Winning Candidates:</strong><br>')
    for (i in seq_len(nrow(legend_data))) {
      html <- paste0(html,
                     '<div style="display: flex; align-items: center;">',
                     '<div style="width: 10px; height: 10px; background-color:',
                     council_palette(legend_data$Winner[i]), '; margin-right: 5px;"></div>',
                     legend_data$Label[i], '</div>')
    }
    html <- paste0(html, '</div>')
    return(html)
  
  #     addLegend(
  #       "bottomright",
  #       pal = winner_palette,
  #       values = legend_values,
  #       title = "Winning Candidates",
  #       labels = legend_labels,
  #       opacity = 1
  #     )
  # })
  }
}

shinyApp(ui, server)
