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
#View(precincts)

districts <- st_read('RedistrictedCouncilDistricts2022.shp') %>%
  st_transform(crs = 4326)

districts <- districts %>% arrange(as.numeric(District))

old_districts <- st_read('CouncilDistricts.shp') %>%
  st_transform(crs = 4326) %>%
  st_make_valid()

# Spatial join precincts & districts
precincts <- st_join(precincts, districts['District'])
precincts <- precincts %>% filter(!is.na(District))

# Load election data & turnout data
# 2021
may2021general <- read_csv('may2021general_cleaned.csv') %>%
  mutate(ElectionYear = 2021, ElectionType = 'General')
june2021runoff <- read_csv('june2021runoff_cleaned.csv') %>%
  mutate(ElectionYear = 2021, ElectionType = 'Runoff')
elections_2021 <- bind_rows(may2021general, june2021runoff) %>%
  mutate(
    `Total Votes` = replace_na(`Total Votes`, 0),
    `Vote Percentage` = replace_na(`Vote Percentage`, 0)
  )
voter_turnout2021general <- read_csv('voter_turnout2021general.csv') %>%
  mutate(
    `Voter Turnout (%)` = as.numeric(gsub("%", "", `Voter Turnout (%)`)) / 100,
    ElectionYear = 2021, ElectionType = "General"
  )
voter_turnout2021runoff <- read_csv('voter_turnout2021runoff.csv') %>%
  mutate(
    `Voter Turnout (%)` = as.numeric(gsub("%", "", `Voter Turnout (%)`)) / 100,
    ElectionYear = 2021, ElectionType = "Runoff"
  )
voter_turnout_2021 <- bind_rows(voter_turnout2021general, voter_turnout2021runoff)

may2023election <- read_csv('may2023general_clean.csv') %>%
  mutate(ElectionYear = 2023, ElectionType = "General")
may2023election <- may2023election %>% select(-...1)

june2023runoff <- read.csv('june2023runoff_clean.csv') %>%
  mutate(ElectionYear = 2023, ElectionType="Runoff")
june2023runoff <- june2023runoff %>% select(-X)
june2023runoff <- june2023runoff %>%
  rename(`Total Votes` = `Total.Votes`, `Vote Percentage` = `Vote.Percentage`)
elections_2023 <- bind_rows(may2023election, june2023runoff) %>%
  mutate(
    `Total Votes` = replace_na(`Total Votes`, 0),
    `Vote Percentage` = replace_na(`Vote Percentage`, 0)
  )

# Combine election datasets
all_elections <- bind_rows(elections_2021, elections_2023)

# Voter turnout
voter_turnout2023_general <- read_csv('voter_turnout2023.csv')
voter_turnout2023_general <- voter_turnout2023_general %>%
  mutate(`Voter Turnout (%)` = as.numeric(gsub("%", "", `Voter Turnout (%)`)) /100,
         ElectionYear = 2023, ElectionType = "General")

voter_turnout2023_runoff <- read_csv('voter_turnout2023runoff.csv')
voter_turnout2023_runoff <- voter_turnout2023_runoff %>%
  mutate(`Voter Turnout (%)` = as.numeric(gsub("%", "", `Voter Turnout (%)`)) /100,
         ElectionYear = 2023, ElectionType = "Runoff")
voter_turnout2023 <- bind_rows(voter_turnout2023_general, voter_turnout2023_runoff)

# Combine voter turnout datasets
all_voter_turnout <- bind_rows(voter_turnout_2021, voter_turnout2023)
all_voter_turnout <- all_voter_turnout %>%
  left_join(
    precincts %>% select(NAME, District),
    by = c("Precinct" = "NAME")
  )
#View(all_voter_turnout)
# Precinct checks
precinct_district_clean <- read_csv("precinct_district_clean.csv")

precincts_turnout <- precinct_district_clean %>%
  left_join(all_voter_turnout, by=c("NAME" = "Precinct"))


#View(precincts_turnout)

# Create a color palette for voter turnout
turnout_palette <- colorNumeric(
  palette = c("blue", "lightblue", "yellow", "orange", "red"),
  domain = precincts_turnout$`Voter Turnout (%)`,
  na.color = "transparent"
)

# Aggregate Mayor's race results
mayor_results <- all_elections %>%
  filter(Race == 'Mayor') %>%
  group_by(Precinct, ElectionYear, ElectionType) %>%
  summarise(
    Results = paste(
      Candidate, ": ", `Total Votes`, " votes (",
      round(`Vote Percentage`, 1), "%)",
      collapse = "<br>"
    ),
    MaxVoteShare = max(`Vote Percentage`, na.rm = TRUE),
    Winner = Candidate[which.max(`Vote Percentage`)],
    .groups = 'drop'
  )
# View(mayor_results)
mayoral_winners = unique(mayor_results$Winner)

# Mayoral palette
mayoral_palette <- colorFactor(
  palette = brewer.pal(min(length(mayoral_winners), 12), "Set3"),
  domain = mayoral_winners
)
mayor_results <- mayor_results %>%
  mutate(WinnerColor = mayoral_palette(Winner))


#Aggregate City Council results
council_results <- all_elections %>%
  filter(str_detect(Race, 'District')) %>%
  mutate(
    District = as.numeric(str_extract(Race, "\\d+"))
  ) %>%
  group_by(Precinct, District, ElectionYear, ElectionType) %>%
  summarise(
    Results = paste(
      Candidate, ": ", `Total Votes`, " votes (",
      round(`Vote Percentage`, 1), "%)",
      collapse = "<br>"
    ),
    MaxVoteShare = max(`Vote Percentage`, na.rm = TRUE),
    Winner = Candidate[which.max(`Vote Percentage`)],
    .groups = 'drop'
  )

council_winners <- unique(council_results$Winner)
# Council Palette
council_palette <- colorFactor(
  palette = brewer.pal(min(length(council_winners),12), "Set3"),
  domain = council_winners
)
council_results <- council_results %>%
  mutate(WinnerColor = council_palette(Winner))

#View(council_results)

# Voter turnout
valid_precincts <- unique(mayor_results$Precinct)
all_voter_turnout <- all_voter_turnout %>%
  filter(Precinct %in% valid_precincts)
#View(voter_turnout2023)

precincts_turnout <- precincts %>%
  filter(NAME %in% valid_precincts) %>%
  left_join(all_voter_turnout, by = c("NAME" = "Precinct"))


##########################################################################################
## UI ##
ui <- dashboardPage(
  dashboardHeader(title = tags$div(
    style = "white-space: nowrap; overflow: visible; text-overflow: clip;",
    "San Antonio Municipal Elections"
  ), titleWidth = 400),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Mayoral Elections", tabName = "mayor", icon = icon("user-tie")),
      menuItem("City Council Elections", tabName = "council", icon = icon("users")),
      menuItem("Voter Turnout", tabName = "turnout", icon = icon("chart-bar")),
      selectInput("electionYear", "Select Election Year:",
                  choices = unique(all_elections$ElectionYear), selected = 2023),
      selectInput("electionType", "Select Election Type:",
                  choices = unique(all_elections$ElectionType), selected = "General"),
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
      ),
      conditionalPanel(
        condition = "input.tabs == 'turnout'",
        selectInput("turnoutDistrict", "Select Council District:",
                    choices = c("All", unique(districts$District)),
                    selected = "All")
      ),
      menuItem("Voter Turnout by Year", tabName = "turnout_year",
               icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      # Mayoral Race Tab
      tabItem(
        tabName = "mayor",
        fluidRow(
          box(
            title = "Mayoral Election Results",
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
            title = "City Council Election Results",
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
        ),
        fluidRow(
          uiOutput("runoffMessage")
        )
      ),
      # Voter Turnout Tab
      tabItem(
        tabName = "turnout",
        fluidRow(
          box(
            title = "Voter Turnout by Precinct",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            leafletOutput("turnoutMap", height = 600)
          )
        ),
        fluidRow(
          box(
            title = "Precinct Voter Turnout Data",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            dataTableOutput("turnoutTable")
          )
        )
      ),
      tabItem(
        tabName = "turnout_year",
        fluidRow(
          box(
            title = "Voter Turnout by Year",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            dataTableOutput("turnoutComparisonTable")
          )
        )
      )
    )
  )
)
######################################################################################
## Server ##
server <- function(input, output, session) {
  # Reactive selection of district shapefile
  selected_districts <- reactive({
    if (input$electionYear == 2021) {
      validate(need(!is.null(old_districts), "Old district shapefile not loaded"))
      old_districts
    } else {
      validate(need(!is.null(districts), "new districts shp not loaded"))
      districts
    }
  })
  
  # Join precincts with selected districts
  # Join precincts with the selected districts
  precincts_with_districts <- reactive({
    validate(need(!is.null(selected_districts()), "District shapefile not found."))
    st_join(precincts, selected_districts(), join = st_within) %>%
      filter(!is.na(District))
  })
  
  # Filter election results based on user input
  filtered_election_data <- reactive({
    validate(need(!is.null(all_elections), "Election data not loaded."))
    all_elections %>%
      filter(ElectionYear == input$electionYear, ElectionType == input$electionType)
  })
  
  # Filter voter turnout data based on user input
  filtered_turnout_data <- reactive({
    validate(need(!is.null(all_voter_turnout), "Voter turnout data not loaded."))
    all_voter_turnout %>%
      filter(ElectionYear == input$electionYear, ElectionType == input$electionType)
  })
  
  # Mayor's Table
  filteredMayorTableData <- reactive({
    mayor_data <- all_elections %>%
      filter(Race == 'Mayor', ElectionYear == input$electionYear, ElectionType == input$electionType) %>%
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
    council_data <- all_elections %>%
      filter(str_detect(Race, "District"), ElectionYear == input$electionYear, ElectionType == input$electionType) %>%
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
      filter(ElectionYear == input$electionYear, ElectionType == input$electionType)
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
      filter(ElectionYear == input$electionYear, ElectionType == input$electionType)
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
  
  # Reactive data for Voter Turnout Map
  filteredTurnoutData <- reactive({
    precincts_turnout %>%
      filter(ElectionYear == input$electionYear, ElectionType == input$electionType)
  })
  # filteredTurnoutData <- reactive({
  #   if (input$turnoutDistrict == "All") {
  #     precincts_turnout
  #   } else {
  #     precincts_turnout %>%
  #       filter(District == as.numeric(input$turnoutDistrict))
  #   }
  # })
  
  # Reactive data for Voter Turnout Table
  filteredTurnoutTableData <- reactive({
    filteredTurnoutData() %>%
      st_drop_geometry() %>%
      select(
        Precinct = NAME,
        District,
        `Registered Voters`,
        `Ballots Cast`,
        `Voter Turnout (%)`
      ) %>%
      arrange(desc(`Ballots Cast`)) %>%
      mutate(
        `Voter Turnout (%)` = round(`Voter Turnout (%)`*100,1),
        `Ballots Cast` = scales::comma(`Ballots Cast`),
        `Registered Voters` = scales::comma(`Registered Voters`)
      )
  })
  
  # Render Voter Turnout Table
  output$turnoutTable <- renderDataTable({
    filteredTurnoutTableData()
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
        pal = mayoral_palette,
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
  
  # Voter Turnout Map
  output$turnoutMap <- renderLeaflet({
    leaflet(data = filteredTurnoutData()) %>%
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
      # Precinct Turnout
      addPolygons(
        color = 'black',
        weight = 1,
        opacity = 1,
        fillColor = ~turnout_palette(`Voter Turnout (%)`),
        fillOpacity = 0.7,
        popup = ~paste(
          "<strong>Precinct:</strong>", NAME, "<br>",
          "<strong>Turnout Percentage:</strong>", round(`Voter Turnout (%)`*100,1), "%", "<br>",
          "<strong>Registered Voters:</strong>", scales::comma(`Registered Voters`), "<br>",
          "<strong>Total Votes:</strong>", scales::comma(`Ballots Cast`)
        ),
        highlight = highlightOptions(
          color = 'red',
          weight = 2,
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
      ) %>%
      addLegend(
        pal = turnout_palette,
        values = precincts_turnout$`Voter Turnout (%)`,
        position = 'bottomright',
        title = "Voter Turnout",
        labFormat = labelFormat(transform = function(x) round(x * 100,1), suffix = '%')
      )
    
  })
  # Check if the selected district had a runoff election
  runoffMessage <- reactive({
    selected_district <- input$councilDistrict
    selected_year <- input$electionYear
    
    # Filter runoff data for the selected district and year
    has_runoff <- council_results %>%
      filter(
        ElectionType == "Runoff",
        ElectionYear == selected_year,
        District == as.numeric(selected_district)
      ) %>%
      nrow() > 0
    
    if (!has_runoff) {
      paste("District", selected_district, "did not have a runoff election in", selected_year)
    } else {
      NULL
    }
  })
  
  # Render the message
  output$runoffMessage <- renderUI({
    message <- runoffMessage()
    if (!is.null(message)) {
      div(
        style = "color: red; font-weight: bold; margin-top: 10px;",
        message
      )
    }
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
    
  }
  
  turnout_comparison_data <- reactive({
    turnout_2021 <- all_voter_turnout %>%
      filter(ElectionYear == 2021) %>%
      select(Precinct, District, `Registered Voters`, `Ballots Cast`, `Voter Turnout (%)`) %>%
      rename(
        `Registered Voters 2021` = `Registered Voters`,
        `Ballots Cast 2021` = `Ballots Cast`,
        `Voter Turnout 2021 (%)` = `Voter Turnout (%)`
      )
    
    turnout_2023 <- all_voter_turnout %>%
      filter(ElectionYear == 2023) %>%
      select(Precinct, District, `Registered Voters`, `Ballots Cast`, `Voter Turnout (%)`) %>%
      rename(
        `Registered Voters 2023` = `Registered Voters`,
        `Ballots Cast 2023` = `Ballots Cast`,
        `Voter Turnout 2023 (%)` = `Voter Turnout (%)`
      )
    
    # Join 2021 and 2023 data
    comparison <- turnout_2021 %>%
      full_join(turnout_2023, by = c("Precinct", "District")) %>%
      mutate(
        `Difference in Ballots Cast` = `Ballots Cast 2023` - `Ballots Cast 2021`,
        `Difference in Voter Turnout (%)` = round((`Voter Turnout 2023 (%)` - `Voter Turnout 2021 (%)`)*100,2),
        `Voter Turnout 2021 (%)` = `Voter Turnout 2021 (%)` * 100,
        `Voter Turnout 2023 (%)` = `Voter Turnout 2023 (%)` * 100
      )
    
    comparison
  })
  
  output$turnoutComparisonTable <- renderDataTable({
    validate(need(turnout_comparison_data(), "No turnout data available for comparison."))
    turnout_comparison_data()
  })
  
 
}

shinyApp(ui, server)
