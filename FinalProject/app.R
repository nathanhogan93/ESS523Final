# app.R
library(shiny)
library(shinydashboard)
library(rgbif)
library(tidyverse)
library(plotly)
library(DT)
library(leaflet)

ui <- dashboardPage(
  dashboardHeader(title = "GBIF Bird Species Explorer"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Controls", tabName = "controls", icon = icon("sliders-h")),
      menuItem("Visuals", tabName = "visuals", icon = icon("chart-bar"))
    ),
    hr(),
    textInput("country", "Country ISO Code:", value = "US"),
    numericInput("limit", "Max Records to Fetch:", value = 2000, min = 100, step = 100),
    uiOutput("species_dropdown"),   # Dynamic dropdown
    actionButton("go", "Fetch GBIF Data", icon = icon("download"))
  ),

  dashboardBody(
    tabItems(
      tabItem(tabName = "visuals",
              fluidRow(
                box(title = "Species Occurrence Plot", status = "primary", solidHeader = TRUE,
                    width = 6, plotlyOutput("speciesPlot", height = "500px")),
                box(title = "Species Occurrence Map", status = "success", solidHeader = TRUE,
                    width = 6, leafletOutput("speciesMap", height = "500px"))
              ),
              fluidRow(
                box(title = "Species Table", status = "info", solidHeader = TRUE,
                    width = 12, DTOutput("speciesTable"))
              )
      )
    )
  )
)

server <- function(input, output, session) {

  # Fetch GBIF data
  bird_data <- eventReactive(input$go, {
    req(input$country)

    aves_key <- 212

    res <- occ_search(
      taxonKey = aves_key,
      country = toupper(input$country),
      hasCoordinate = TRUE,
      limit = input$limit,
      fields = c("scientificName", "vernacularName", "decimalLatitude", "decimalLongitude")
    )

    if(is.null(res$data) || nrow(res$data) == 0) {
      showNotification("No GBIF data found.", type = "error")
      return(NULL)
    }

    df <- res$data %>%
      filter(!is.na(scientificName)) %>%
      select(scientificName, commonName = vernacularName,
             decimalLatitude, decimalLongitude) %>%
      mutate(commonName = ifelse(is.na(commonName), scientificName, commonName))

    df
  })

  # Dynamic species dropdown
  output$species_dropdown <- renderUI({
    df <- bird_data()
    req(df)
    species_choices <- df %>%
      distinct(scientificName, commonName) %>%
      mutate(label = paste0(commonName, " (", scientificName, ")")) %>%
      arrange(label)

    selectInput("species_select", "Select Species:", choices = c("All Species" = "", species_choices$label))
  })

  # Filtered species counts
  species_counts <- reactive({
    df <- bird_data()
    req(df)

    if (!is.null(input$species_select) && input$species_select != "") {
      # Extract scientificName from label "Common Name (scientificName)"
      sci_name <- str_extract(input$species_select, "\\((.*)\\)$") %>% str_remove_all("[()]")
      df <- df %>% filter(scientificName == sci_name)
    }

    df %>%
      count(scientificName, commonName, name = "count") %>%
      arrange(desc(count))
  })

  # Filtered data for map
  filtered_data_map <- reactive({
    df <- bird_data()
    req(df)

    if (!is.null(input$species_select) && input$species_select != "") {
      sci_name <- str_extract(input$species_select, "\\((.*)\\)$") %>% str_remove_all("[()]")
      df <- df %>% filter(scientificName == sci_name)
    }
    df
  })

  # Plot
  output$speciesPlot <- renderPlotly({
    df <- species_counts()
    req(df)

    df <- df %>% mutate(label = ifelse(!is.na(commonName), commonName, scientificName))

    p <- ggplot(df, aes(x = reorder(label, count), y = count)) +
      geom_col(fill = "#1f77b4") +
      coord_flip() +
      labs(title = paste("Bird Species Counts in", toupper(input$country)),
           x = "Species",
           y = "Number of Occurrences") +
      theme_minimal(base_size = 14)

    ggplotly(p) %>% layout(margin = list(l = 200))
  })

  # Map
  output$speciesMap <- renderLeaflet({
    df <- filtered_data_map()
    req(df)

    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        ~decimalLongitude, ~decimalLatitude,
        radius = 5, color = "#ff7f0e", stroke = FALSE, fillOpacity = 0.7,
        popup = ~paste0("<b>", commonName, "</b><br>", scientificName)
      )
  })

  # Table
  output$speciesTable <- renderDT({
    df <- species_counts()
    req(df)
    datatable(df, options = list(pageLength = 10), rownames = FALSE)
  })

}

shinyApp(ui, server)
