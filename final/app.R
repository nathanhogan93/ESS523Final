# app.R
library(shiny)
library(shinydashboard)
library(rgbif)
library(tidyverse)
library(plotly)
library(DT)
library(leaflet)
library(shinyWidgets)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "GBIF Bird Explorer"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Controls", tabName = "controls", icon = icon("sliders-h")),
      menuItem("Visuals", tabName = "visuals", icon = icon("chart-bar"))
    ),
    hr(),
    textInput("country", "Country ISO Code:", value = "US"),
    numericInput("limit", "Max Records to Fetch:", value = 500, min = 100, step = 100),
    sliderInput("top_n", "Number of species to show in plot:", min = 5, max = 10, value = 5, step = 1),
    uiOutput("species_dropdown"),
    actionButton("go", "Fetch GBIF Data", icon = icon("download"), class = "btn-success"),
    width = 250
  ),

  dashboardBody(
    tabItems(
      tabItem(tabName = "visuals",
              fluidRow(
                valueBoxOutput("total_species"),
                valueBoxOutput("total_obs")
              ),
              fluidRow(
                box(title = "Species Occurrence Plot", status = "primary", solidHeader = TRUE,
                    width = 12, plotlyOutput("speciesPlot"))
              ),
              fluidRow(
                box(title = "Species Occurrence Map", status = "success", solidHeader = TRUE,
                    width = 12, leafletOutput("speciesMap", height = "600px"))
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

  # Dynamic species dropdown with multiple selection
  output$species_dropdown <- renderUI({
    df <- bird_data()
    req(df)
    species_choices <- df %>%
      distinct(scientificName, commonName) %>%
      mutate(label = paste0(commonName, " (", scientificName, ")")) %>%
      arrange(label)

    pickerInput("species_select", "Select Species:",
                choices = species_choices$label,
                options = list(`live-search` = TRUE, `actions-box` = TRUE),
                multiple = TRUE)
  })

  # Filtered species counts based on selection
  species_counts <- reactive({
    df <- bird_data()
    req(df)

    if (!is.null(input$species_select) && length(input$species_select) > 0) {
      selected_sci <- str_extract(input$species_select, "\\((.*)\\)$") %>% str_remove_all("[()]")
      df <- df %>% filter(scientificName %in% selected_sci)
    }

    df %>%
      count(scientificName, commonName, name = "count") %>%
      arrange(desc(count))
  })

  # Filtered data for map
  filtered_data_map <- reactive({
    df <- bird_data()
    req(df)

    if (!is.null(input$species_select) && length(input$species_select) > 0) {
      selected_sci <- str_extract(input$species_select, "\\((.*)\\)$") %>% str_remove_all("[()]")
      df <- df %>% filter(scientificName %in% selected_sci)
    }
    df
  })

  # Info boxes
  output$total_species <- renderValueBox({
    df <- filtered_data_map()
    req(df)
    valueBox(
      value = n_distinct(df$scientificName),
      subtitle = "Selected Species",
      icon = icon("feather"),
      color = "purple"
    )
  })

  output$total_obs <- renderValueBox({
    df <- filtered_data_map()
    req(df)
    valueBox(
      value = nrow(df),
      subtitle = "Observations",
      icon = icon("binoculars"),
      color = "orange"
    )
  })

  # Species plot (simplified, top 10, hide legend)
  output$speciesPlot <- renderPlotly({
    df <- species_counts()
    req(df)

    df <- df %>%
      slice_max(order_by = count, n = input$top_n)

    p <- ggplot(df, aes(x = reorder(commonName, count), y = count, fill = commonName)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "Species",
           y = "Number of Occurrences") +
      theme_minimal(base_size = 14)

    ggplotly(p) %>%
      layout(showlegend = FALSE)
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
    datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

}

shinyApp(ui, server)


