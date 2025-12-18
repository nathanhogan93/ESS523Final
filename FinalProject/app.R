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
  skin = "green", # dashboard theme
  dashboardHeader(title = "GBIF Bird Explorer"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Controls", tabName = "controls", icon = icon("sliders-h")),
      menuItem("Visuals", tabName = "visuals", icon = icon("chart-bar"))
    ),
    hr(),
    textInput("country", "Country ISO Code:", value = "US"),
    numericInput("limit", "Max Records to Fetch:", value = 2000, min = 100, step = 100),
    uiOutput("species_dropdown"),
    actionButton("go", "Fetch GBIF Data", icon = icon("download"), class = "btn-success"),
    width = 250
  ),

  dashboardBody(
    shinyDashboardThemes(
      theme = "blue_gradient" # nice gradient theme
    ),

    tabItems(
      tabItem(tabName = "visuals",
              fluidRow(
                valueBoxOutput("total_species"),
                valueBoxOutput("total_obs")
              ),
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

    pickerInput("species_select", "Select Species:",
                choices = c("All Species" = "", species_choices$label),
                options = list(`live-search` = TRUE))
  })

  # Filtered species counts
  species_counts <- reactive({
    df <- bird_data()
    req(df)

    if (!is.null(input$species_select) && input$species_select != "") {
      sci_name <- str_extract(inp

