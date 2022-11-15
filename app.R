#LIBRARIES-----------
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(DT)
#READ IN DATA-------------
short <- readRDS(here::here("processed_data", "mpi_shortraw.rds"))
short_last_obs <- short %>%
  group_by(project_id) %>%
  filter(published_dates == max(published_dates))
#USER INTERFACE----------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .selectize-input {
        max-height: 20px;
        font-size: 9pt;
      }
      .control-label {
        font-size: 9pt;
      }"))
  ),
  titlePanel("British Columbia's Major Project Inventory"),
  #SELECTIZE GROUP-------------
  fluidRow(
    column(
      width = 12,
      selectizeGroupUI(
        id = "my-filters",
        inline = TRUE,
        params = list(
          var_one = list(inputId = "estimated_cost", title = "Estimated Cost (M)", placeholder = "select"),
          var_two = list(inputId = "project_name", title = "Project Name", placeholder = "select"),
          var_three = list(inputId = "region", title = "Region", placeholder = "select"),
          var_four = list(inputId = "municipality", title = "Municipality", placeholder = "select"),
          var_five = list(inputId = "project_type", title = "Project Type", placeholder = "select"),
          var_six = list(inputId = "project_category", title = "Project Category", placeholder = "select"),
          var_seven = list(inputId = "construction_type", title = "Construction Type", placeholder = "select"),
          var_eight = list(inputId = "construction_subtype", title = "Construction Subtype", placeholder = "select"),
          var_nine = list(inputId = "public_funding", title = "Public Funding", placeholder = "select"),
          var_ten = list(inputId = "green_building", title = "Green Building", placeholder = "select"),
          var_eleven = list(inputId = "clean_energy", title = "Clean Energy", placeholder = "select"),
          var_twelve = list(inputId = "indigenous", title = "Indigenous", placeholder = "select"),
          var_thirteen = list(inputId = "project_id", title = "Project ID", placeholder = "select")
        )
      )
    )
  ),
  #TABLE OUTPUT----------------------
  fluidRow(
    column(12, DT::DTOutput("table"))
  ),
  #TEXT OUTPUT------------------
  fluidRow(
    column(12, uiOutput("text"), style = "padding-bottom:10px")
  ),
  fluidRow(
    #MAP OUTPUT-------------
    column(4, leafletOutput("map")),
    #STATUS PLOT---------------
    column(4, plotOutput("status")),
    #STAGE PLOT
    column(4, plotOutput("stage"))
  )
)

server <- function(input, output, session) {

#APPLIES FILTERS TO DATA------------
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = short_last_obs,
    vars = c(
      "estimated_cost",
      "project_name",
      "region",
      "municipality",
      "project_type",
      "project_category",
      "construction_type",
      "construction_subtype",
      "public_funding",
      "green_building",
      "clean_energy",
      "indigenous",
      "project_id"
    )
  )
#TIDY UP THE FILTERED DATA-------------------
  filtered <- reactive({
    res_mod() %>%
      select(
        estimated_cost,
        project_name,
        region,
        municipality,
        project_type,
        project_category,
        construction_type,
        construction_subtype,
        public_funding,
        green_building,
        clean_energy,
        indigenous,
        project_id
      ) %>%
      arrange(desc(estimated_cost))
  })
#PROJECT NUMBER ASSOCIATED WITH ROW SELECTED-------------------
  project_selected <- reactive({
    filtered()[input$table_rows_selected, "project_id"]
  })
#ALL OBSERVATIONS OF SELECTED PROJECT-------------------------
  selected_data <- reactive({
    short %>% filter(project_id == project_selected())
  })
#EXTRACT THE LAST OBSERVATION OF A GIVEN VARIABLE-----------------
  get_var <- function(var) {
    selected_data() %>%
      pull({{ var }}) %>%
      last()
  }
#RENDER THE TABLE---------------
  output$table <- DT::renderDT({
    df <- filtered()
    colnames(df) <- str_to_title(str_replace_all(colnames(df), "_", " "))
    df <- df %>%
      rename(`Estimated Cost (M)` = `Estimated Cost`)
    datatable(df,
      colnames = rep("", ncol(df)),
      selection = list(mode = "single", selected = 1),
      options = list(
        columnDefs = list(
          list(width = "100px", targets = "_all"),
          list(className = "dt-center", targets = "_all")
        ),
        autoWidth = FALSE,
        ordering = F,
        dom = "tp",
        pageLength = 5,
        server = TRUE,
        selection = "single"
      ),
      rownames = FALSE
    )
  })
#RENDER THE TEXT----------
  output$text <- shiny::renderUI({
    HTML(paste0(
      "<b>",
      get_var(project_name),
      ":</b> ",
      get_var(project_description),
      "<b> Developer:</b> ",
      get_var(developer),
      "<b> Phone:</b> ",
      get_var(telephone)
    ))
  })
#RENDER THE MAP-----------------
  output$map <- renderLeaflet({
    leafMap <- leaflet(selected_data()) %>%
      setView(lat = 55, lng = -120, zoom = 4) %>%
      addTiles() %>%
      addMarkers(~lon, ~lat, popup = ~paste("Approximate location of ", project_name), label = ~municipality) %>%
      addProviderTiles(providers$Esri.WorldStreetMap)
    leafMap
  })
#RENDER STATUS PLOT---------------
  output$status <- renderPlot({
    ggplot(selected_data(), aes(last_update, project_status)) +
      geom_point() +
      labs(
        title = "Project Status",
        x = "",
        y = ""
      ) +
      theme(text = element_text(size = 20))
  })
#RENDER THE STAGE PLOT---------------
  output$stage <- renderPlot({
    ggplot(selected_data(), aes(last_update, project_stage)) +
      geom_point() +
      labs(
        title = "Project Stage",
        x = "",
        y = ""
      ) +
      theme(text = element_text(size = 20))
  })
}

shinyApp(ui, server)
