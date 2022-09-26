# Caltrans Accessibility Metric Builder
# Henry.McKay@dot.ca.gov
# Last update: 08/25/2022

# Load packages (will need to be installed first if not already)
library(shiny)
library(shinyWidgets)
library(raster)
library(rgdal)
library(dplyr)
library(DT)
library(data.table)
library(leaflet)
library(leaflet.extras)
library(sf)
library(stats)
library(terra)

### User interface ###

# Tab 1 UI: Parameters
tab1 <- tabPanel(
  title = "Project Analysis",
  sidebarLayout(
    sidebarPanel(
      textInput("project_name", "Project Name", value = "", width = NULL, placeholder = NULL),
      helpText("Upload Conveyal Outputs"),
      fileInput("baseline_tiff", "Baseline Conveyal .TIFF Output", multiple = FALSE),
      fileInput("project_tiff", "Project Conveyal .TIFF Output", multiple = FALSE),
      fileInput("weight_tiff", "Weight .TIFF (optional)", multiple = FALSE),
      fileInput("shapefile_input", "Project Extent Shapefile", multiple = T,
                accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj', '.cpg')),
      numericInput("buffer", "Analysis Area Buffer (miles)", 3),
      download1 <- downloadButton('download1', "Export CSV")
    ),
    mainPanel(
      table1 <- dataTableOutput("table1")
    )
  )
)

# Tab 2 UI: Parameters
tab3 <- tabPanel(
  title = "Map",
  sidebarLayout(
    sidebarPanel(
      selectInput("map_options", "Select Raster to Map", choices = c("Baseline Accessibility", 
                                                                     "Project Accessibility",
                                                                     "Accessibility Change",
                                                                     "Weighted Accessibility Change",
                                                                     "Baseline Accessibility (natural)", 
                                                                     "Project Accessibility (natural)",
                                                                     "Accessibility Change (natural)",
                                                                     "Weighted Accessibility Change (natural)"))
    ),
    mainPanel(
      tags$style(type = "text/css", "#map1 {height: calc(100vh - 100px) !important; }"),
      leafletOutput("map1")
    )
  )
)

# Assemble UI
ui <- navbarPage(
  title = "Caltrans Accessibility Metrics Post-Processing Tool",
  tab1,
  tab3
)

### END User Interface ###

### Server ###

server <- function(input, output, session) {
  
  # Shapefile upload (if the input was a GEOJSON, KML, etc, this would be one line of code)
  project_extent <- reactive({
    project_input <- input$shapefile_input
    tempdirname <- dirname(project_input$datapath[1])
    
    # Rename files
    for (i in 1:nrow(project_input)) {
      file.rename(
        project_input$datapath[i],
        paste0(tempdirname, "/", project_input$name[i])
      )
    }
    
    # Read shapefile from upload
    project_extent <- readOGR(paste(tempdirname,
                         project_input$name[grep(pattern = "*.shp$", project_input$name)],
                         sep = "/"
    ))
    project_extent <- st_as_sf(project_extent)
    project_extent <- st_transform(project_extent, crs = 4326)
  })
  
  # Calculate accessibility metrics
  eval_table <- reactive({
    
    # Validate input .tiff files
    validate(
      need(input$baseline_tiff != "", "Please upload baseline .tiff file"),
      need(input$project_tiff != "", "Please upload a project .tiff file"),
      need(input$shapefile_input != "", "Please upload a project shapefile")
    )
    
    # Read in baseline and build .TIFF files
    baseline <- raster(input$baseline_tiff$datapath)
    build <- raster(input$project_tiff$datapath)
    
    ### Natural Study Area ##########################################################################
    # Compute change in accessibility
    difference_natural <- build - baseline
    
    # Calculate adjusted baseline
    baseline_adj <- baseline
    baseline_adj[] = ifelse(difference_natural[]!=0, baseline[], NA)
    baseline_num_natural <- sum(values(baseline_adj), na.rm = T)
    
    # Calculate adjusted build
    build_adj <- build
    build_adj[] <- ifelse(difference_natural[]!=0, build[], NA)
    build_num_natural <- sum(values(build_adj), na.rm = T)
    
    # Compute % change in accessibility
    diff_pct_natural <- ((build_num_natural - baseline_num_natural) / baseline_num_natural)
    
    # Compute average change in accessibility
    avg_diff_natural <- mean((values(build_adj) - values(baseline_adj)), na.rm = T)
    
    #################################################################################################
    
    # Read in project shapefile and apply buffer at specified distance
    project_extent <- st_as_sf(project_extent())
    project_extent <- st_transform(project_extent, crs = 3857)
    buffer <- st_buffer(project_extent, (input$buffer * 1609.34)) # Convert input (in miles) to meters
    buffer_diss <- st_as_sf(st_union(buffer))
    
    # Calculate unweighted accessibility metrics
    if(is.null(input$weight_tiff)) {
      
      # Clip baseline .tiff to project area
      baseline_cropped <- mask(baseline, buffer_diss)
      # Sum accessibility values for all raster cells within the buffer
      baseline_num <- sum(values(baseline_cropped), na.rm = T)
      
      # Build
      build_cropped <- mask(build, buffer_diss)
      # Clip build .tiff to project area
      build_num <- sum(values(build_cropped), na.rm = T)
      
      # Compute % change in accessibility
      diff_pct <- ((build_num - baseline_num) / baseline_num)
      
      # Compute average change in accessibility
      avg_diff <- mean((values(build_cropped) - values(baseline_cropped)), na.rm = T)
      
      # Create summary data frame
      project <- input$project_name
      pct_change_access <- diff_pct
      pct_change_access_natural <- diff_pct_natural
      avg_change_access <- avg_diff
      avg_change_access_natural <- avg_diff_natural
      eval_table <- data.frame(project, pct_change_access, pct_change_access_natural, avg_change_access, avg_change_access_natural)
    
    # Calculate weighted accessibility metrics
    } else {
      
      # Read weight raster, crop to buffer
      weight <- raster(input$weight_tiff$datapath)
      weight_cropped <- mask(weight, buffer_diss)
      
      # Baseline
      baseline_cropped <- mask(baseline, buffer_diss)
      baseline_num <- sum(values(baseline_cropped), na.rm = T)
      
      # Build
      build_cropped <- mask(build, buffer_diss)
      build_num <- sum(values(build_cropped), na.rm = T)
      
      # Difference
      difference <- build_cropped - baseline_cropped
      
      # Weight baseline and build .TIFFs
      baseline_weighted <- baseline * weight_cropped
      build_weighted <- build * weight_cropped
    
      # Weighted Baseline
      w_baseline_cropped <- mask(baseline_weighted, buffer_diss)
      w_baseline_num <- sum(values(w_baseline_cropped), na.rm = T)
      
      # Weighted Build
      w_build_cropped <- mask(build_weighted, buffer_diss)
      w_build_num <- sum(values(w_build_cropped), na.rm = T)
      
      ### Natural #######################################################################################
      # Calculate adjusted difference
      difference_adjusted <- difference_natural
      difference_adjusted[] = ifelse(difference[]!=0, difference[], NA)
      
      # Calculate adjusted weight
      weight_adjusted <- weight
      weight_adjusted[] = ifelse(difference_natural[] != 0, weight[], NA)
      
      # Calculate weighted raster
      weighted_access_natural <- weighted.mean(values(difference_adjusted), values(weight_adjusted), na.rm = T)
      
      
      # Compute weighted % change in accessibility
      baseline_num_weighted <- sum(values(baseline_adj * weight_adjusted), na.rm = T)
      build_num_weighted <- sum(values(build_adj * weight_adjusted), na.rm = T)
      weighted_diff_pct_natural <- ((build_num_weighted - baseline_num_weighted) / baseline_num_weighted)
      
      # Compute average change in accessibility
      avg_diff_natural <- mean((values(build_adj) - values(baseline_adj)), na.rm = T)
      
      # Compute weighted % change in accessibility
      weighted_diff_pct <- ((w_build_num - w_baseline_num) / w_baseline_num)
      ###
      
      # Compute average change in accessibility
      weighted_access_change <- weighted.mean(values(difference) - values(weight_cropped), na.rm = T)
      
      # Compute % change in accessibility
      diff_pct <- ((build_num - baseline_num) / baseline_num)
      
      # Compute average change in accessibility
      avg_diff <- mean((values(build_cropped) - values(baseline_cropped)), na.rm = T)
      
      # Create summary data frame
      project <- input$project_name
      pct_change_access <- diff_pct
      pct_change_access_weighted <- weighted_diff_pct
      pct_change_access_natural <- diff_pct_natural
      pct_change_access_weighted_natural <- weighted_diff_pct_natural
      avg_change_access <- avg_diff
      weighted_avg_change_access <- weighted_access_change
      avg_change_access_natural <- avg_diff_natural
      weighted_avg_change_access_natural <- weighted_access_natural
      
      eval_table <- data.frame(project, pct_change_access, pct_change_access_weighted, pct_change_access_natural, pct_change_access_weighted_natural, avg_change_access, weighted_avg_change_access, avg_change_access_natural, weighted_avg_change_access_natural)
    }
  })
  
  # Render/format data table
  output$table1 = renderDataTable({
    if (is.null(input$weight_tiff)) {
    datatable(eval_table()) %>%
      formatPercentage(c("pct_change_access"), 2) %>%
      formatPercentage(c("pct_change_access_natural"), 2) %>% 
      formatCurrency(c("avg_change_access"), currency = "", interval = 3, mark = ",", digits = 0) %>%
      formatCurrency(c("avg_change_access_natural"), currency = "", interval = 3, mark = ",", digits = 0)
    } else {
      datatable(eval_table()) %>%
        formatPercentage(c("pct_change_access"), 2) %>%
        formatPercentage(c("pct_change_access_weighted"), 2) %>%
        formatPercentage(c("pct_change_access_natural"), 2) %>% 
        formatPercentage(c("pct_change_access_weighted_natural"), 2) %>% 
        formatCurrency(c("avg_change_access"), currency = "", interval = 3, mark = ",", digits = 0) %>%
        formatCurrency(c("weighted_avg_change_access"), currency = "", interval = 3, mark = ",", digits = 0) %>%
        formatCurrency(c("avg_change_access_natural"), currency = "", interval = 3, mark = ",", digits = 0) %>%
        formatCurrency(c("weighted_avg_change_access_natural"), currency = "", interval = 3, mark = ",", digits = 0)
    }
  })
  
  # Download summary table as csv
  output$download1 <- downloadHandler(
    filename = function() {
      "Accessibility_Project_Evaluation.csv"
    },
    content = function(file) {
      write.csv(eval_table(),
                file,
                row.names=FALSE)
    }
  )
  
  # Render project map
  output$map1 <- renderLeaflet({
    
    # Validate input .tiff files
    validate(
      need(input$baseline_tiff != "", "Please upload a baseline .tiff file"),
      need(input$project_tiff != "", "Please upload a project .tiff file"),
      need(input$shapefile_input != "", "Please upload a project shapefile")
    )
    
    baseline <- raster(input$baseline_tiff$datapath)
    build <- raster(input$project_tiff$datapath)
    
    project_extent <- st_as_sf(project_extent())
    project_extent <- st_transform(project_extent, crs = 3857)
    
    buffer <- st_buffer(project_extent, (input$buffer * 1609.34))
    buffer_diss <- st_as_sf(st_union(buffer))
    
    # Read in baseline and build .TIFF files
    baseline_clipped <- mask(baseline, buffer_diss)
    build_clipped <- mask(build, buffer_diss)
    
    # Compute change in accessibility
    difference_clipped <- mask((build - baseline), buffer_diss)
    
    # Temporarily disabled the weighting from the map option since I have to rewrite the front end for it
    clipped_weight <- mask(raster(input$weight_tiff$datapath), buffer_diss)
    weighted_differences_clipped <- (build_clipped * clipped_weight) - (baseline_clipped * clipped_weight)
    
    ### Natural ################################################################################################
    # Compute change in accessibility
    difference <- build - baseline
    difference[] = ifelse(difference[]!=0, difference[], NA)
    
    # Compute weighted change in accessibility
    weight_adjusted <- raster(input$weight_tiff$datapath)
    weight_adjusted[] = ifelse(difference[] != 0, weight_adjusted[], NA)
    
    weighted_differences <- weight_adjusted * difference
    
   # weighted_differences_clipped <- weight_adjusted * difference
    
    if(input$map_options == "Baseline Accessibility") {
      raster_file <- baseline_clipped
    } else if(input$map_options == "Project Accessibility") {
      raster_file <- build_clipped
    } else if(input$map_options == "Accessibility Change") {
      raster_file <- difference_clipped
    } else if(input$map_options == "Weighted Accessibility Change") {
      raster_file <- weighted_differences_clipped
    } else if(input$map_options == "Baseline Accessibility (natural)") {
      raster_file <- baseline
    } else if(input$map_options == "Project Accessibility (natural)") {
      raster_file <- build
    } else if(input$map_options == "Accessibility Change (natural)") {
      raster_file <- difference
    } else if(input$map_options == "Weighted Accessibility Change (natural)") {
      raster_file <- weighted_differences
    }
    
    # Define color pallette
    pal <- colorNumeric(c("viridis"), values(raster_file), na.color = "transparent")
    
    # Render leaflet map
    leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
      addRasterImage(raster_file, colors = pal, opacity = .6, project = F) %>%
      addPolylines(data = project_extent()) %>%
      addLegend(pal = pal, values = values(raster_file), title = input$map_options) %>%
      addFullscreenControl()
  })
}

### END Server ###

# Run app
shinyApp(ui, server)
