# Conveyal Post-Processing Tool (for CSIS mode shift analysis)
# Henry.McKay@dot.ca.gov
# Last update: 07/21/2023

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
      helpText("Upload Accessibility .TIFFs"),
      fileInput("baseline_multimodal_tiff", "Baseline Multimodal .TIFF Output", multiple = FALSE),
      fileInput("project_multimodal_tiff", "Build Multimodal .TIFF Output", multiple = FALSE),
      fileInput("baseline_auto_tiff", "Baseline Auto .TIFF Output", multiple = FALSE),
      fileInput("project_auto_tiff", "Build Auto .TIFF Output", multiple = FALSE),
      fileInput("weight_tiff", "Weight .TIFF", multiple = FALSE),
      fileInput("shapefile_input", "Project Extent Shapefile", multiple = T,
                accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj', '.cpg')),
      numericInput("buffer", "Analysis Area Buffer (kilometers)", 3),
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
      selectInput("map_options", "Select Raster to Map", choices = c("Baseline Accessibility Ratio", 
                                                                     "Build Accessibility Ratio",
                                                                     "Change in Accessibility Ratio",
                                                                     "Weighted Change in Accessibility Ratio")),
      helpText("Note: The weighted change in accessibility ratio map is meant only to show where the relative
               changes in accessibility occur when accounting for a weighting factor, using a color scale. 
               Individual numbers associated with these changes should only be analyzed in the context
               of aggregate metrics, such as the ones shown on the project analysis tab.")
    ),
    mainPanel(
      tags$style(type = "text/css", "#map1 {height: calc(100vh - 100px) !important; }"),
      leafletOutput("map1")
    )
  )
)

# Assemble UI
ui <- navbarPage(
  title = "CSIS Mode Shift Metric Post-Processing Tool",
  tab1,
  tab3
)

### END User Interface ###

### Server ###

server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=30*1024^2)
  
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
      need(input$baseline_multimodal_tiff != "", "Please upload a baseline multimodal .tiff file"),
      need(input$project_multimodal_tiff != "", "Please upload a project multimodal .tiff file"),
      need(input$baseline_auto_tiff != "", "Please upload baseline auto .tiff file"),
      need(input$project_auto_tiff != "", "Please upload a project auto .tiff file"),
      need(input$weight_tiff != "", "Please upload a weight .tiff file"),
      need(input$shapefile_input != "", "Please upload a project shapefile")
    )
    
    # Read in project shapefile and apply buffer at specified distance
    project_extent <- st_as_sf(project_extent())
    project_extent <- st_transform(project_extent, crs = 3857)
    buffer <- st_buffer(project_extent, (input$buffer * 1000)) # Convert input (in kilometers) to meters
    buffer_diss <- st_as_sf(st_union(buffer))
    
    # Read in files and create ratios
    baseline_multimodal <- raster(input$baseline_multimodal_tiff$datapath)
    build_multimodal <- raster(input$project_multimodal_tiff$datapath)
    baseline_auto <- raster(input$baseline_auto_tiff$datapath)
    build_auto <- raster(input$project_auto_tiff$datapath)
    
    baseline_multimodal <- crop(baseline_multimodal, build_multimodal)
    baseline_auto <- crop(baseline_auto, build_auto)
    
    baseline_ratio <- (baseline_multimodal / baseline_auto)
    baseline_ratio[is.nan(baseline_ratio[])] <- 0
    baseline_ratio[is.infinite(baseline_ratio[])] <- 0
    baseline_ratio[] = ifelse(baseline_ratio[] <= 1, baseline_ratio[], 1)
    
    build_ratio <- (build_multimodal / build_auto)
    build_ratio[is.nan(build_ratio[])] <- 0
    build_ratio[is.infinite(build_ratio[])] <- 0
    build_ratio[] = ifelse(build_ratio[] <= 1, build_ratio[], 1)  
    
    baseline_ratio <- crop(baseline_ratio, build_ratio)
    
    ### Calculate metrics
    # Read weight raster, crop to buffer
    weight <- raster(input$weight_tiff$datapath)
    weight <- crop(weight, baseline_ratio)
    weight_cropped <- mask(weight, buffer_diss)
    
    # Baseline
    baseline_cropped <- mask(baseline_ratio, buffer_diss)
    baseline_num <- sum(values(baseline_cropped), na.rm = T)
    
    # Build
    build_cropped <- mask(build_ratio, buffer_diss)
    build_num <- sum(values(build_cropped), na.rm = T)
    
    # Difference
    difference <- build_cropped - baseline_cropped
    
    # Weight baseline and build .TIFFs
    baseline_weighted <- baseline_ratio * weight_cropped
    build_weighted <- build_ratio * weight_cropped
    
    # Weighted Baseline
    w_baseline_cropped <- mask(baseline_weighted, buffer_diss)
    w_baseline_num <- sum(values(w_baseline_cropped), na.rm = T)
    
    # Weighted Build
    w_build_cropped <- mask(build_weighted, buffer_diss)
    w_build_num <- sum(values(w_build_cropped), na.rm = T)
    
    # Compute weighted % change in accessibility
    weighted_diff_pct <- ((w_build_num - w_baseline_num) / w_baseline_num)
    ###
    
    # Compute average change in accessibility
    weighted_access_change <- weighted.mean(values(difference), values(weight_cropped), na.rm = T)
    
    # Compute % change in accessibility
    diff_pct <- ((build_num - baseline_num) / baseline_num)
    
    # Compute average change in accessibility
    avg_diff <- mean((values(build_cropped) - values(baseline_cropped)), na.rm = T)
    
    # Create summary data frame
    project <- input$project_name
    pct_change_access <- diff_pct
    pct_change_access_weighted <- weighted_diff_pct
    avg_change_access <- avg_diff
    weighted_avg_change_access <- weighted_access_change
    
    eval_table <- data.frame(project, 
                             pct_change_access, 
                             pct_change_access_weighted, 
                             avg_change_access, 
                             weighted_avg_change_access)
  })
  
  # Render/format data table
  output$table1 = renderDataTable({
    datatable(eval_table()) %>%
      formatPercentage(c("pct_change_access"), 2) %>%
      formatPercentage(c("pct_change_access_weighted"), 2) %>%
      formatCurrency(c("avg_change_access"), currency = "", interval = 3, mark = ",", digits = 4) %>%
      formatCurrency(c("weighted_avg_change_access"), currency = "", interval = 3, mark = ",", digits = 4)
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
      need(input$baseline_multimodal_tiff != "", "Please upload a baseline multimodal .tiff file"),
      need(input$project_multimodal_tiff != "", "Please upload a project multimodal .tiff file"),
      need(input$baseline_auto_tiff != "", "Please upload baseline auto .tiff file"),
      need(input$project_auto_tiff != "", "Please upload a project auto .tiff file"),
      need(input$weight_tiff != "", "Please upload a weight .tiff file"),
      need(input$shapefile_input != "", "Please upload a project shapefile")
    )
    
    # Read in project shapefile and apply buffer at specified distance
    project_extent <- st_as_sf(project_extent())
    project_extent <- st_transform(project_extent, crs = 3857)
    buffer <- st_buffer(project_extent, (input$buffer * 1000)) # Convert input (in kilometers) to meters
    buffer_diss <- st_as_sf(st_union(buffer))
    
    # Read in files and create ratios
    baseline_multimodal <- raster(input$baseline_multimodal_tiff$datapath)
    build_multimodal <- raster(input$project_multimodal_tiff$datapath)
    baseline_auto <- raster(input$baseline_auto_tiff$datapath)
    build_auto <- raster(input$project_auto_tiff$datapath)
    
    baseline_multimodal <- crop(baseline_multimodal, build_multimodal)
    baseline_auto <- crop(baseline_auto, build_auto)
    
    baseline_ratio <- (baseline_multimodal / baseline_auto)
    baseline_ratio[is.nan(baseline_ratio[])] <- 0
    baseline_ratio[is.infinite(baseline_ratio[])] <- 0
    baseline_ratio[] = ifelse(baseline_ratio[] <= 1, baseline_ratio[], 1)
    
    build_ratio <- (build_multimodal / build_auto)
    build_ratio[is.nan(build_ratio[])] <- 0
    build_ratio[is.infinite(build_ratio[])] <- 0
    build_ratio[] = ifelse(build_ratio[] <= 1, build_ratio[], 1)  
    
    baseline_ratio <- crop(baseline_ratio, build_ratio)
    
    ###
    baseline <- baseline_ratio
    build <- build_ratio
    
    project_extent <- st_as_sf(project_extent())
    project_extent <- st_transform(project_extent, crs = 3857)
    
    buffer <- st_buffer(project_extent, (input$buffer * 1000))
    buffer_diss <- st_as_sf(st_union(buffer))
    
    # Read in baseline and build .TIFF files
    baseline_clipped <- mask(baseline, buffer_diss)
    build_clipped <- mask(build, buffer_diss)
    
    # Compute change in accessibility
    difference_clipped <- mask((build - baseline), buffer_diss)
    
    # Weighting
    clipped_weight <- mask(raster(input$weight_tiff$datapath), buffer_diss)
    weighted_differences_clipped <- (build_clipped * clipped_weight) - (baseline_clipped * clipped_weight)
    
    if(input$map_options == "Baseline Accessibility Ratio") {
      raster_file <- baseline_clipped
    } else if(input$map_options == "Build Accessibility Ratio") {
      raster_file <- build_clipped
    } else if(input$map_options == "Change in Accessibility Ratio") {
      raster_file <- difference_clipped
    } else if(input$map_options == "Weighted Change in Accessibility Ratio") {
      raster_file <- weighted_differences_clipped
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
