# Load libraries and functions #################################################

#install.packages(c("shiny","leaflet","shinythemes","FedData","maps",
#                   "maptools","rgdal","raster","dplyr","ggplot2","XML",
#                   "Hmisc","lubridate"))

library(shiny)
library(leaflet)
library(shinythemes)
library(FedData)
library(maps)
library(maptools)
library(rgdal)
library(raster)
library(dplyr)
library(ggplot2)
library(XML)
library(Hmisc)
library(lubridate)

source("R/downloadSSURGO.R")
source("R/moldSSURGO.R")
source("R/calcSSURGO.R")
source("R/ssurgo2apsim.R")

# USER INTERFACE ###########################

ui <- fluidPage(theme = shinytheme("cosmo"),
  h2(strong("APssurgo:"),em("Get SSURGO data and convert into APSIM '.soil' format")),
  div(
    p("This tool was developed by ",a("R. Martinez-Feria",href="mailto:rmartine@iastate.edu"),
      "and",a("S.V. Archontoulis",href="mailto:sarchont@iastate.edu"),", Dept. of Agronomy, Iowa State Univerity, Ames, Iowa"),
    style ="margin: 20px"  
  ),
  div(
    h3("Field boundaries:"),
    fixedRow(column(width = 4),
             column(width = 4,numericInput("north","North:",value = 42.0216, step = 10^-4)),
             column(width = 4)),
    fixedRow(column(width = 4,numericInput("west","West:",value = -93.7762, step = 10^-4)),
             column(width = 4),
             column(width = 4,numericInput("east","East:",value = -93.7751, step = 10^-4))),
    fixedRow(column(width = 4),
             column(width = 4,numericInput("south","South:",value = 42.0206, step = 10^-4)),
             column(width = 4)),
    style ="background-color: #ededed; border-color: #2e6da4; padding: 20px; margin: 20px",
    leafletOutput("mymap")
  ),
      column(
        inputPanel(
          textInput("myfield","Site Name:",value = "ISU Agronomy Farm", width = "100%"),
          textInput("mySoil_layers","Soil layer breaks (in cm separated by commas):",
                    value = "10,30,45,60,80,100,120,160,200", width = "100%"),
          selectInput("myCrops", "Crops:",c("Maize","Soybean","Wheat","Canola","AgPasture","Barley","Chickpea","Cotton","Cowpea","Fababean","FieldPea","LabLab","Lucerne","Lupin","Millet","Mucuna","Mungbean","OilPalm","Peanut","PigeonPea","Plant","Rice","Sorghum","Sugar","Weed"),
                      selected = c("Maize","Soybean"),
                      multiple = TRUE,
                      width = "100%")
          ),
        width = 6
        ),
      column(
        inputPanel(
          radioButtons("myBy_soil","Average across soil types",c(TRUE,FALSE)),
          radioButtons("myMap_save","Save soil map?",c(TRUE,FALSE))
        ),
        width = 3
      ),
      column(
        br(),
        actionButton("goDownload","Download SSURGO",
                     icon("arrow-circle-down"), 
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        width = 3
      )
  )

# SERVER ###########################

server <- function(input, output, session) {

  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri Imagery") %>%
      addTiles(group = "OSM (default)") %>%
      addRectangles(lat1=input$north, lng1=input$east,
                    lat2=input$south, lng2=input$west) %>%
      addLayersControl(
        baseGroups = c("Esri Imagery","OSM (default)"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observeEvent(input$goDownload, {
    
    withProgress(message = 'Working...', value = 0, {
      
    # Step 1 (Down)Loading SSURGO data
    
      incProgress(1/5, detail = "(Down)Loading SSURGO data. May take several minutes.")
      
      downloadSSURGO(
        SiteName = input$myfield,
        north=input$north,
        south=input$south,
        east=input$east,
        west=input$west
      ) -> x
      
      # Step 2 Mold data
      
      incProgress(1/5, detail = "Extracting data from tables")
      
      soil_layers <- as.numeric(unlist(strsplit(input$mySoil_layers,",")))
      
      moldSSURGO(x,soil_layers) -> x2
      
      # Step 3 Calculate new variables
      incProgress(1/5, detail = "Calculating new variables")
      
      calcSSURGO(x2,input$myBy_soil,soil_layers) -> x3
      
      # Step 4 Create XML FILE
      incProgress(1/5, detail = "Compiling soil XLM file")
      
      SSURGO2APSIM(x3,0.1,
                   site_name = input$myfield,
                   coords = c(input$north,input$west),
                   crops = tolower(input$myCrops))
      
      # Step 5 Save map
      
      incProgress(1/5, detail = "Saving files")
      
      if(input$myMap_save) {
        
        writeOGR(x$spatial, dsn=paste0(input$myfield,"_shapefile"), layer="x", driver="ESRI Shapefile")
        
        fortify(x$spatial, region = "MUSYM") %>%
          left_join(x$tabular$mapunit %>%
                      select(musym,muname),
                    by = c("id" = "musym")) %>%
          ggplot() +
          geom_polygon(aes(x=long, y=lat, group = group, fill =  muname),
                       alpha = 0.5, color = "black")  +
          scale_fill_hue(l = 40) +
          coord_equal() +
          labs(title = input$myfield,
               fill= "Soil type") +
          theme_minimal() + 
          theme(plot.background = element_blank(),
                legend.position = "bottom",
                legend.direction = "vertical") -> fig
        
          ggsave(filename =  paste0(input$myfield,".jpeg"),
               plot = fig, width = 6, height = 6, dpi = 300)
        
      }
    
    })
    
  })
  
}

shinyApp(ui, server)
