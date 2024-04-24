library(shiny)
#library(shinyWidgets)
library(plotly)
library(tidyverse)
#library(shinythemes)
library(DT)
library(htmltools)
library(htmlwidgets)
library(lubridate)
library(scales)
library(datasets)
library(dplyr)
library(leaflet)

#


ui<-shinyUI(
  navbarPage("Distribucion de locales de venta de barbacoa", id="main",
             tabPanel("Mapa", leafletOutput("bbmap", height=1000)),
             tabPanel("Información", DT::dataTableOutput("data")))
)


server <- shinyServer(function(input, output) {
  # Import Data and clean it
  

  
  # Todos ------------------------------------------------------------------------
  t_todos <- read.csv("D:/OneDrive - INEGI/Respaldo/varios/Medioambiente/demografia/barbacha_denue/INEGI_DENUE_22042024.csv")
  bb_data <- t_todos %>% janitor::clean_names()
  
  
  # new column for the popup label
  
  bb_data <- mutate(bb_data, cntnt = paste0('<b>Nom: </b>',nombre_de_la_unidad_economica,'<br>',
                                          '<b>Ent: </b>', entidad_federativa,'<br>',
                                          '<b>Mun: </b>', municipio,'<br>',
                                          '<b>CVE_ENT: </b>', clave_entidad,'<br>'))
  
  # create a color paletter for category type in the data file
  
  pal_casos <- colorNumeric(palette = "viridis",
                            rev = TRUE, 
                            domain = c(1, 
                                       32))  
  # create the leaflet map  
  output$bbmap <- renderLeaflet({
    leaflet(bb_data) %>% 
      addCircles(lng = ~longitud, lat = ~latitud) %>% 
      addTiles() %>%
      addCircleMarkers(lat =  ~latitud, lng =~longitud, 
                       radius = 3, popup = ~as.character(cntnt), 
                       color = pal_casos(bb_data$clave_entidad),
                       stroke = TRUE, fillOpacity = 0.8, group = T)%>%
      addLegend(labels =unique(bb_data$entidad_federativa), color = pal_casos(unique(bb_data$clave_entidad)),opacity=1, na.label = "Not Available") %>% 
      setView(lng = -92.65089, lat = 16.73891, zoom = 5)
  })
  
  #create a data object to display data
  
  output$data <-DT::renderDataTable(datatable(
    bb_data[, c("nombre_de_la_unidad_economica","entidad_federativa", "municipio", "codigo_postal")], filter = 'top',
    colnames = c("Nombre", "Entidad", "Municipio", "CP")
  ))
  
  
})


shinyApp(ui = ui, server = server)
