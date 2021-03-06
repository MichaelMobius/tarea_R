tsunami_data <- read.csv("tsevent.csv",stringsAsFactors=FALSE)
eruption_data <- read.csv("volerup.csv", stringsAsFactors=FALSE)

str(tsunami_data)
str(eruption_data)

tsunami_filter<-tsunami_data[!(is.na(tsunami_filter$LATITUDE)|tsunami_filter$LATITUDE==""), ]
tsunami_filter<-tsunami_filter[!(is.na(tsunami_filter$LONGITUDE)|tsunami_filter$LONGITUDE==""), ]

eruption_filter<-eruption_data[!(is.na(eruption_filter$Latitude)|eruption_filter$Latitude==""), ]
eruption_filter<-eruption_filter[!(is.na(eruption_filter$Longitude)|eruption_filter$Longitude==""), ]

library(leaflet)
library(shiny)

shinyApp(
  ui = fluidPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("MapPlot1",height=700)
  ),
  
  server = function(input, output){
    output$MapPlot1 <- renderLeaflet({
      leaflet() %>% 
        addProviderTiles("OpenTopoMap", group="background 1") %>% 
        addProviderTiles("Esri.WorldStreetMap", group="background 2") %>%
        setView(lng = -100, lat = 50, zoom = 2) %>%
        addLegend("bottomright", 
                  colors =c("red", "blue"),
                  labels= c("erupciones", "tsunamis"),
                  title= "Desastres naturales",
                  opacity = 1)
    }) 
    
    observe({
      #age <- input$time
      
      sites <- tsunami_filter  
        #filter(findInterval(tsunami_filter$YEAR, c(age - 0.5, age + 0.5)) == 1)
      
      sites2 <- eruption_filter 
        #filter(findInterval(eruption_filter$Year, c(age - 0.5, age + 0.5)) == 1)
      
      proxy <- leafletProxy("MapPlot1") 
      
      proxy %>% clearMarkers() %>%
        addCircleMarkers(lng = sites$LONGITUDE,
                         lat = sites$LATITUDE,
                         opacity = 0.1,radius=8,color="black",fillColor="blue",stroke = FALSE, fillOpacity = 0.8,group="Tsunamis") %>%
        addCircleMarkers(lng = sites2$Longitude,
                         lat = sites2$Latitude,
                         opacity = 0.1,radius=8,color="black",fillColor="red",stroke = FALSE, fillOpacity = 0.8,group="Erupciones") %>%
        addLayersControl(overlayGroups = c("Tsunamis","Erupciones") , baseGroups = c("background 1","background 2"), options = layersControlOptions(collapsed = FALSE))
      
    })
    
  } 
)