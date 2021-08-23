library(bs4Dash)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(highcharter)
library(dplyr)
library(readr)

datos <- read_csv("https://raw.githubusercontent.com/ccamporag/piloto_calefaccion/main/datos_simulados.csv?token=AKPZOQM4INSHELXD6EJFOFDBD4EVI")



ui <- dashboardPage(header = dashboardHeader(title = icon("fas fa-world")),
    
    dashboardSidebar(collapsed = TRUE,expandOnHover = TRUE,
                     
                     sidebarMenu(
                         menuItem("Mapa",tabName = "map",icon = icon("fas fa-map"))
                     )
                     
                     
                     ),
    dashboardBody(
        
        tabItem(tabName = "map",
            
               fluidRow(
                    column(6,
                           leafletOutput("mapa",height = "900px",width = "100%")
                    ),
                    column(6,
                           box(width = 12,solidHeader = FALSE,collapsible = FALSE,
                               highchartOutput("plot1")
                           ),
                           
                           # tabBox(type = "pills",id = "adnjakdj",sidebar = boxSidebar(background = "red",icon = icon("fas-fa map")),
                           #        "dshjdjh","kcdnkjdcnck"
                           #        
                           #        ),
                           
                           tabBox(
                               id = "tabcard",width = 12,
                               title = "",
                             #  selected = "Temperatura Media",
                               status = "primary",
                               solidHeader = FALSE,
                               type = "pills",
                               tabPanel(
                                   title = "PM 2.5",
                                   highchartOutput("plot0")
                               ),
                               tabPanel(
                                   title = "PM 10",
                                   highchartOutput("plot3")
                               ),
                               tabPanel(
                                   title = "CO2",
                                   highchartOutput("plot2")
                               )
                           )
                           
                          
                    )
               )    
                
            
        )
        

        

    )
    
    # ,
    # controlbar = dashboardControlbar(
    #     collapsed = FALSE,
    #     div(class = "p-3", skinSelector()),
    #     pinned = TRUE
    # )
    
)

server <- function(input, output) {
    
    
    observeEvent(input$mapa_marker_click,{
        
        
        output$plot0 <- renderHighchart({
            
           # req(input$mapa_marker_click)
            
            temp <- tibble(
                Hora = 0:23,
                PM2.5 = 10*cos(seq(0,2*pi,length.out = 24)+rnorm(24,sd = 0.35)+20)+500
            )
            
            hchart(temp,type = "line",hcaes(x=Hora,y=PM2.5)) %>% 
                hc_title(text=list("PM 2.5 [ppm]"))
            
            
            
        })
        
        output$plot1 <- renderHighchart({
            
            req(input$mapa_marker_click)
            
            temp <- tibble(
                Hora = 0:23,
                Temperatura = 10*cos(seq(0,2*pi,length.out = 24)+rnorm(24,sd = 0.15)+20)+10
                )
            
            hchart(temp,type = "line",hcaes(x=Hora,y=Temperatura),color="darkred") %>% 
                hc_title(text=list("Temperatura Media [ºC]"))
            
            
            
        })
        
        
        output$plot2 <- renderHighchart({
            
            temp <- tibble(
                Hora = 0:23,
                CO2 = 10*cos(seq(0,2*pi,length.out = 24)+rnorm(24,sd = 0.8)+200)+100
            )
            
            hchart(temp,type = "line",hcaes(x=Hora,y=CO2),color="red") %>% 
                hc_title(text=list("CO2"))
            
            
            
        })
        
        
        output$plot3 <- renderHighchart({
            
            temp <- tibble(
                Hora = 0:23,
                PM10 = 10*cos(seq(0,2*pi,length.out = 24)+rnorm(24,sd = 0.45)+600)+200
            )
            
            hchart(temp,type = "line",hcaes(x=Hora,y=PM10),color="green") %>% 
                hc_title(text=list("PM 10 [ppm]"))
            
            
            
        })
        
        
    })
    
    

    
    
    
    
    
    output$mapa <- renderLeaflet({
        

        
        leaflet(datos) %>% 
            
            addTiles(group = "Satélite",
                     attribution = 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS Comm.',
                     urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}'
                     
            ) %>% 
            
            addTiles(group = "Open Street Map") %>% 
            
            addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>% 
            

            
            addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
            addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
            
            setView(lng = -70.6341 , lat = -33.4369, zoom = 4) %>%  
            
            addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE) %>% 
            
            addLayersControl(
                baseGroups = c("Satélite","Toner Lite","Toner","CartoDB","Open Street Map"),
              #  overlayGroups = c("Ver/Ocultar Leyenda"),
                options = layersControlOptions(collapsed = TRUE)
            ) %>% 
            
            addMarkers(lng = ~longitude, lat = ~latitude,layerId = datos$id_casa)
        
        
        
        
        
    })
    

}

shinyApp(ui, server)
