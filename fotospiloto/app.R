
library(shiny)
library(DT)
library(tidyverse)
library(aws.s3)
library(readxl)
library(tidyverse)
library(lubridate)
library(RMariaDB)
library(pool)
library(glue)
library(DBI)
library(leaflet)
library(shinymanager)


# pool <- dbPool(
#   drv = MariaDB(),
#   dbname = Sys.getenv("dbname") ,
#   host = Sys.getenv("dbhost"),
#   username = Sys.getenv("dbusername"),
#   password = Sys.getenv("dbpassword")
# )
# 

credentials <- data.frame(
  user = c("indata", "ccampora"), # mandatory
  password = c("piloto", "a"), # mandatory
  start = c("2019-04-15"), # optinal (all others)
  expire = c(NA, NA),
  admin = c(FALSE, TRUE),
  comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

createLink <- function(val) {
  sprintf('<a href="%s" target="_blank" class="btn btn-primary">Ver</a>',val)
}

lista_fotos = aws.s3::s3readRDS(object = "lista_fotos_piloto.rds",bucket = "pilotocalefaccion")

lista_fotos


casas = 
 # readxl::read_excel("../datos/datos_septiembre22/Casas.xlsx")  %>% 
  readxl::read_excel("Casas.xlsx")  %>% 
  
  mutate(latitud = as.numeric(latitud), longitud = as.numeric(longitud)) %>% 
  left_join(lista_fotos, by = "ID_Casa") %>% 
  arrange(ID_Casa)



#%>% select(-url_foto,-url_foto3)

# casas = readxl::read_excel(paste0(getwd(),"/datos/datos_septiembre22/Casas.xlsx")) %>% 
#   mutate(latitud = as.numeric(latitud), longitud = as.numeric(longitud))



# Define UI for application that draws a histogram
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyalert::useShinyalert(),

    # Application title
    titlePanel("Fotos piloto calefaccion"),

    sidebarLayout(
        sidebarPanel(width = 3,
          selectInput(inputId = "id_casa",label = "ID Casa",choices = casas$ID_Casa,multiple = FALSE,width = "100%"),
          fluidRow(
            column(8,fileInput(inputId = "foto", label = NULL, multiple = FALSE,
                               accept = c(".jpg",".jpeg",".png"),buttonLabel = "Seleccionar",width = "100%")),
            column(4,actionButton(inputId = "borrar_foto","Eliminar"))
          )  
          ,
          actionButton(inputId = "boton_subir",label = "Subir",width = "100%",style = "background: steelblue, color: white;"),
          
          br(),
          
          leaflet::leafletOutput(outputId = "mapa")
          
        ),

        # Show a plot of the generated distribution
        mainPanel(width = 9,
           DT::dataTableOutput("casas")
        )
    )
)

ui <- secure_app(ui)


server <- function(input, output) {
  
  # RV subir archivos ----
  
  rv <- reactiveValues(data = NULL, name = NULL)
  
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  
  
  observe({
    req(input$foto)
    rv$data <- input$foto$datapath
    rv$name <- input$foto$name
  })
  
  
  observeEvent(input$borrar_foto, {
    rv$data <- NULL
    rv$name <- NULL
    shinyjs::reset('foto')
  })

  
  
  lista = reactiveValues(data = lista_fotos)
  
  casas = reactiveValues(data = casas)
  
  
  output$casas = DT::renderDataTable({
    
    DT::datatable(
      
      casas$data %>% 
        mutate(foto = ifelse(is.na(url_foto),NA,createLink(url_foto))) %>% 
        select(-url_foto) %>% 
        select(ID_Casa,foto,region:habitantes)
      
      ,escape=FALSE
      ,options = list(scrollX = TRUE))
    
  })
  

  
  
  caso = reactiveValues(data = NULL)
  
  observeEvent(input$id_casa,{

    caso$data = casas$data  %>% filter(ID_Casa == input$id_casa) 
    
    shinyjs::click("borrar_foto")
    
   # print(caso$data)

  })
  



  output$mapa = leaflet::renderLeaflet({
    
    caso$data %>% 

    leaflet::leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(lng = ~longitud, lat = ~latitud) 
  })
  
  
  observeEvent(input$boton_subir,{
    
    
   # print(rv$name)
    
   shinybusy::show_modal_spinner(
     
     spin = "cube-grid",
     color = "darkgreen",
     text = "Subiendo Imagen..."
     
   )
    
    
   extension = stringr::str_split(rv$name, "\\.", simplify=T)[,2]
    
   nombre_archivo = paste0(paste0(sample(c(letters,0:9),5),collapse = ""),"_ID",input$id_casa,".",extension)
    
   #print(nombre_archivo)
    
   lista$data[lista$data$ID_Casa == input$id_casa,2] = paste0("https://pilotocalefaccion.s3.sa-east-1.amazonaws.com/",nombre_archivo)
    
   print( lista$data)
   
   aws.s3::put_object(file = rv$data, object = nombre_archivo, bucket = "pilotocalefaccion" )
    
   aws.s3::s3saveRDS(#lista_fotos
                     lista$data
                     ,object = "lista_fotos_piloto.rds",bucket = "pilotocalefaccion")
   
   
    
   shinybusy::remove_modal_spinner()
   
   lista$data = aws.s3::s3readRDS(object = "lista_fotos_piloto.rds",bucket = "pilotocalefaccion")
   
   casas$data = 
   #  readxl::read_excel("../datos/datos_septiembre22/Casas.xlsx")  %>% 
     readxl::read_excel("Casas.xlsx")  %>% 
     mutate(latitud = as.numeric(latitud), longitud = as.numeric(longitud)) %>% 
     left_join(lista$data, by = "ID_Casa") %>% 
     arrange(ID_Casa)
   
   
   
   shinyalert::shinyalert(
     title = "Imagen Guardada",
     text = "",
     size = "s", 
     closeOnEsc = TRUE,
     closeOnClickOutside = FALSE,
     html = FALSE,
     type = "success",
     showConfirmButton = TRUE,
     showCancelButton = FALSE,
     confirmButtonText = "OK",
     confirmButtonCol = "#AEDEF4",
     timer = 0,
     imageUrl = "",
     animation = TRUE
   )
   
   
   
   
    
  })
  
  
  observe({
    
    if (is.null(rv$data)){
      
      shinyjs::disable("boton_subir")
      
    } else {
      
      shinyjs::enable("boton_subir")
      
    }
    
  })
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
