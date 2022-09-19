source("global.R")



ui <- dashboardPage(header = dashboardHeader(title = div(img(src="https://pilotocalefaccion.s3.sa-east-1.amazonaws.com/logo_indata_png.png",width = "100px"), "")),
    
    dashboardSidebar(collapsed = TRUE,expandOnHover = TRUE,disable = TRUE,
                     
                     sidebarMenu(
                         menuItem("Mapa",tabName = "map",icon = icon("fas fa-map"))
                     )
                     
                     
                     ),
    dashboardBody( useSever(),useShinyjs(),useWaiter(),
      
      tags$style(type = "text/css", "#mapa {height: calc(90vh - 80px) !important;}"),
        
        tabItem(tabName = "map",
                
            
               fluidRow(
                    column(4,
                           leafletOutput("mapa",width = "100%"),
                           div("*La ubicación de las viviendas es aproximada.")
                    ),
                    column(8,
                           
                           box(width = 12,solidHeader = FALSE,title = "Filtros",
                           fluidRow(
                             column(3,shinyWidgets::airDatepickerInput(
                               inputId = "range1",
                               label = "Rango de Fechas Periodo 1:",
                               autoClose = FALSE,
                               language = "es",
                               minDate = "2021-01-01",
                               maxDate = "2022-08-31",
                               range = TRUE, value = c("2021-01-01", "2022-08-31")
                             )),
                             column(3,shinyWidgets::airDatepickerInput(
                               inputId = "range2",
                               label = "Rango de Fechas Periodo 2:",
                               autoClose = FALSE,
                               language = "es",
                               minDate = "2021-01-01",
                               maxDate = "2022-08-31",
                               range = TRUE, value = c("2021-01-01", "2022-08-31")
                             )),
                             column(3,shinyWidgets::pickerInput(inputId = "visualizacion",
                                                                label = "Visualización de datos",
                                                                choices = c("Por Hora","Por Día","Por Mes"),
                                                                selected = "Por Hora",
                                                                multiple = FALSE,
                                                                width = "100%")),
                             column(3,
                              uiOutput("ui_boton_filtrar"),
                             
                             uiOutput("boton_descargar_datos")
                             
                             
                             
                             
                             )
                           )
                           ),
                           
                           
                           tabBox(
                             id = "tabcard",width = 12,
                             title = "",
                             #  selected = "Temperatura Media",
                             status = "primary",
                             solidHeader = FALSE,
                             type = "pills",
                             tabPanel(
                               title = "Temperatura",
                               highchartOutput("plot1")
                               
                             ),
                             
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
                             ),
                             tabPanel(
                               title = "Humedad"
                               ,
                               highchartOutput("plot4")
                             ),
                             tabPanel(
                               title = "Consumo Eléctrico"
                               ,
                                highchartOutput("plot5")
                             )
                           )
                           
                           ,
                           tabBox(
                             id = "uppertab",
                             width = 12,
                             title = "",
                             status = "primary",
                             solidHeader = FALSE,
                             type = "pills",
                             tabPanel(
                               title = "Ficha Vivienda",
                               uiOutput("ficha_vivienda")
                             ),
                             tabPanel(
                               title = "Equipo Anterior",
                               uiOutput("ficha_equipo_anterior")
                             ),
                             tabPanel(
                               title = "Equipo A/C",
                               uiOutput("ficha_equipo_actual")
                             )
                             # ,
                             # tabPanel(
                             #   title = "Otras Instalaciones"
                             # )
                             # ,
                             # tabPanel(
                             #   title = "Imágenes",
                             #   uiOutput("foto_casa")
                             # )
                           ),
                       column(12,
                              
                              # fluidRow(
                              #   column(4,actionButton(inputId = "todos",label = "Ver todo el periodo", width = "100%")),
                              #   column(4,actionButton(inputId = "anterior",label = "Periodo anterior al recambio", width = "100%")),
                              #   column(4,actionButton(inputId = "actual",label = "Periodo posterior al recambio", width = "100%"))
                              #   
                              # )
                              
                              )    
                           

                       #       ,
                       # 
                       # column(12,
                       #        
                       #        fluidRow(
                       #          column(4,uiOutput("fecha_inicio_medicion")),
                       #          column(4,uiOutput("fecha_fin_medicion")),
                       #          column(4,uiOutput("fecha_recambio")),
                       #        )
                       #        
                       #        )    
                      #     
                      #     ,
                           
                           
                         #  br(),
                           # box(width = 12,
                           #     solidHeader = FALSE,
                           #     collapsible = FALSE,
                           #     uiOutput("foto_casa")
                           #     
                           # ),
                           
                           # tabBox(type = "pills",id = "adnjakdj",sidebar = boxSidebar(background = "red",icon = icon("fas-fa map")),
                           #        "dshjdjh","kcdnkjdcnck"
                           #        
                           #        ),
                           
                           
                           
                          
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

server <- function(input, output,session) {
  
  sever(html = disconnected, bg_color = "white")
    
    temp1 <- reactiveValues(data = NULL, fecha_min = NULL, fecha_max = NULL, fecha_recambio = NULL)
    
    temp2 <- reactiveValues(data = NULL)
    
    
    # observeEvent(input$todos,{
    #   
    #   req(input$mapa_marker_click$id)
    #   
    #   temp1$data <- datos %>% 
    #     filter(nombre_sensor == input$mapa_marker_click$id) %>% 
    #     # mutate_at(.vars = c(6:ncol(datos)),.funs = as.numeric) %>% 
    #     mutate(x1 = row_number())
    #   
    #   temp1$fecha_min <- min(temp1$data$fecha_aaaa_mm_dd, na.rm = TRUE)
    #   
    #   temp1$fecha_max <- max(temp1$data$fecha_aaaa_mm_dd, na.rm = TRUE)
    #   
    #   temp1$fecha_recambio <- filter(temp1$data,tipo_calefaccion == "Periodo AC") %>% 
    #     pull(fecha_aaaa_mm_dd) %>%  
    #     min(na.rm = TRUE)
    #   
    #   temp1$fecha_recambio <- ifelse(is.na(temp1$fecha_recambio),"",as.character(temp1$fecha_recambio))
    #   
    #   
    # })
    
    
    # observeEvent(input$anterior,{
    #   
    #   req(input$mapa_marker_click$id)
    #   
    #   temp1$data <- datos %>% 
    #     filter(nombre_sensor == input$mapa_marker_click$id) %>% 
    #     # mutate_at(.vars = c(6:ncol(datos)),.funs = as.numeric) %>% 
    #     mutate(x1 = row_number()) %>% 
    #     filter(tipo_calefaccion == "Periodo Leña")
    #   
    #   temp1$fecha_min <- min(temp1$data$fecha_aaaa_mm_dd, na.rm = TRUE)
    #   
    #   temp1$fecha_max <- max(temp1$data$fecha_aaaa_mm_dd, na.rm = TRUE)
    #   
    #   temp1$fecha_recambio <- filter(temp1$data,tipo_calefaccion == "Periodo AC") %>% 
    #     pull(fecha_aaaa_mm_dd) %>%  
    #     min(na.rm = TRUE)
    #   
    #   temp1$fecha_recambio <- ifelse(is.na(temp1$fecha_recambio),"",as.character(temp1$fecha_recambio))
    #   
    #   
    # })
    
    # observeEvent(input$actual,{
    #   
    #   req(input$mapa_marker_click$id)
    #   
    #   temp1$data <- datos %>% 
    #     filter(nombre_sensor == input$mapa_marker_click$id) %>% 
    #     # mutate_at(.vars = c(6:ncol(datos)),.funs = as.numeric) %>% 
    #     mutate(x1 = row_number()) %>% 
    #     filter(tipo_calefaccion == "Periodo AC")
    #   
    #   
    #   
    #   
    # })
    # 

    
    resultados_filtro = reactiveValues(periodo1 = NULL, periodo2 = NULL)
    
    
    
    
    
    observeEvent(input$filtrar_fechas,{
      
      
      if (input$visualizacion == "Por Día"){
        
        resultados_filtro$periodo1 = temp1$data %>% 
          
          filter(fecha_aaaa_mm_dd >= input$range1[1], fecha_aaaa_mm_dd <= input$range1[2]) %>% 
          
          # group_by(hora_hh_mm_ss) %>% 
          
          group_by(date = fecha_aaaa_mm_dd) %>% 
          
          summarise(temperatura = mean(temperatura_º_c, na.rm = TRUE),
                    co2 = mean(co2_ppm, na.rm = TRUE),
                    mp2_5 = mean(mp2_5_st_p_ug_m3, na.rm = TRUE),
                    humedad = mean(humedad_percent_hr, na.rm = TRUE),
                    mp10 = mean(mp10_st_p_ug_m3,na.rm = TRUE)
          ) %>% 
          
          ungroup() %>% mutate(Periodo = "Periodo 1", date = as.Date(date))
        
        
        resultados_filtro$periodo2 = temp1$data %>% 
          
          filter(fecha_aaaa_mm_dd >= input$range2[1], fecha_aaaa_mm_dd <= input$range2[2]) %>% 
          
          #group_by(hora_hh_mm_ss) %>% 
          
          group_by(date = fecha_aaaa_mm_dd) %>% 
          
          summarise(temperatura = mean(temperatura_º_c, na.rm = TRUE),
                    co2 = mean(co2_ppm, na.rm = TRUE),
                    mp2_5 = mean(mp2_5_st_p_ug_m3, na.rm = TRUE),
                    humedad = mean(humedad_percent_hr, na.rm = TRUE),
                    mp10 = mean(mp10_st_p_ug_m3,na.rm = TRUE)) %>% 
          
          ungroup() %>% mutate(Periodo = "Periodo 2", date = as.Date(date))
        
        
      } else if (input$visualizacion == "Por Hora"){
        
        resultados_filtro$periodo1 = temp1$data %>% 
          
          filter(fecha_aaaa_mm_dd >= input$range1[1], fecha_aaaa_mm_dd <= input$range1[2]) %>% 
          
          group_by(date = hora_hh_mm_ss) %>% 
          
          #group_by(date = fecha_aaaa_mm_dd) %>% 
          
          summarise(temperatura = mean(temperatura_º_c, na.rm = TRUE),
                    co2 = mean(co2_ppm, na.rm = TRUE),
                    mp2_5 = mean(mp2_5_st_p_ug_m3, na.rm = TRUE),
                    humedad = mean(humedad_percent_hr, na.rm = TRUE),
                    mp10 = mean(mp10_st_p_ug_m3,na.rm = TRUE)
          ) %>% 
          
          ungroup() %>% mutate(Periodo = "Periodo 1")
        
        
        resultados_filtro$periodo2 = temp1$data %>% 
          
          filter(fecha_aaaa_mm_dd >= input$range2[1], fecha_aaaa_mm_dd <= input$range2[2]) %>% 
          
          group_by(date = hora_hh_mm_ss) %>% 
          
          #group_by(date = fecha_aaaa_mm_dd) %>% 
          
          summarise(temperatura = mean(temperatura_º_c, na.rm = TRUE),
                    co2 = mean(co2_ppm, na.rm = TRUE),
                    mp2_5 = mean(mp2_5_st_p_ug_m3, na.rm = TRUE),
                    humedad = mean(humedad_percent_hr, na.rm = TRUE),
                    mp10 = mean(mp10_st_p_ug_m3,na.rm = TRUE)) %>% 
          
          ungroup() %>% mutate(Periodo = "Periodo 2")
        
      } else if (input$visualizacion == "Por Mes"){
        
        resultados_filtro$periodo1 = temp1$data %>% 
          
          filter(fecha_aaaa_mm_dd >= input$range1[1], fecha_aaaa_mm_dd <= input$range1[2]) %>% 
          
          group_by(date = as.Date(paste0(lubridate::year(fecha_aaaa_mm_dd),"-",lubridate::month(fecha_aaaa_mm_dd),"-01"))) %>% 
          
          #group_by(date = fecha_aaaa_mm_dd) %>% 
          
          summarise(temperatura = mean(temperatura_º_c, na.rm = TRUE),
                    co2 = mean(co2_ppm, na.rm = TRUE),
                    mp2_5 = mean(mp2_5_st_p_ug_m3, na.rm = TRUE),
                    humedad = mean(humedad_percent_hr, na.rm = TRUE),
                    mp10 = mean(mp10_st_p_ug_m3,na.rm = TRUE)
          ) %>% 
          
          ungroup() %>% mutate(Periodo = "Periodo 1")
        
        
        resultados_filtro$periodo2 = temp1$data %>% 
          
          filter(fecha_aaaa_mm_dd >= input$range2[1], fecha_aaaa_mm_dd <= input$range2[2]) %>% 
          
          group_by(date = as.Date(paste0(lubridate::year(fecha_aaaa_mm_dd),"-",lubridate::month(fecha_aaaa_mm_dd),"-01"))) %>% 
          
          #group_by(date = fecha_aaaa_mm_dd) %>% 
          
          summarise(temperatura = mean(temperatura_º_c, na.rm = TRUE),
                    co2 = mean(co2_ppm, na.rm = TRUE),
                    mp2_5 = mean(mp2_5_st_p_ug_m3, na.rm = TRUE),
                    humedad = mean(humedad_percent_hr, na.rm = TRUE),
                    mp10 = mean(mp10_st_p_ug_m3,na.rm = TRUE)) %>% 
          
          ungroup() %>% mutate(Periodo = "Periodo 2", date = as.Date(date))
        
        
      }
      
      
      

      
      
    })
    
    
    observeEvent(input$mapa_marker_click,{
      
      temp1$fecha_min <- NULL
      temp1$fecha_max <- NULL
      temp1$fecha_recambio <- NULL
      
      
      temp2$data = NULL
      
      
      sensor = sensores2 %>% filter(ID_Casa == input$mapa_marker_click$id) %>% pull(nombre_del_sensor)
      
      temp1$data = readr::read_rds(paste0(path_datos,sensor,".rds"))  %>% 
        mutate(x1 = row_number(),
               temperatura_º_c = ifelse(temperatura_º_c<0,NA,temperatura_º_c)
               ) 
      
      temp2$data = consumos_ac %>% filter(id_casa == input$mapa_marker_click$id)
      
      if (nrow(temp2$data)>0){
        
        temp2$data = temp2$data %>%  mutate(date = as.Date(paste0(mes,"-01")))
        
      } else {
        
        NULL
        
      }
      
        
      
      
      #print(temp1$data)
      
      
      temp1$fecha_min <- min(temp1$data$fecha_aaaa_mm_dd, na.rm = TRUE) %>% as.character()
      
      temp1$fecha_max <- max(temp1$data$fecha_aaaa_mm_dd, na.rm = TRUE) %>% as.character()
      
      temp1$fecha_recambio <- temp1$data %>% dplyr::filter(tipo_calefaccion == "Periodo AC") %>% 
        pull(fecha_aaaa_mm_dd) %>%  
        min(na.rm = TRUE) %>% as.character()
      
      #temp1$fecha_recambio <- ifelse(is.na(temp1$fecha_recambio),"",as.character(temp1$fecha_recambio))
      
      
      
      print(temp1$fecha_min)
      print(temp1$fecha_max)
      print(temp1$fecha_recambio)
        
        # temp1$data <- datos %>% 
        #   filter(nombre_sensor == input$mapa_marker_click$id) %>% 
        #  # mutate_at(.vars = c(6:ncol(datos)),.funs = as.numeric) %>% 
        #   mutate(x1 = row_number())
        

      # print(c(temp1$fecha_min,temp1$fecha_recambio))
      # 
      # print(c(as.character(temp1$fecha_min),as.character(temp1$fecha_recambio)))
      
      shinyWidgets::updateAirDateInput(session = session,inputId = "range1",
                                       value = c(temp1$fecha_min,temp1$fecha_recambio))
      shinyWidgets::updateAirDateInput(session = session,inputId = "range2",
                                       value = c(temp1$fecha_recambio,as.character(as.Date(temp1$fecha_max)+1)))
      # 
      # 
        
      
      resultados_filtro$periodo1 = temp1$data %>% 
                                           
                                           filter(fecha_aaaa_mm_dd >= temp1$fecha_min, fecha_aaaa_mm_dd <= temp1$fecha_recambio) %>% 
                                           
                                           group_by(date = hora_hh_mm_ss) %>% 
                                           
                                           summarise(temperatura = mean(temperatura_º_c, na.rm = TRUE),
                                                     co2 = mean(co2_ppm, na.rm = TRUE),
                                                     mp2_5 = mean(mp2_5_st_p_ug_m3, na.rm = TRUE),
                                                     humedad = mean(humedad_percent_hr, na.rm = TRUE),
                                                     mp10 = mean(mp10_st_p_ug_m3,na.rm = TRUE)
                                           ) %>% 
                                           
                                           ungroup() %>% mutate(Periodo = "Periodo 1")
                                         
                                         
      resultados_filtro$periodo2 = temp1$data %>% 
                                           
                                           filter(fecha_aaaa_mm_dd >= temp1$fecha_recambio, fecha_aaaa_mm_dd <= temp1$fecha_max) %>% 
                                           
                                           group_by(date = hora_hh_mm_ss) %>% 
                                           
                                           summarise(temperatura = mean(temperatura_º_c, na.rm = TRUE),
                                                     co2 = mean(co2_ppm, na.rm = TRUE),
                                                     mp2_5 = mean(mp2_5_st_p_ug_m3, na.rm = TRUE),
                                                     humedad = mean(humedad_percent_hr, na.rm = TRUE),
                                                     mp10 = mean(mp10_st_p_ug_m3,na.rm = TRUE)) %>% 
                                           
                                           ungroup() %>% mutate(Periodo = "Periodo 2")
                                         
      
      
      
      
        
        output$fecha_inicio_medicion <- renderUI({

          req(temp1$fecha_min)

          div(span(strong("Fecha Inicio Medición: "),as.character(as.Date(temp1$fecha_min))))

        })

        output$fecha_fin_medicion <- renderUI({

          req(temp1$fecha_max)

          div(span(strong("Fecha Fin Medición: "),as.character(as.Date(temp1$fecha_max))))

        })

        output$fecha_recambio <- renderUI({

          req(temp1$fecha_recambio)

          div(span(strong("Fecha Recambio: "),temp1$fecha_recambio))

        })
        
        
        output$foto_equipo_anterior <- renderUI({
          
          url_equipo_anterior = equipo_anterior %>% filter(`ID Casa` == input$mapa_marker_click$id) %>% pull(url_foto)
          
          if (is.na(url_equipo_anterior)){
            div(strong("IMAGEN NO DISPONIBLE"))
          } else {
            div(tags$img(src=url_equipo_anterior,width="100%"))
          }
          
          
        })
        
        
        url_equipo_actual <- equipo_actual %>% filter(id_casa == input$mapa_marker_click$id) %>% pull(url_foto)
        
        output$slick_equipo_actual <- renderSlickR({
          
          slick1 <- slickR(
            
            obj         = url_equipo_actual,
            slideType   = 'img',
            slideId     = 'img',
            height      = 350,
            width = '85%')

          
          slick1 + settings(autoplay = FALSE, autoplaySpeed = 4000)
          
        })
        
        
        output$foto_equipo_actual <- renderUI({
          
          
          if (is.na(url_equipo_actual)){
            NULL
          } else {
            
            
            div(slickROutput("slick_equipo_actual",width='100%'))
            
          #  div(tags$img(src=url_equipo_anterior,width="100%"))
          
            }
          
          
        })
        
        
        
        output$foto_casa <- renderUI({
          
          url = casas %>% filter(ID_Casa == input$mapa_marker_click$id) %>% pull(url_foto)
          
          if (is.na(url)){
           # div(tags$img(src="logo_casa.png",width="100%", style = "border-radius: 20px;"))
            NULL
          } else {
            div(tags$img(src=url,width="100%", style = "border-radius: 20px;"))
          }
          
          
        })
        
        
        output$ficha_vivienda <- renderUI({
          
          fila <- casas %>% filter(ID_Casa == input$mapa_marker_click$id) %>% 
            mutate(materialidad = paste(ifelse(is.na(materialidad),"",materialidad),
                                        ifelse(is.na(materialidad_2),"",materialidad_2),
                                        ifelse(is.na(materialidad_3),"",materialidad_3),
                                        sep = "/",collapse = ""))
            
          div(
            fluidRow(
              
              column(7,
                     div(
                       #div(span(strong("ID Piloto: "),ifelse(is.na(fila$`ID Sensor`),"",fila$`ID Sensor`))),
                       div(span(strong("Región: "),ifelse(is.na(fila$region),"",fila$region))),
                       div(span(strong("Comuna: "), ifelse(is.na(fila$comuna),"",fila$comuna))),
                       div(span(strong("Zona: "), ifelse(is.na(fila$zona),"",fila$zona))),
                       div(span(strong("Área: "), ifelse(is.na(fila$area),"",fila$area))),
                       div(span(strong("Agrupamiento: "), ifelse(is.na(fila$agrupamiento),"",fila$agrupamiento))),
                       div(span(strong("Materialidad: "), ifelse(is.na(fila$materialidad),"",fila$materialidad))),
                       div(span(strong("Tipo de Ventanas: "), ifelse(is.na(fila$tipo_ventana),"",fila$tipo_ventana))),
                       div(span(strong("Nº Pisos: "), ifelse(is.na(fila$pisos),"",fila$pisos))),
                       div(span(strong("Año de Construcción: "), ifelse(is.na(fila$ano_construccion),"",fila$ano_construccion))),
                       div(span(strong("Estándar: "), ifelse(is.na(fila$estandar),"",fila$estandar))),
                       div(span(strong("Habitantes: "), ifelse(is.na(fila$habitantes),"",fila$habitantes)))
                       
                       ,
                       fluidRow(
                         column(12,uiOutput("fecha_inicio_medicion")),
                         column(12,uiOutput("fecha_fin_medicion")),
                         column(12,uiOutput("fecha_recambio")),
                       )
                       
                       
                     )
                     
              ),
              column(5,
                     uiOutput("foto_casa")
              )
              
            )

          )

          
          
        })
        
        output$ficha_equipo_anterior <- renderUI({
          
          fila_ea <- equipo_anterior %>% filter(`ID Casa` == input$mapa_marker_click$id)
          div(
            fluidRow(
              
              column(5,
                     div(
                      
                      # div(span(strong("ID Piloto: "),ifelse(is.na(fila_ea$`ID Sensor`),"",fila_ea$`ID Sensor`))),
                       div(span(strong("Tipo de Calefactor: "),ifelse(is.na(fila_ea$tipo_calefaccion),"",fila_ea$tipo_calefaccion))),
                       div(span(strong("Gasto en calefacción (CLP/año): "), ifelse(is.na(fila_ea$gasto_calefaccion_clp),"",fila_ea$gasto_calefaccion_clp))),
                       div(span(strong("Pros: "), ifelse(is.na(fila_ea$pro),"",fila_ea$pro))),
                       div(span(strong("Contras: "), ifelse(is.na(fila_ea$contra),"",fila_ea$contra)))
                       
                     )
                     
              ),
              column(7,
                     uiOutput("foto_equipo_anterior")
              )
              
            )
            
          )
          
        })
        
        
        output$ficha_equipo_actual <- renderUI({
          
          fila_actual <- equipo_actual2 %>% filter(id_casa == input$mapa_marker_click$id)
          
          
          
          div(
            fluidRow(

              column(5,
                     div(
                       #div(span(strong("ID Piloto: "),ifelse(is.na(fila$`ID Sensor`),"",fila$`ID Sensor`))),
                       div(span(strong("Modelo: "),ifelse(is.na(fila_actual$modelo),"",fila_actual$modelo))),
                       div(span(strong("Marca: "), ifelse(is.na(fila_actual$marca),"",fila_actual$marca))),
                       div(span(strong("Potencia: "), ifelse(is.na(fila_actual$potencia),"",fila_actual$potencia))),
                       div(span(strong("Ubicación: "), ifelse(is.na(fila_actual$ubicacion),"Sin Info",fila_actual$ubicacion)))

                     )

              ),
              column(7,
                     uiOutput("foto_equipo_actual")
              )

            )

          )

        })
        
        

        
        
        output$plot0 <- renderHighchart({

            
            if (nrow(temp1$data)==0){
                NULL
            } else {

              # 
              
              
              bind_rows(resultados_filtro$periodo1,resultados_filtro$periodo2) %>% 
                
                
                hchart(type = "line",hcaes(x=date,y=mp2_5, group = Periodo)) %>% 
                
                #    hc_add_series(resultados_filtro$periodo2, type = "line",hcaes(x=hora_hh_mm_ss,y=temperatura)) %>% 
                
                hc_title(text=list("PM 2.5")) %>% 
                
                hc_yAxis(
                  gridLineColor = "#E5E5E5",
                  title = list(text = "PM 2.5 [ppm]")) %>%
                hc_xAxis(title = list(text = "")) %>% 
                
                hc_add_theme(hc_theme_smpl()) 
              

                
            }
            

            
            
            
        })
        

        # plot temperatura -----------
        
        output$plot1 <- renderHighchart({
            
            if (nrow(temp1$data)==0){
                NULL
            } else {
              
              
              bind_rows(resultados_filtro$periodo1,resultados_filtro$periodo2) %>% 
              

              
                hchart(type = "line",hcaes(x=date,y=temperatura, group = Periodo)) %>% 
                
            #    hc_add_series(resultados_filtro$periodo2, type = "line",hcaes(x=hora_hh_mm_ss,y=temperatura)) %>% 
                
                     hc_title(text=list("Temperatura Media [ºC]")) %>% 
                
                hc_yAxis(
                  gridLineColor = "#E5E5E5",
                  title = list(text = "Temperatura [ºC]")) %>%
                hc_xAxis(title = list(text = "")) %>% 
                
                hc_add_theme(hc_theme_smpl()) 
              

                
            }

        })
        
        
        output$plot2 <- renderHighchart({
            
            if (nrow(temp1$data)==0){
                NULL
            } else {
                

              
              bind_rows(resultados_filtro$periodo1,resultados_filtro$periodo2) %>% 
                
                
                hchart(type = "line",hcaes(x=date,y=co2, group = Periodo)) %>% 
                
                #    hc_add_series(resultados_filtro$periodo2, type = "line",hcaes(x=hora_hh_mm_ss,y=temperatura)) %>% 
                
                hc_title(text=list("Dióxido de Carbono")) %>% 
                
                hc_yAxis(
                  gridLineColor = "#E5E5E5",
                  title = list(text = "co2 [ppm]")) %>%
                hc_xAxis(title = list(text = "")) %>% 
                hc_add_theme(hc_theme_smpl()) 
                
            }
            
            
            
        })
        
        
        output$plot3 <- renderHighchart({
            
            if (nrow(temp1$data)==0){
                NULL
            } else {
                

              
              bind_rows(resultados_filtro$periodo1,resultados_filtro$periodo2) %>% 
                
              
              bind_rows(resultados_filtro$periodo1,resultados_filtro$periodo2) %>% 
                
                hchart(type = "line",hcaes(x=date,y=mp10, group = Periodo)) %>% 
                
                #    hc_add_series(resultados_filtro$periodo2, type = "line",hcaes(x=hora_hh_mm_ss,y=temperatura)) %>% 
                
                hc_title(text=list("PM 10")) %>% 
                
                hc_yAxis(
                  gridLineColor = "#E5E5E5",
                  title = list(text = "PM 10 [ppm]")) %>%
                hc_xAxis(title = list(text = "")) %>% 
                hc_add_theme(hc_theme_smpl()) 
              
              
                
            }
            
            
        })
        
        
        output$plot4 <- renderHighchart({
          
          if (nrow(temp1$data)==0){
            NULL
          } else {
            
            
            
            bind_rows(resultados_filtro$periodo1,resultados_filtro$periodo2) %>% 
              
              
              hchart(type = "line",hcaes(x=date,y=humedad, group = Periodo)) %>% 
              
              #    hc_add_series(resultados_filtro$periodo2, type = "line",hcaes(x=hora_hh_mm_ss,y=temperatura)) %>% 
              
              hc_title(text=list("Humedad Relativa [%]")) %>% 
              
              hc_yAxis(
                gridLineColor = "#E5E5E5",
                title = list(text = "Humedad Relativa [%]")) %>%
              hc_xAxis(title = list(text = "")) %>% 
              
              hc_add_theme(hc_theme_smpl()) 
            
            
            
          }
          
          
        })
        
        output$plot5 <- renderHighchart({
          
          if (nrow(temp2$data)==0){
            NULL
          } else {
            
            
            
            temp2$data %>% 
              
              
              hchart(type = "line",hcaes(x=date,y=value)) %>% 
              
              #    hc_add_series(resultados_filtro$periodo2, type = "line",hcaes(x=hora_hh_mm_ss,y=temperatura)) %>% 
              
              hc_title(text=list("Consumo AC [kWh/mes]")) %>% 
              
              hc_yAxis(
                gridLineColor = "#E5E5E5",
                title = list(text = "Consumo [KWh/mes]")) %>%
              hc_xAxis(title = list(text = "")) %>% 
              
              hc_add_theme(hc_theme_smpl()) 
            
            
            
          }
          
          
        })
        
        
    })
    
    

    
    # observeEvent(input$descargar_datos,{
    #   
    #   
    #   
    #   temp1$data %>% openxlsx::write.xlsx(sensor)
    #   
    # })
    
    
    output$descargar_datos <- downloadHandler(
      filename = function() {
        paste0(temp1$data %>% slice(1) %>% pull(nombre_del_sensor), '.xlsx')
      },
      content = function(con) {
        
        openxlsx::write.xlsx(x = temp1$data %>% select(-x1), file = con)
        
        #readr::write_excel_csv2(x = temp1$data %>% select(-x1), file = con,)
        
        #write.csv(temp1$data, con,row.names = FALSE,dec = ",",sep=";")
      }
    )
    
    
    output$boton_descargar_datos <- renderUI({
      
      req(input$mapa_marker_click$id)
      
      div(
      
      shinyWidgets::downloadBttn(
        outputId = "descargar_datos",
        
        label = "Descargar datos",
        style = "bordered",
        color = "primary",
        #size = "md",
        block = TRUE,
        no_outline = TRUE
        #,icon = shiny::icon("download")
      ))
      
    })
    
    
    output$ui_boton_filtrar <- renderUI({
      
      req(input$mapa_marker_click$id)
      
      div(
        shinyWidgets::actionBttn(
          inputId = "filtrar_fechas",
          label = "Filtrar", 
          style = "bordered",block = TRUE,
          color = "success"
          #,icon = icon("sliders")
        )
      )
      
    })
    
    
    output$mapa <- renderLeaflet({

      
      casas$lat = rnorm(nrow(casas),mean = 0, sd = 0.01)
      casas$lon = rnorm(nrow(casas),mean = 0, sd = 0.01)
      
      casas %>%  mutate(latitud = latitud + lat, longitud = longitud + lon) %>% 
        
        leaflet() %>% 
            
            addTiles(group = "Satélite",
                     attribution = 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS Comm.',
                     urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}'
                     
            ) %>% 
            
            addTiles(group = "Open Street Map") %>% 
            
            addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>% 
            

            
            addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
            addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
            
        #    setView(lng = -70.6341 , lat = -33.4369, zoom = 4) %>%  
            
            addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE) %>% 
            
            addLayersControl(
                baseGroups = c("Open Street Map","Satélite","Toner Lite","Toner","CartoDB"),
              #  overlayGroups = c("Ver/Ocultar Leyenda"),
                options = layersControlOptions(collapsed = TRUE)
            ) %>% 
            
            addCircleMarkers(lng = ~longitud, lat = ~latitud,layerId = casas$ID_Casa
                             #,radius = 800
                             )
        
        
        
        
        
    })
    

}

shinyApp(ui, server)
