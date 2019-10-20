## app.R ##
library(shiny)
library(shinydashboard)
library(shinyBS)
library(ggplot2)
library(leaflet)
library(ggalt)
library(lubridate)
library(dplyr)
library(shinyjs)
library(qpcR)
library(markdown)
# library(plotly)

# setwd("E:/ESTEBAN/DE LA U/02 ESPECIALIZACION/SEMESTRE 1/01 CIENCIA DE LOS DATOS/Shiny/trabajo_final/accidentes")


datos_accidentes<-read.csv("accidentalidad_2014_2019.csv", header = TRUE, sep = ";")

fechas<-as.Date(datos_accidentes$FECHA)
fecha_min<-min(fechas)
fecha_max<-max(fechas)


youtube_video <- '<iframe width="853" height="480" src="https://www.youtube.com/embed/kUZCrEWzXII" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'
style <- "my_css2.css"
footer<-'<div class="row">
          <div class="col-sm-6">
            <p class="footer-links">
        
              <a href="https://github.com/juanrpo19/trabajo_ciencia_datos.git">Acceder al repositorio de git.</a>
            </p>
          </div>
          <div class="col-sm-6">
            <p class="footer-company-name">Accidentalidad en Medellín</p>
          </div>
        </div>'



# ...::: Captura de los datos originales :::...
datos_accidentes<-read.csv("accidentalidad_2014_2019.csv", header = TRUE, sep = ";")

opciones_comuna<-c("TODAS",as.array(as.character(unique(datos_accidentes$COMUNA))))


ui <- dashboardPage(
    dashboardHeader(title = "Accidentalidad Medellín"),
    dashboardSidebar(
        
        sidebarMenu(  #<i class="fas fa-chart-line"></i>
            menuItem("Inicio",tabName = "home_page", icon=icon("fab fa-youtube"))
            ,menuItem("Análisis de accidentes",tabName = "analisis_accidentes",icon = icon("fas fa-poll"))
            ,menuItem("Tendencias",tabName = "tendencia_accidentes",icon = icon("fas fa-chart-line"))
            ,menuItem("Informe Técnico",tabName = "informe_tecnico",icon = icon("fas fa-chart-line"))
        )
    ),
    
    
    dashboardBody(
        tabItems(
            #Homepage
            tabItem(tabName = "home_page",
                    fluidRow(
                        box(title = "Video",HTML(youtube_video),width=700)
                    ),
                    fluidRow(
                        HTML(footer)
                    )
            ),
            
            # corresponde al menuItem "analisis_accidentes"
            tabItem(tabName = "analisis_accidentes",
                    fluidRow(
                       box(width = 12,
                             column(width = 4,
                                selectInput("lista_comuna","Comuna:",choices =  opciones_comuna) 
                            ),
                            column(width = 8,
                                dateRangeInput("rango_fechas", "Seleccione las fechas:", start = "", end = "",
                                                min = fecha_min, max = fecha_max,
                                                separator = " - ", format = "dd/mm/yyyy",
                                                startview = 'year', language = 'es', weekstart = 1)
                                
                            )
                           ,fluidRow(
                               column(width = 1, checkboxInput("chkLunes","Lunes",value=TRUE,width = '100%'))
                               ,column(width = 1 ,checkboxInput("chkMartes","Martes",value=TRUE,width = '100%'))
                               ,column(width = 1,checkboxInput("chkMiercoles","Miércoles",value=TRUE,width = '100%'))
                               ,column(width = 1 ,checkboxInput("chkJueves","Jueves",value=TRUE,width = '100%'))
                               ,column(width = 1 ,checkboxInput("chkViernes","Viernes",value=TRUE,width = '100%'))
                               ,column(width = 1 ,checkboxInput("chkSabado","Sábado",value=TRUE,width = '100%'))
                               ,column(width = 1,checkboxInput("chkDomingo","Domingo",value=TRUE,width = '100%'))
                           )
                           )
                             
                        
                    ),
                    
                    fluidRow(
                        infoBoxOutput("conteo_muertes",width = 3)
                        ,infoBoxOutput("conteo_heridos",width = 3)
                        ,infoBoxOutput("conteo_danios",width = 3)
                        ,infoBoxOutput("conteo_total",width = 3)
                    ),

                    fluidRow(
                        box(title = "Georeferenciación",leafletOutput("graficarMapa"), height = 500)
                        ,box(title ="Distribución de accidentes por día" , plotOutput("graficarBarras", height = 250))
                        ,box(title ="Accidentes por rango de hora" , plotOutput("graficarLineas", height = 250))
                    )
            )
            
            # corresponde al menuItem "tendencia_accidentes"
            ,tabItem(tabName = "tendencia_accidentes",
                    fluidRow(
                        box(title ="Variación Mensual",plotOutput("graficarTendenciaAnual",height = 400))
                        ,box(title ="Variación Anual",plotOutput("graficarTendenciaPorAnio",height = 400))
                        ,box(title ="Accidentalidad por comuna",plotOutput("grfTendenciaPorComuna"))
                    )
            )
            ,tabItem(tabName = "informe_tecnico",
                     uiOutput('pagina_web')
            )
            
         
        )
    )
)


server <- function(input, output, session) { 
    
    # Inserta la pagina Web que contiene el informe técnico
    output$pagina_web<-renderUI({
        includeHTML('www/App_UN_Accidentalidad.html')
    })
    
    
    # Actualiza la información de los infoBox de Muertes
    output$conteo_muertes <- renderInfoBox({
        
        req(input$rango_fechas[1],input$rango_fechas[2],input$rango_fechas[1]<=input$rango_fechas[2])
        fecha_ini<-(input$rango_fechas[1])
        fecha_fin<-(input$rango_fechas[2])
        req(fecha_fin,fecha_ini)
        comuna<-as.character(input$lista_comuna)
        
        vectorDias<-"0"
        vectorDias<-append(c("0"), c("0"), after = length(vectorDias))
        if (input$chkLunes==TRUE){ vectorDias<-append(vectorDias, c("LUNES"), after = length(vectorDias))}
        if (input$chkMartes==TRUE){ vectorDias<-append(vectorDias, c("MARTES"), after = length(vectorDias))}
        if (input$chkMiercoles==TRUE){ vectorDias<-append(vectorDias, c("MIERCOLES"), after = length(vectorDias))}
        if (input$chkJueves==TRUE){ vectorDias<-append(vectorDias, c("JUEVES"), after = length(vectorDias))}
        if (input$chkViernes==TRUE){ vectorDias<-append(vectorDias, c("VIERNES"), after = length(vectorDias))}
        if (input$chkSabado==TRUE){ vectorDias<-append(vectorDias, c("SABADO"), after = length(vectorDias))}
        if (input$chkDomingo==TRUE){ vectorDias<-append(vectorDias, c("DOMINGO"), after = length(vectorDias))}
        
        
        ifelse(comuna=="TODAS"
            ,registro_muertes<-(subset(datos_accidentes
                                      ,subset = (as.Date(datos_accidentes$FECHA) >= as.Date(fecha_ini)
                                                 & as.Date(datos_accidentes$FECHA) <= as.Date(fecha_fin)
                                                 & datos_accidentes$DIA_NOMBRE %in% as.array(vectorDias)
                                                 & datos_accidentes$GRAVEDAD=="MUERTO") 
                                      ,select = c("GRAVEDAD")))
            ,registro_muertes<-(subset(datos_accidentes
                                        ,subset = (as.Date(datos_accidentes$FECHA) >= as.Date(fecha_ini)
                                                   & as.Date(datos_accidentes$FECHA) <= as.Date(fecha_fin)
                                                   & datos_accidentes$COMUNA==comuna
                                                   & datos_accidentes$DIA_NOMBRE %in% as.array(vectorDias)
                                                   & datos_accidentes$GRAVEDAD=="MUERTO") 
                                        ,select = c("GRAVEDAD")))
        )        
        
        ifelse(NROW(registro_muertes$GRAVEDAD)==0
               ,cant_muertes<-0
               ,cant_muertes<-aggregate(registro_muertes$GRAVEDAD~registro_muertes$GRAVEDAD,FUN = length)
        )
        
        infoBox("Muertes", paste0(cant_muertes), icon = icon("fas fa-dizzy"),color = "red")
    })
    
    
    # Actualiza la información de los infoBox de Heridos
    output$conteo_heridos <- renderInfoBox({
        req(input$rango_fechas[1],input$rango_fechas[2],input$rango_fechas[1]<=input$rango_fechas[2])
        fecha_ini<-(input$rango_fechas[1])
        fecha_fin<-(input$rango_fechas[2])
        comuna<-as.character(input$lista_comuna)
        
        
        vectorDias<-"0"
        vectorDias<-append(c("0"), c("0"), after = length(vectorDias))
        if (input$chkLunes==TRUE){ vectorDias<-append(vectorDias, c("LUNES"), after = length(vectorDias))}
        if (input$chkMartes==TRUE){ vectorDias<-append(vectorDias, c("MARTES"), after = length(vectorDias))}
        if (input$chkMiercoles==TRUE){ vectorDias<-append(vectorDias, c("MIERCOLES"), after = length(vectorDias))}
        if (input$chkJueves==TRUE){ vectorDias<-append(vectorDias, c("JUEVES"), after = length(vectorDias))}
        if (input$chkViernes==TRUE){ vectorDias<-append(vectorDias, c("VIERNES"), after = length(vectorDias))}
        if (input$chkSabado==TRUE){ vectorDias<-append(vectorDias, c("SABADO"), after = length(vectorDias))}
        if (input$chkDomingo==TRUE){ vectorDias<-append(vectorDias, c("DOMINGO"), after = length(vectorDias))}

        
        ifelse(comuna=="TODAS"
               ,registro_heridos<-(subset(datos_accidentes
                                          ,subset = (as.Date(datos_accidentes$FECHA) >= as.Date(fecha_ini)
                                                     & as.Date(datos_accidentes$FECHA) <= as.Date(fecha_fin)
                                                     & datos_accidentes$DIA_NOMBRE %in% as.array(vectorDias)
                                                     & datos_accidentes$GRAVEDAD=="HERIDO") 
                                          ,select = c("GRAVEDAD")))
               ,registro_heridos<-(subset(datos_accidentes
                                          ,subset = (as.Date(datos_accidentes$FECHA) >= as.Date(fecha_ini)
                                                     & as.Date(datos_accidentes$FECHA) <= as.Date(fecha_fin)
                                                     & datos_accidentes$COMUNA==comuna
                                                     & datos_accidentes$DIA_NOMBRE %in% vectorDias
                                                     & datos_accidentes$GRAVEDAD=="HERIDO") 
                                          ,select = c("GRAVEDAD")))
        )
        
        ifelse(NROW(registro_heridos$GRAVEDAD)==0
               ,cant_heridos<-0
               ,cant_heridos<-aggregate(registro_heridos$GRAVEDAD~registro_heridos$GRAVEDAD,FUN = length)
        )
        
        infoBox("Heridos", paste0(cant_heridos), icon = icon("fas fa-ambulance"),color = "orange")
    })
    
    
    # Actualiza la información de los infoBox de Solo Daños
    output$conteo_danios <- renderInfoBox({
        req(input$rango_fechas[1],input$rango_fechas[2],input$rango_fechas[1]<=input$rango_fechas[2])
        fecha_ini<-(input$rango_fechas[1])
        fecha_fin<-(input$rango_fechas[2])
        comuna<-as.character(input$lista_comuna)
        
        
        
        vectorDias<-"0"
        vectorDias<-append(c("0"), c("0"), after = length(vectorDias))
        if (input$chkLunes==TRUE){ vectorDias<-append(vectorDias, c("LUNES"), after = length(vectorDias))}
        if (input$chkMartes==TRUE){ vectorDias<-append(vectorDias, c("MARTES"), after = length(vectorDias))}
        if (input$chkMiercoles==TRUE){ vectorDias<-append(vectorDias, c("MIERCOLES"), after = length(vectorDias))}
        if (input$chkJueves==TRUE){ vectorDias<-append(vectorDias, c("JUEVES"), after = length(vectorDias))}
        if (input$chkViernes==TRUE){ vectorDias<-append(vectorDias, c("VIERNES"), after = length(vectorDias))}
        if (input$chkSabado==TRUE){ vectorDias<-append(vectorDias, c("SABADO"), after = length(vectorDias))}
        if (input$chkDomingo==TRUE){ vectorDias<-append(vectorDias, c("DOMINGO"), after = length(vectorDias))}
        
        
        ifelse(comuna=="TODAS"
               ,registro_danios<-(subset(datos_accidentes
                                          ,subset = (as.Date(datos_accidentes$FECHA) >= as.Date(fecha_ini)
                                                     & as.Date(datos_accidentes$FECHA) <= as.Date(fecha_fin)
                                                     & datos_accidentes$DIA_NOMBRE %in% as.array(vectorDias)
                                                     & datos_accidentes$GRAVEDAD=="SOLO DAÑOS") 
                                          ,select = c("GRAVEDAD")))
               ,registro_danios<-(subset(datos_accidentes
                                          ,subset = (as.Date(datos_accidentes$FECHA) >= as.Date(fecha_ini)
                                                     & as.Date(datos_accidentes$FECHA) <= as.Date(fecha_fin)
                                                     & datos_accidentes$COMUNA==comuna
                                                     & datos_accidentes$DIA_NOMBRE %in% as.array(vectorDias)
                                                     & datos_accidentes$GRAVEDAD=="SOLO DAÑOS") 
                                          ,select = c("GRAVEDAD")))
        )
        
        ifelse(NROW(registro_danios$GRAVEDAD)==0
               ,cant_danios<-0
               ,cant_danios<-aggregate(registro_danios$GRAVEDAD~registro_danios$GRAVEDAD,FUN = length)
        )
        
        infoBox("Solo Daños", paste0(cant_danios), icon = icon("fas fa-car-crash"),color = "yellow"
        )
    })
    
    
    # Actualiza la información de los infoBox de Total
    output$conteo_total <- renderInfoBox({
        req(input$rango_fechas[1],input$rango_fechas[2],input$rango_fechas[1]<=input$rango_fechas[2])
        fecha_ini<-(input$rango_fechas[1])
        fecha_fin<-(input$rango_fechas[2])
        comuna<-as.character(input$lista_comuna)
        
        
        vectorDias<-"0"
        vectorDias<-append(c("0"), c("0"), after = length(vectorDias))
        if (input$chkLunes==TRUE){ vectorDias<-append(vectorDias, c("LUNES"), after = length(vectorDias))}
        if (input$chkMartes==TRUE){ vectorDias<-append(vectorDias, c("MARTES"), after = length(vectorDias))}
        if (input$chkMiercoles==TRUE){ vectorDias<-append(vectorDias, c("MIERCOLES"), after = length(vectorDias))}
        if (input$chkJueves==TRUE){ vectorDias<-append(vectorDias, c("JUEVES"), after = length(vectorDias))}
        if (input$chkViernes==TRUE){ vectorDias<-append(vectorDias, c("VIERNES"), after = length(vectorDias))}
        if (input$chkSabado==TRUE){ vectorDias<-append(vectorDias, c("SABADO"), after = length(vectorDias))}
        if (input$chkDomingo==TRUE){ vectorDias<-append(vectorDias, c("DOMINGO"), after = length(vectorDias))}

        
        ifelse(comuna=="TODAS"
               ,registro_total<-(subset(datos_accidentes
                                         ,subset = (as.Date(datos_accidentes$FECHA) >= as.Date(fecha_ini)
                                                    & as.Date(datos_accidentes$FECHA) <= as.Date(fecha_fin)
                                                    & datos_accidentes$DIA_NOMBRE %in% as.array(vectorDias)
                                                    ) 
                                         ,select = c("GRAVEDAD")))
               ,registro_total<-(subset(datos_accidentes
                                         ,subset = (as.Date(datos_accidentes$FECHA) >= as.Date(fecha_ini)
                                                    & as.Date(datos_accidentes$FECHA) <= as.Date(fecha_fin)
                                                    & datos_accidentes$COMUNA==comuna
                                                    & datos_accidentes$DIA_NOMBRE %in% as.array(vectorDias)
                                                    ) 
                                         ,select = c("GRAVEDAD")))
        )
        
        
        ifelse(NROW(registro_total$GRAVEDAD)==0
               ,cant_total<-0
               ,cant_total<-aggregate(registro_total$GRAVEDAD~registro_total$GRAVEDAD,FUN = length)
        )
        
        # cant_total<-aggregate(registro_total$GRAVEDAD~registro_total$GRAVEDAD,FUN = length)
        infoBox("Total", paste0(cant_total), icon = icon("list"),color = "purple"
        )
    })
    

    # ...::: Crea el mapa con los puntos a mostrar :::...
    output$graficarMapa<-renderLeaflet({
        
        req(input$rango_fechas[1],input$rango_fechas[2],input$rango_fechas[1]<=input$rango_fechas[2])
        fecha_ini<-(input$rango_fechas[1])
        fecha_fin<-(input$rango_fechas[2])
        comuna<-as.character(input$lista_comuna)
        
        
        vectorDias<-"0"
        vectorDias<-append(c("0"), c("0"), after = length(vectorDias))
        if (input$chkLunes==TRUE){ vectorDias<-append(vectorDias, c("LUNES"), after = length(vectorDias))}
        if (input$chkMartes==TRUE){ vectorDias<-append(vectorDias, c("MARTES"), after = length(vectorDias))}
        if (input$chkMiercoles==TRUE){ vectorDias<-append(vectorDias, c("MIERCOLES"), after = length(vectorDias))}
        if (input$chkJueves==TRUE){ vectorDias<-append(vectorDias, c("JUEVES"), after = length(vectorDias))}
        if (input$chkViernes==TRUE){ vectorDias<-append(vectorDias, c("VIERNES"), after = length(vectorDias))}
        if (input$chkSabado==TRUE){ vectorDias<-append(vectorDias, c("SABADO"), after = length(vectorDias))}
        if (input$chkDomingo==TRUE){ vectorDias<-append(vectorDias, c("DOMINGO"), after = length(vectorDias))}
        
        
        
        ifelse(comuna=="TODAS"
            ,puntos_accidentes<-(subset(datos_accidentes
                                       ,subset = (as.Date(datos_accidentes$FECHA) >= as.Date(fecha_ini)
                                                  & as.Date(datos_accidentes$FECHA) <= as.Date(fecha_fin)
                                                  & datos_accidentes$DIA_NOMBRE %in% as.array(vectorDias)
                                                  ) 
                                       ,select = c("LATITUD","LONGITUD")))
        
            ,puntos_accidentes<-(subset(datos_accidentes
                                       ,subset = (as.Date(datos_accidentes$FECHA) >= as.Date(fecha_ini)
                                                  & as.Date(datos_accidentes$FECHA) <= as.Date(fecha_fin)
                                                  & datos_accidentes$DIA_NOMBRE %in% as.array(vectorDias)
                                                  & datos_accidentes$COMUNA==comuna) 
                                       ,select = c("LATITUD","LONGITUD")))
        )
            
        lngA=min(puntos_accidentes$LONGITUD)
        lngB=max(puntos_accidentes$LONGITUD)
        latA=min(puntos_accidentes$LATITUD)
        latB=max(puntos_accidentes$LATITUD)
        
        mapa<-leaflet()
        mapa<-addTiles(mapa)
        mapa<-addProviderTiles(mapa,provider <- "OpenStreetMap.Mapnik")
        mapa<-fitBounds(mapa,lng1=lngA,lng2=lngB,lat1=latA,lat2=latB)
        mapa<-addMarkers(mapa,lng=puntos_accidentes$LONGITUD,lat=puntos_accidentes$LATITUD,clusterOptions = markerClusterOptions())
        mapa
        
    })
    
    
    
    #...::: Imprime grafico de barras con los dias de la semana :::...
    # output$graficarBarras<-renderPlotly({
    output$graficarBarras<-renderPlot({    
        req(input$rango_fechas[1],input$rango_fechas[2],input$rango_fechas[1]<=input$rango_fechas[2])
        fecha_i<-(input$rango_fechas[1])
        fecha_f<-(input$rango_fechas[2])
        comuna<-as.character(input$lista_comuna)
        
        vectorDias<-c("0")
        vectorDias<-append(c("0"), c("0"), after = length(vectorDias))
        if (input$chkLunes==TRUE){ vectorDias<-append(vectorDias, c("LUNES"), after = length(vectorDias))}
        if (input$chkMartes==TRUE){ vectorDias<-append(vectorDias, c("MARTES"), after = length(vectorDias))}
        if (input$chkMiercoles==TRUE){ vectorDias<-append(vectorDias, c("MIERCOLES"), after = length(vectorDias))}
        if (input$chkJueves==TRUE){ vectorDias<-append(vectorDias, c("JUEVES"), after = length(vectorDias))}
        if (input$chkViernes==TRUE){ vectorDias<-append(vectorDias, c("VIERNES"), after = length(vectorDias))}
        if (input$chkSabado==TRUE){ vectorDias<-append(vectorDias, c("SABADO"), after = length(vectorDias))}
        if (input$chkDomingo==TRUE){ vectorDias<-append(vectorDias, c("DOMINGO"), after = length(vectorDias))}
        

        ifelse(comuna=="TODAS"
                ,datos<-subset(datos_accidentes,subset = (as.Date(datos_accidentes$FECHA)>=as.Date(fecha_i)
                                                 & as.Date(datos_accidentes$FECHA)<=as.Date(fecha_f)
                                                 & datos_accidentes$DIA_NOMBRE %in% as.array(vectorDias)
                                                ))
        
                ,datos<-subset(datos_accidentes,subset = (as.Date(datos_accidentes$FECHA)>=as.Date(fecha_i)
                                                 & as.Date(datos_accidentes$FECHA)<=as.Date(fecha_f)
                                                 & datos_accidentes$DIA_NOMBRE %in% as.array(vectorDias)
                                                 & datos_accidentes$COMUNA==comuna))
        )

        if(NROW(datos)>0){
            
            datos$DIA_SEMANA<-as.character(datos$DIA_NOMBRE)
            datos_conteo<-aggregate(DIA_SEMANA~DIA_NOMBRE, data=datos,FUN=length)
            names(datos_conteo)<-c("DIA_SEMANA","CONTEO")
            
            datos_conteo$NUM_DIA<- ifelse(datos_conteo$DIA_SEMANA=="DOMINGO",7,
                                          ifelse(datos_conteo$DIA_SEMANA=="SABADO",6,
                                                 ifelse(datos_conteo$DIA_SEMANA=="VIERNES",5,
                                                        ifelse(datos_conteo$DIA_SEMANA=="JUEVES",4,
                                                               ifelse(datos_conteo$DIA_SEMANA=="MIERCOLES",3,
                                                                      ifelse(datos_conteo$DIA_SEMANA=="MARTES",2,
                                                                             ifelse(datos_conteo$DIA_SEMANA=="LUNES",1,0)
                                                                      )
                                                               )
                                                        )
                                                 )
                                          )
            )
            
            datos_conteo<-datos_conteo[order(datos_conteo$NUM_DIA),]
            names(datos_conteo)=c("DIA","CUENTA","NRO_DIA")
            
            
            ggplot(data=datos_conteo,aes(x=reorder(datos_conteo$DIA,datos_conteo$NRO_DIA), y=datos_conteo$CUENTA)) +
                geom_bar(stat="identity", fill="steelblue")+
                xlab("DÍA")+
                ylab("CANTIDAD DE ACCIDENTES")+
                theme(plot.title = element_text(hjust = 0.5,size = 10, face = "bold"))+
                geom_text(aes(label=datos_conteo$CUENTA), vjust=1.6, color="white", size=5)

        }
    })
    
    
    # ...::: Imprime grafico de lineas con las horas del dia :::...
    output$graficarLineas<-renderPlot({
        
        fecha_i<-(input$rango_fechas[1])
        fecha_f<-(input$rango_fechas[2])
        req(fecha_i,fecha_f,fecha_i<=fecha_f)
        comuna<-input$lista_comuna
        
        vectorDias<-"0"
        vectorDias<-append(c("0"), c("0"), after = length(vectorDias))
        if (input$chkLunes==TRUE){ vectorDias<-append(vectorDias, c("LUNES"), after = length(vectorDias))}
        if (input$chkMartes==TRUE){ vectorDias<-append(vectorDias, c("MARTES"), after = length(vectorDias))}
        if (input$chkMiercoles==TRUE){ vectorDias<-append(vectorDias, c("MIERCOLES"), after = length(vectorDias))}
        if (input$chkJueves==TRUE){ vectorDias<-append(vectorDias, c("JUEVES"), after = length(vectorDias))}
        if (input$chkViernes==TRUE){ vectorDias<-append(vectorDias, c("VIERNES"), after = length(vectorDias))}
        if (input$chkSabado==TRUE){ vectorDias<-append(vectorDias, c("SABADO"), after = length(vectorDias))}
        if (input$chkDomingo==TRUE){ vectorDias<-append(vectorDias, c("DOMINGO"), after = length(vectorDias))}

            
        
        ifelse(comuna=="TODAS"
            ,intradia<-subset(datos_accidentes,subset = (as.Date(datos_accidentes$FECHA)>=as.Date(fecha_i)
                                                        & as.Date(datos_accidentes$FECHA)<=as.Date(fecha_f)
                                                        & datos_accidentes$DIA_NOMBRE %in% as.array(vectorDias)
                                                        )
                                                        ,select = c("RANGO_HORA","DIA_NOMBRE"))
                             
            ,intradia<-subset(datos_accidentes,subset = (as.Date(datos_accidentes$FECHA)>=as.Date(fecha_i)
                                                        & as.Date(datos_accidentes$FECHA)<=as.Date(fecha_f)
                                                        & datos_accidentes$COMUNA==comuna
                                                        & datos_accidentes$DIA_NOMBRE %in% as.array(vectorDias)
                                                        )
                                                        ,select = c("RANGO_HORA","DIA_NOMBRE"))
        )
        
        if(NROW(intradia$RANGO_HORA)>0){        
            datos_horas <-  intradia %>%
                group_by(RANGO_HORA, DIA_NOMBRE)%>%
                summarise(
                    CUENTA_DATOS = n(),
                )

            # table.temp <-  datos_completos %>%group_by(MES, PERIODO)%>%
            #     summarise(
            #         frecuencia = n(),
                # )
            p<-ggplot(data =datos_horas, aes(y= CUENTA_DATOS, x = RANGO_HORA,color=factor(DIA_NOMBRE)))+ 
                # geom_point()+
                geom_line()+ theme(legend.position="top")+
                theme_bw()+
                labs(color = "Año")+
                scale_x_discrete("Mes", limits=c(0:23))
            print(p)
            
                
            # ggplot(data = datos_horas, aes(y= CUENTA, x = RANGO_HORA, color=factor(DIA_NOMBRE)))+
            #     geom_point()+
            #     theme_bw()+
            #     labs(color = "Día Semana", x="Hora")+
            #     scale_x_discrete("Hora 24H", limits=names(datos_horas$RANGO_HORA))
        }
    })
    
    
    # ...::: Imprime grafico de multiples lineas con la tendencia Anual :::...
    output$graficarTendenciaAnual<-renderPlot({
        # req(input$rango_fechas[1],input$rango_fechas[2])
        
        datos_completos<-subset(datos_accidentes,subset = (datos_accidentes$PERIODO) %in% c(2014,2015,2016,2017,2018,2019)
                                , select = c("PERIODO","MES"))
        
        
        
        table.temp <-  datos_completos %>%group_by(MES, PERIODO)%>%
            summarise(
                frecuencia = n(),
            )
        p<-ggplot(data =table.temp, aes(y= frecuencia, x = MES,color=factor(PERIODO)))+ 
            # geom_point()+
            geom_line()+ 
            theme(legend.position="top")+
            theme_bw()+
            labs(color = "Año")+
            scale_x_discrete("Mes", limits=c(0:12))
        print(p)
        
        
        

        
    })
    
    
    # ...::: Imprime grafico de multiples lineas con la tendencia Por Año :::...
    output$graficarTendenciaPorAnio<-renderPlot({
        # req(input$rango_fechas[1],input$rango_fechas[2])
        datos_completos<-subset(datos_accidentes,subset = (datos_accidentes$PERIODO) %in% c(2014,2015,2016,2017,2018,2019)
                                , select = c("PERIODO","MES"))
        
        
        datos_completos$OBSERVACION<-1
        datos_completos %>% group_by(PERIODO) %>% 
            summarise(ex = length(OBSERVACION))
        
        datos_resumidos<-datos_completos %>% group_by(PERIODO)%>%summarise(CUENTA = length(OBSERVACION))
        
        
        ggplot(datos_resumidos,aes(x=datos_resumidos$PERIODO,y=datos_resumidos$CUENTA)) +
            geom_bar(stat="identity", fill="steelblue")+
            xlab("AÑO")+
            ylab("CANTIDAD DE ACCIDENTES")+
            theme(plot.title = element_text(hjust = 0.5,size = 10, face = "bold"))+
            geom_text(aes(label=datos_resumidos$CUENTA), vjust=1.6, color="white", size=5)+
            scale_x_continuous(breaks=unique(datos_resumidos$PERIODO))
        
    })



    # ...::: Imprime grafico de multiples lineas con la tendencia Por Comuna :::...
    output$grfTendenciaPorComuna<-renderPlot({
        # req(input$rango_fechas[1],input$rango_fechas[2])


        datos_completos<-subset(datos_accidentes,subset = (datos_accidentes$PERIODO) %in% c(2014,2015,2016,2017,2018,2019)
                                , select = "COMUNA")


        datos_comuna <- datos_completos%>%
            group_by(COMUNA)%>%
            summarise(
                Accidentes = n(),)

        ggplot(data= datos_comuna, aes(x=reorder(COMUNA,Accidentes), y=Accidentes))+
            geom_bar(stat="identity", fill= c("Green","Green","Green","Green","Green","Green","Green","Yellow","Yellow","Yellow","Yellow","Yellow","Orange","Orange","Orange","Orange","Orange","Red","Red","Red","Red","Red"))+
            theme_minimal()+
            theme_bw()+
            coord_flip() +
            theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
            labs(title="Accidentalidad por comuna", y = "Número accidentes", x = "Comuna")

    })
}

shinyApp(ui, server)
