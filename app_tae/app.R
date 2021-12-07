#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options("rgdal_show_exportToProj4_warnings"="none")
library(shiny)
library(rgdal)
library(leaflet)
library(party)
library(plyr)
library(lubridate)
library(DT)
library(caret)
library(class)
library(e1071)
library(FNN) 
library(gmodels) 
library(psych)
library(readr) #Leer archivo csv
library(dplyr) #Manipular el dataset
library(randomForest)

library(tidyr) #Separar columna
library(ggplot2) #Hacer el mapa



data<-read.csv("base_accidentes.csv",header = TRUE,sep = ";", dec = ",")

data <- data[,-19]
data <- data[,-19]


data <- data %>%
    #Eliminamos parÃ©ntesis
    mutate(Ubicacion2 = gsub('[][]', '', LOCATION)) %>%
    #Separamos columna con separate de tidyr
    separate(Ubicacion2, c("long", "lat"), sep=", ") %>%
    #Convertimos Lat y Long a double o valor numÃ©rico
    mutate(latitude = as.numeric(lat), longitude =as.numeric(long))


#cambio el tipo de dato a Date
data<-mutate(data, fecha = format(strptime(as.character(data$fecha), "%d/%m/%Y"), "%Y-%m-%d"))


semanas <- data.frame(semana = week(data$fecha))
data<-cbind(data, semanas)
comunas <- unique(data$COMUNA)
#Sbarrios <- unique(select(data, COMUNA, BARRIO))
Sbarrios <- unique(data[c("COMUNA","BARRIO")])

#para el mapa
barrios_med = readOGR("barrios_med/Barrio_Vereda.shp","Barrio_Vereda")
datos_barrios <- barrios_med@data
names (datos_barrios)[3] = "BARRIO"
datos_barrios$BARRIO <- iconv(barrios_med@data$NOMBRE,"UTF-8","ISO_8859-1")
datos_barrios$BARRIO <- iconv(datos_barrios$BARRIO, to="ASCII//TRANSLIT")


#grupos de riesgo
barrios_categorias <- read.csv("riesgo_barrio.csv", header = TRUE)
barrios_categorias$BARRIO <- iconv(barrios_categorias$barrio, to="ASCII//TRANSLIT")
datos_listos <- join(datos_barrios, barrios_categorias)

#Interfaz grafica
ui <- fluidPage(title = "PredictCom",
                theme = "estilos.css",
                navbarPage(title=div(),
                           tabPanel("Principal", style = "font-family: Trebuchet MS,Lucida Grande,Lucida Sans Unicode,Lucida Sans,Tahoma,sans-serif; ",
                                    
                                    fluidRow(
                                        column(2,
                                               p('')
                                        ),
                                        column(8, 
                                               p('Según la Organización Mundial de la Salud (OMS) la accidentalidad vial constituye la principal causa de muerte en personas de 40 años en todo el mundo.
                                                 Solo en Medellín se atendieron 1242 casos de accidentes de tránsito, explicando que porcentaje más alto de muertes se dio en personas menores de los 26 años,
                                                 En este trabajo se tiene la meta de construir un modelo que sea capaz de predecir la accidentalidad en Medellín para el año 2020 y 2021.', style = "font-family: Trebuchet MS,Lucida Grande,Lucida Sans Unicode,Lucida Sans,Tahoma,sans-serif;  font-size: 2em; text-align: justify;" )
                                        ),
                                        column(2,
                                               p('')
                                        )
                                    ),
                                    div(id='footer', align='center', style = "font-family: Trebuchet MS,Lucida Grande,Lucida Sans Unicode,Lucida Sans,Tahoma,sans-serif;  font-size: 2em; text-align: justify;",
                                        h4('Realizado por: ', style = "font-family: Trebuchet MS,Lucida Grande,Lucida Sans Unicode,Lucida Sans,Tahoma,sans-serif;font-size: 2em; text-align: justify;"),'Valentina Lopez Montoya, Marisol Gonzalez Madrid, David Alejandro Hernandez', hr())
                                    
                           ),
                           navbarMenu('Historico',
                                      tabPanel("Todos los datos", style = "font-family: Trebuchet MS,Lucida Grande,Lucida Sans Unicode,Lucida Sans,Tahoma,sans-serif; ",
                                               fluidRow(
                                                   column(4,
                                                          dateRangeInput("dates", "Rango de fechas",
                                                                         start = "2014-01-01",
                                                                         end = "2021-12-31",
                                                                         min = "2014-01-01",
                                                                         max = "2021-12-31",
                                                                         separator = " - ")
                                                   ),
                                                   column(4, 
                                                          selectInput("comu",
                                                                      "Comuna:",
                                                                      c("All",
                                                                        unique(as.character(data$COMUNA))
                                                                      )
                                                          )
                                                   ),
                                                   column(4, 
                                                          uiOutput('columns')
                                                   )
                                               ),
                                               
                                               DT::dataTableOutput('tablePred')
                                      ),
                                      tabPanel("Agrupación",
                                               # Sidebar to demonstrate various slider options ----
                                               div(id='divtit', align='center', h3('Análisis de Agrupamiento')),
                                               p('Para el agrupamiento de los barrios utilizamos la K-means para identificar patrones similares en los datos de accidentalidad.
                                   Se obtuvo que el número optimo era de 4, los cuales describimos a continuación:',br(),
                                                 
                                                 strong('-Barrios con los promedio de accidentalidad más altos: '),
                                                 strong(span('CATEGORIA ALTA', style = "color:red")),br(),
                                                 
                                                 strong('-Barrios considerados peligrosos por su promedio de accidentalidad pero no alcanza a ser de los más altos: '),
                                                 strong(span(' CATEGORIA MEDIA ALTA', style = "color:darkorange")),br(),
                                                 
                                                 strong('-Barrios con accidentalidad relativamente bajos: '),
                                                 strong(span('CATEGORIA MEDIA BAJA', style = "color:gold")),br(),
                                                 
                                                 strong('-Barrios relativamente tranquilos y el promedio de accidentalidad es muy bajo: '),
                                                 strong(span(' CATEGORIA BAJA', style = "color:green"))
                                               ),
                                               leafletOutput("mymapcategory")
                                      )                        
                           ),
                           navbarMenu('Prediccion',
                                      tabPanel("Para Comuna", style = "font-family: Trebuchet MS,Lucida Grande,Lucida Sans Unicode,Lucida Sans,Tahoma,sans-serif; ",
                                               
                                               div(id='divtitM', align='center', h2('Predicción para comuna')),
                                               fluidRow(
                                                   column(4,
                                                          selectInput("yearchoice",
                                                                      "Año:",
                                                                      c(2014,2015,2016,2017,2018,2019,
                                                                        2020,2021,2022)
                                                          )
                                                   ),
                                                   column(4,
                                                          selectInput("meschoice",
                                                                      "Mes:",
                                                                      c(1,2,3,4,5,6,7,8,9,10,11,12)
                                                          )
                                                   ),
                                                   column(4, 
                                                          selectInput("comuPred",
                                                                      "Comuna:",
                                                                      unique(as.character(data$COMUNA))
                                                          )
                                                   ),
                                                   column(4, 
                                                          selectInput("escala",
                                                                      "Escala:",
                                                                      c("Mes", "Dia")
                                                          )
                                                   )
                                               ),
                                               fluidRow(
                                                   column(12, align="center",
                                                          actionButton("predictcomuna", "Predicción")
                                                   )
                                               ),
                                               DT::dataTableOutput('tablePredCom'),
                                               div(id='NotaBarr', align='center', strong('Importante: '),p('Para los días faltantes es que no existe predicción', style = "color:red"))
                                               
                                      ),
                                      tabPanel("Por Barrio", style = "font-family: Trebuchet MS,Lucida Grande,Lucida Sans Unicode,Lucida Sans,Tahoma,sans-serif; ",
                                               
                                               div(id='divtitM', align='center', h2('Predicción para barrios')),
                                               fluidRow(
                                                   column(4,
                                                          dateRangeInput("PredBarrdates", "Fecha",
                                                                         start = "2014-01-01",
                                                                         end = "2021-12-31",
                                                                         min = "2014-01-01",
                                                                         max = "2021-12-31",
                                                                         separator = " - ")
                                                   ),
                                                   column(4, 
                                                          selectInput("barrioPred",
                                                                      "Barrio:",
                                                                      unique(as.character(data$BARRIO))
                                                          )
                                                   ),
                                                   column(4, 
                                                          selectInput("escalaBarr",
                                                                      "Escala:",
                                                                      c("Mes", "Dia")
                                                          )
                                                   )
                                               ),
                                               fluidRow(
                                                   column(12, align="center",
                                                          actionButton("predecirBarr", "Predicción")
                                                   )
                                               ),
                                               DT::dataTableOutput('tablePredBarr'),
                                               div(id='NotaBarr', align='center', strong('Importante: '),p('Para los días faltantes es que no existe predicción', style = "color:red"))
                                      )
                           )
                )
)

#app
server <- function(input, output, session) {
    
    output$columns <- renderUI({
        barrios3 <- dplyr::select(filter(data, COMUNA == input$comu), BARRIO)
        selectInput('barr', 'Barrios:', c('All',barrios3))
    })
    
    table <- reactive({
        data <- filter(data, fecha >= input$dates[1]  & fecha <= input$dates[2])
        
        #if(input$comu != "All"){
        #    data <- data[data$COMUNA == input$comu,]
            #data <- filter(data, COMUNA == input$comu)
        #}
        if(input$barr != "All"){
            #data <- data[data$BARRIO == input$barr,]
            data <- filter(data, BARRIO == input$barr)
        }
    })
    
    output$tablePred <- DT::renderDataTable(DT::datatable({
        table()
    }))
    
    
    

    
    output$mymap <- renderLeaflet({
        data2 <- filter(data, fecha >= input$Mapdates[1]  & fecha <= input$Mapdates[2])
        coordenadas <- data.frame("latitude"= data2$latitude, "longitude" = data2$longitude)
        coordenadas%>%leaflet() %>% addTiles() %>% addMarkers(
            clusterOptions = markerClusterOptions()
        )
    })
    
    output$mymapcategory <- renderLeaflet({
        
        #Asignación de colores a la categoria
        datos_listos$riesgo <- gsub(1,"green", datos_listos$riesgo )
        datos_listos$riesgo <- gsub(2,"yellow", datos_listos$riesgo )
        datos_listos$riesgo <- gsub(3,"orange", datos_listos$riesgo )
        datos_listos$riesgo <- gsub(4,"red", datos_listos$riesgo )
        barrios_med@data$NOMBRE <- iconv(barrios_med@data$NOMBRE, to="ASCII//TRANSLIT")
        barrios_med@data <- datos_listos
        m=leaflet(barrios_med) 
        m=addTiles(m)
        m=addPolygons(m,popup=barrios_med@data$NOMBRE,color=barrios_med@data$riesgo)
    })
    
    
    #Tabla prediccion por comuna
    
    data_modelos <- read.csv("base_modelos.csv",header = TRUE,sep = "", dec = ",")
    
    predCom <- eventReactive(input$predictcomuna, {
        predcomuna <- subset(data_modelos, COMUNA == 'Aranjuez' & MES == 12 & YEAR == 2019)
        #predcomuna <- subset(data_modelos, COMUNA == input$comuPred & MES == input$meschoice & YEAR == input$yearchoice)
        #predcomuna <- subset(predcomuna1, fecha >= input$PredComdates[1]  & fecha <= input$PredComdates[2])
        if(input$escala == "Mes"){
            m1comunmes <- rpart(numero_accidentes~YEAR+MES+dia+numcomuna2+festivo+
                                           fines_semana+hora_pico+con_heridos+con_muertos+
                                           solo_danos+atropello+incendio+volcamiento+
                                           caida_ocupante+choque+otro+paso_a_nivel+puente+
                                           via_peatonal+ciclo_ruta+glorieta+paso_elevado+
                                           interseccion+lote_o_predio+paso_inferior+
                                           tramo_de_via+tunel+ponton+Bajo+Medio_bajo+Medio_alto+Alto,
                                data = data_modelos,
                                method = 'anova')
            predict1 <- predict(m1comunmes, predcomuna, OOB = TRUE,
                         type = "response")
            pred <- strtoi(predict1)
            marco <- data.frame(Anio = predcomuna$YEAR, Mes = predcomuna$MES, Predicciones = pred)
            res <- unique(marco)
            res <- res[!is.na(res$Predicciones),]
            res <- res[order(res$Anio, res$Mes),]
        }
        if(input$escala == "Dia"){
            m1comundia <- randomForest(numero_accidentes~YEAR+MES+dia+numcomuna2+festivo+
                                                fines_semana+hora_pico+con_heridos+con_muertos+
                                                solo_danos+atropello+incendio+volcamiento+
                                                caida_ocupante+choque+otro+paso_a_nivel+puente+
                                                via_peatonal+ciclo_ruta+glorieta+paso_elevado+
                                                interseccion+lote_o_predio+paso_inferior+
                                                tramo_de_via+tunel+ponton+Bajo+Medio_bajo+Medio_alto+Alto,
                                            data = predcomuna,
                                            importance = TRUE,ntree = 100)
            pDIA <- predict(m1comundia, predcomuna, OOB = TRUE,
                            type = "response")
            pDIA <- round(pDIA)
            marcoDIA <- data.frame(Anio = predcomuna$YEAR, Mes = predcomuna$MES, Dia = predcomuna$dia, Predicciones = pDIA)
            #Selecciona un solo registro de cada día
            res <- marcoDIA %>%
                group_by(Dia,Mes,Anio) %>%
                slice(1)
            res <- res[order(res$Anio, res$Mes, res$Dia),]
        }
        res
    })
    
    output$tablePredCom <- DT::renderDataTable(DT::datatable({
        predCom()
    }))
    

    
    
    #FIN
    
    #Tabla prediccion por barrio
    
    predBarr <- eventReactive(input$predecirBarr, {
        barriodata <- subset(data_modelos, BARRIO == input$barrioPred)
        if(input$escalaBarr == "Mes"){
            m2barriomes <- randomForest(numero_accidentes~YEAR+MES+dia+numcomuna2+festivo+
                                                fines_semana+hora_pico+con_heridos+con_muertos+
                                                solo_danos+atropello+incendio+volcamiento+
                                                caida_ocupante+choque+otro+paso_a_nivel+puente+
                                                via_peatonal+ciclo_ruta+glorieta+paso_elevado+
                                                interseccion+lote_o_predio+paso_inferior+
                                                tramo_de_via+tunel+ponton+Bajo+Medio_bajo+Medio_alto+Alto,
                                            data = barriodata,
                                            importance = TRUE,ntree = 100)
            
            predictbarmes <- predict(m2barriomes, barriodata, OOB = TRUE,
                                  type = "response")
            predictbarmes <- round(predictbarmes)
            marco <- data.frame(fecha = barriodata$fecha, Anio = barriodata$year, Mes = barriodata$MES, Predicciones = predictbarmes)
            marco <- subset(marco, fecha >= input$PredBarrdates[1]  & fecha <= input$PredBarrdates[2])
            marco <- marco[,-1]
            res <- marco %>%
                group_by(Anio,Mes) %>%
                slice(1)
            res <- res[order(res$Anio, res$Mes),]
        }
        if(input$escalaBarr == "Dia"){
            modeloBarrioDia <- randomForest(numero_accidentes~YEAR+MES+dia+numcomuna2+festivo+
                                                fines_semana+hora_pico+con_heridos+con_muertos+
                                                solo_danos+atropello+incendio+volcamiento+
                                                caida_ocupante+choque+otro+paso_a_nivel+puente+
                                                via_peatonal+ciclo_ruta+glorieta+paso_elevado+
                                                interseccion+lote_o_predio+paso_inferior+
                                                tramo_de_via+tunel+ponton+Bajo+Medio_bajo+Medio_alto+Alto,
                                            data = barriodata,
                                            importance = TRUE,ntree = 100)
            predictbardia <- predict(modeloBarrioDia, barriodata, OOB = TRUE,
                                  type = "response")
            predictbardia <- round(predictbardia)
            marco <- data.frame(fecha = barriodata$fecha, Anio = barriodata$YEAR, Mes = barriodata$MES, Dia =  barriodata$dia, Predicciones = predictbardia)
            marco <- subset(marco, fecha >= input$PredBarrdates[1]  & fecha <= input$PredBarrdates[2])
            marco <- marco[,-1]
            res <- marco %>%
                group_by(Dia,Mes,Anio) %>%
                slice(1)
            res <- res[order(res$Anio, res$Mes, res$Dia),]
        }
        res
    })
    
    output$tablePredBarr <- DT::renderDataTable(DT::datatable({
        predBarr()
    }))
}


shinyApp(ui = ui, server = server)
