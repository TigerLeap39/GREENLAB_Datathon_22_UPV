#
# ######################################### SCRIPT ########################################
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(sp)
library(sf)
library(dplyr)
library(waiter)


#carga MAPA
setwd("/Users/ctranspaupv/Desktop/luiza_Mapa_vulnerabilidad_Agraria/mapa/shapefile/")
mapa<-sf::read_sf("muni.shp")#carga el fichero shapefile
mapa<- st_transform(mapa, CRS("+proj=longlat +datum=WGS84 +no_defs"))

#carga dataset
municipios <- read.csv("/Users/ctranspaupv/Desktop/luiza_Mapa_vulnerabilidad_Agraria/mapa/completa_municipios.csv")

#ordena por el código de INE, hace merge por columnas COD_INE de shapemap y de municipios para juntar los datos

mapa<-mapa[order(mapa$COD_INE),] 
vector<-as.data.frame(mapa$COD_INE)
colnames(vector)<-"COD_INE"
municipios<-right_join(municipios,vector,by="COD_INE")

municipios<-municipios[order(municipios$COD_INE),] #ordena por código INE
municipios$MUNICIPIO<-c(mapa$MUNICIPIO) #add new column MUNICIMIO from mapa

#coge las columnas necesarias para dibujar el mapa
municipios<- municipios[c(15,2,8,11,12,13,14)]

#añade datos geométricos a dataframe y elimina una fila que contiene valor -1 (que no está bien)
geo<-mapa$geometry
datos<-st_sf(municipios,geo)
datos<-datos[-c(1),]



##################################### UI DASHBOARD ######################################

ui <- dashboardPage(skin = "green",
    dashboardHeader(title = "Vulnerabilidad Agraria",
    titleWidth = 350
    ),
    ## Sidebar content
    dashboardSidebar(
        width = 350,
        sidebarMenu(
            img(src = "andalucia.png",
                width = "350px", height = "200px"),
            menuItem("Descripción de los Indicadores", tabName = "Desc", icon = icon("list-alt")),
            menuItem("Mapa de los Municípios Andaluces", tabName = "Mapa", icon = icon("map-marker")),
            selectInput("filteredData", "Elige Indicador:", 
                        c("Vulnerabilidad" = "vulner",
                          "Indicador ambiental" = "ind_ambiental", 
                          "Indicador social" = "ind_social",
                          "Indicador economico" = "ind_economico"
                        )),
            div(img(src = "shiny.png",
                width = "70px", height = "70px"),
                style="text-align: center;"),
            p(strong("Creado con", a("R.Shiny",
                             href = "http://shiny.rstudio.com"
            ), "por GREENLAB",
            br(),"para", a("Datathon 2022 UPV",
                                                   href = "https://www.ctranspa.webs.upv.es/val/datathon-2022/")), align = "center")
            )
       
    ),
    dashboardBody(
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
          #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
        ),
        tabItems(
            
            
            #First tab content
            ########leaflet object (map)#########

            tabItem(
              tabName = "Mapa",
             
              useWaiter(),
              
                    #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                    leafletOutput("MapPlot", height = 900)
                    
            ),
            
  
            
            #description tab content
            tabItem(tabName = "Desc",
                    box(
                        width = 6, background = "maroon",
                        strong("VULNERABILIDAD AGRARIA DE LOS MUNICIPIOS ANDALUCES")
                    ),
                    box(
                        width = 6, background = "blue",
                        strong("INDICADOR DE SOSTENIBILIDAD ECONÓMICO")
                    ),
                    tabBox(
                        #title = "Vulnerabilidad",
                        side = "left", height = "250px",
                        selected = "Descripción",
                        tabPanel("Descripción", "El concepto de vulnerabilidad circunscrito a los municipios andaluces se presenta como una noción antagónica a los tres pilares que determinan la sostenibilidad agraria (ambiental, económica y social). Serán diagnosticados como vulnerables aquellos municipios que no puedan garantizar la continuidad de la productividad agraria de sus explotaciones, su sostenibilidad económica, o que presenten importantes desigualdades en la distribución de la renta generada y la contribución a la viabilidad. Este diagnóstico se sintetiza a partir de la triple dimensión recogida en los indicadores social, agrario y económico.  Debido a la correlación existente entre las tres dimensiones, la metodología utilizada elimina la información redundante, siendo finalmente el indicador ambiental el que más información aporta, seguido del económico y por último el social."),
                        
                        tabPanel("Metodología", "En primer lugar, para construir el indicador de sostenibilidad social y el de sostenibilidad ambiental se utilizó la metodología indicada por Pena Trapero (1977) para este fin.
                        Por otra parte, el indicador de sostenibilidad económica fue construido únicamente a partir de la variable subsidios por habitante.",
                        br(),
                        p("Finalmente, para construir el índice de vulnerabilidad general de los municipios andaluces, se sintetizaron los tres indicadores anteriores. Para ello,
                        también se ha seguido la metodología de Pena Trapero (1977) a partir de una métrica estándar. Los municipios fueron agrupados los municipios en cuartiles, 
                        siendo 4 el valor que indica que el municipio está en una situación de mayor vulnerabilidad y 1 el de menor."),
                        br(),
                        p(strong("Referencia:"),
                                 "Pena Trapero, J. B. (1977). Problemas de la medición del bienestar y conceptos afines (Una aplicación al caso español). Madrid, Spain: INE.")),
                        tabPanel("Conclusiones", "Los municipios con un mayor grado de vulnerabilidad se concentran en las cordilleras de Sierra Morena, Subbética, Penibética y Sierra de Cádiz, siendo la provincia de Jaén (Sierra de Segura, Cazorla y Sierra Mágica) la más vulnerable"),
                        tabPanel("Fuente", p("A través del siguiente enlace se ha podido acceder a la fuente de datos abiertos necesaria para realizar este trabajo:"),
                                 a(h4(strong("Sistema de Información Multiterritorial de Andalucia (SIMA).")),
                                   href = "https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/informe/anual?CodOper=b3_151&idNode=23204"))
                    ),
                    
                    tabBox(
                        #title = "Indicador económico",
                        side = "left", height = "250px",
                        selected = "Descripción",
                        tabPanel("Descripción", "Este indicador sintetiza aquellos procesos compatibles con el mantenimiento de los ecosistemas naturales, y, por consiguiente, aquellas acciones capaces de garantizar la continuidad de la productividad agrícola. La construcción se ha realizado a través de la variable Subsidios, que recoge información sobre el subsidio agrario por habitante que proporcionan los municipios, que está relacionado con la temporalidad del trabajo agrario. La unidad de medida de esta variable el tanto por uno."),
                        tabPanel("Conclusiones", "Los municipios costeros con valores iguales a 0 al igual que las áreas de influencia de cada una de las capitales son las menos vulnerables. Los municipios más vulnerables con valores iguales a 1 se localizan en zonas de sierra en las cuales el número de subsidios por habitante es más elevado y las oportunidades de tener trabajos alternativos al agrario son más reducidas.")
                    ),
                    box(
                        width = 6, background = "green",
                        strong("INDICADOR DE SOSTENIBILIDAD SOCIAL")
                    ),
                    box(
                        width = 6, background = "orange",
                        strong("INDICADOR DE SOSTENIBILIDAD  AMBIENTAL")
                    ),
                    tabBox(
                        #title = "Indicador Social,
                        side = "left", height = "250px",
                        selected = "Descripción",
                        tabPanel("Descripción", "Este indicador proporciona información acerca de la distribución de la renta generada por la agricultura, además de la contribución a la viabilidad de los municipios. La construcción se ha realizado a través de tres variables: Población (variación de la población en los municipios Andaluces desde 1990 hasta 2021), Desempleo (índice de desempleo de los municipios andaluces en el año 2021), y Gasto por habitante (gasto por habitante que realiza un municipio). Las unidades de medida de cada uno son, respectivamente: tasa variacional, porcentaje y euros por habitante."),
                        tabPanel("Conclusiones", "Los municipios costeros de Cádiz, junto con algunos de Sevilla y Huelva muestran peor indicadores socioculturales debido los elevados gastos por habitante, el acceso reducido a internet y una tasa de elevada de personas desocupadas.")
                        ),
                    
                    tabBox(
                        #title = "Indicador ambiental",
                        side = "left", height = "250px",
                        selected = "Descripción",
                        tabPanel("Descripción", "El indicador sintetiza aquellos procesos compatibles con el mantenimiento de los ecosistemas naturales, y, por consiguiente, aquellas acciones capaces de garantizar la continuidad de la productividad agrícola. La construcción se ha realizado a través de tres variables: Desnivel (municipios con una pendiente media de desnivel superior al 15% en el año 2019), Distancia a la capital (distancia que hay entre un determinado municipio y la capital de la provincia en el año 2019), y Erosión (porcentaje de la superficie con una erosión moderada, alta o muy grave respecto de la superficie del municipio). Las unidades de medida de cada uno son, respectivamente: kilómetros cuadrados, kilómetros y porcentaje."),
                        tabPanel("Conclusiones", "El indicador muestra que existen zonas especialmente vulnerables situadas en la provincia de Almería, algunas zonas de montaña de la Penibética, Subbética, Sierra Morena y la Sierra de Cádiz.")
                        ),
            )
            
        )
    )
)
#################################   SERVER  DASHBOARD ################################
server <- function(input, output) {
 
   #waiter spiner for loading data
  w <- Waiter$new(
    id = "MapPlot",
    html = tagList(spin_dots()), 
    color = transparent(0)
  )
  
  
  ##############Dibujando el MAPA###############
  data_sel <- reactive({
    select(datos,c(1,input$filteredData,8))
  })
  
    output$MapPlot <- renderLeaflet({
    
       w$show()
      
      

      data<-data_sel()
      
      colnames(data)<-c("MUNICIPIO","Valor","geo")
       
        pal <- colorNumeric("YlOrRd", domain = data$Valor)

        labels <- sprintf("<strong>%s:</strong> %g", datos$MUNICIPIO, data$Valor) %>%
            lapply(htmltools::HTML)
        l <- leaflet(data) %>%
            addTiles() %>%
            addPolygons(
                fillColor = ~ pal(Valor),
                weight = 2,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlightOptions = highlightOptions(
                    weight = 5,
                    color = "white",
                    dashArray = "",
                    fillOpacity = 0.8,
                    bringToFront = TRUE
                ),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")
            ) %>%
            setView(lng = -4.5000000, lat = 37.6000000, zoom = 8) %>%
            leaflet::addLegend(
                pal = pal, values = ~Valor,
                opacity = 0.7, title = "Rango"
            )

            })

   
    
    
}
    

shinyApp(ui, server)

