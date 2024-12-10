
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(leaflet)
library(shiny)
library(shinythemes)

var_texto = "
Esta aplicación shiny tiene el objetivo de demostrar 
los conocimientos adquiridos
en la construcción de un cuadro de mandos básico con R.
"
var_texto_mitfm = "
La idea inicial que pretendo con mi TFM es hacer un estudio de las clínicas dentales. 
Que variables marcan la diferencia en el éxito de estas y como se puede maximizar. 
Mi empresa Dental Data se encarga de registrar todos los datos operativos y gastos 
de nuestros clientes, las clínicas. Este estudio puede ser de gran valor para el sector y para mi empresa.
"

GDatasets = c("advertising.csv","datos_salarios.csv","datos_ordenadores.csv")
datos <- read.csv("advertising.csv")
names(datos) = c("Id","TVPubl","RadioPubl","PeriodicosPubl","Ventas")
var_selec = c(2:5)

GLugares <- c("Sevilla","Córdoba","Londres","Edimburgo", "Vaticano")

Gdatos <- reactiveValues(datos = NULL, Nombres = NULL, var_selec = NULL)

GLatLong <- data.frame(
  Lat = c(37.35945,37.886129,51.500818,55.95710112,41.90234465),
  Long = c(-5.98814,-4.867665,-0.124510,-3.187314305,12.4568083)
)

ui <- fluidPage(
  
    #Tema elegido
    theme=shinythemes::shinytheme(theme = "readable"),
    
    includeCSS("www/estilos.css"),
    
    # titlePanel  ------------------------------------------
    titlePanel(strong("Inteligencia de Negocio con R: DSBI",
                      style="color:blue;"),
               windowTitle = "Inteligencia de Negocio con R: DSBI"),
    
    # navlistPanel  ----- -----------
    navlistPanel(widths=c(3,9),
                 # tabPanel: Información  ------------------------------------
                 tabPanel("Información",icon = icon("info"),  
                          div(class="center",
                              img(src="portadashiny.png",height="200px"), 
                              img(src="hex_shiny.jpg",height="200px")
                          ),
                          br(),br(),br(),
                          hr(),
                          h3("Objetivo:",class="estiloh3"),
                          var_texto,
                          h3("Autor:",class="estiloh3"),
                          strong("Alvaro Racero Armario"),
                          h3("Resumen de mi Trabajo Fin de Máster:",class="estiloh3"),
                          var_texto_mitfm
                 ),
                 
                 # tabPanel: Datos  -------------------------------------------  
                 tabPanel("Datos",icon = icon("database"),
                          
                          fluidRow(
                            #Primera columna subir CSV
                            column(width=3,
                                   fileInput(
                                     "file",
                                     "Elige un fichero CSV para subir",
                                     multiple = FALSE,
                                     placeholder = "Ningún fichero subido",
                                     accept = c(
                                       "text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv"
                                     )
                                   )
                            ),
                            #Segunda columna vacia
                            column(width=2),
                            #Tercera columna checkbox
                            column(width=2,
                                   checkboxInput("header","Tiene nombres Columnas",TRUE)
                                   ),
                            #Cuarta columna separador
                            column(width=3,
                                  radioButtons(inputId = "sep",
                                              label = "Separador",
                                              inline = TRUE,
                                              selected = "," ,
                                              choices = list("Comma" = ",", "Semicolon" = ";","Tab"="\t"))
                            ),
                            
                          ),
                          
                          #Selecciona un dataset
                          shiny::selectInput("SelDataset","Selecciona un dataset",
                                             choices = GDatasets,
                                             selected = GDatasets[1]),
                          DTOutput("tabla")
                 ),  
                 
                 # tabPanel: Estudio descriptivo  ----------------------------------------------------------
                 tabPanel("Estudio Descriptivo",icon = icon("chart-bar"),
                          # fluidRow: selecciona variable
                          fluidRow(
                            column(width=4,
                                   shiny::selectInput("Selvariable01Uni","Selecciona variable",
                                                      
                                                      choices = names(datos)[c(var_selec)],
                                                      selected = names(datos)[var_selec[1]])
                            )
                          ),      
                          
                          # tabsetPanel: Unidimensional-----------------------------------------
                          tabsetPanel(
                            # tabPanel: Resumen Numérico ----
                            tabPanel("Resumen Numérico",
                                     shiny::verbatimTextOutput("OUTResNum"),
                                     

                            ),
                               
                            # tabPanel: Gráficos Unidimensionales -------------------     
                            tabPanel("Gráficos Unidimensionales",

                                     uiOutput("title_unidim"),
                                     #plotOutput("unidim_plot")
                                     plotlyOutput("unidim_plot")
                            ),
                            
                            # tabPanel: Regresión Lineal --------------------------------
                            tabPanel("Regresión Lineal",
                                     
                                     fluidRow(
                                       # Seleccion variable dependiente
                                       column(width=4,
                                              shiny::selectInput("Selvariable01Bid","Selecciona variable dependiente (Y)",
                                                                 choices = names(datos)[c(var_selec)],
                                                                 selected = names(datos)[var_selec[1]])
                                                                #Necesito establecer Gdatos primero
                                                                #choices = Gdatos$Nombres[Gdatos$var_selec],
                                                                #selected = Gdatos$Nombres[Gdatos$var_selec[1]])
                                       ),
                                       #Columna espacio en blanco
                                       column(width=2),
                                       # Seleccion variable independiente
                                       column(width=4,
                                              shiny::selectInput("Selvariable02Bid","Selecciona variable independiente (X)",
                                                                 choices = names(datos)[c(var_selec)],
                                                                 selected = names(datos)[var_selec[1]])
                                       )
                                     ),
                                     
                                     # Boton calcular regresion
                                     actionButton("regression", "Calcular regresión",icon = icon("calculator")),
                                     
                                     fluidRow(
                                       column(width=6,
                                              shiny::verbatimTextOutput("reg_sum")),
                                       column(width=6,
                                              plotOutput("reg_plot",width = "100%",height = "400px"))
                                     )
                            )
                            
                          )
                 ),
                  
                            
                  # tabPanel: MAPAS ------------------------------------------------
                  tabPanel("Mapas",icon = icon("globe"),
                           #Columnas seleccion lugar y enlaces
                           fluidRow(
                             #Espacio blanco
                             column(width=1),
                             #Seleccionador lugar
                             column(width=4,
                                    shiny::selectInput("SelLugar01","Selecciona Lugar de visita",
                                                        choices = GLugares,
                                                        selected = GLugares[1])
                                    ),
                             column(width=4),
                             #Columna de enlaces bullet list
                             column(width=4,
                                    HTML("<ul>
                                            <li><a href='https://www.bufa.es/google-maps-latitud-longitud/'>Cómo obtener latitud-longitud</a></li>
                                            <li><a href='https://www.youtube.com/watch?v=AInC_XTANrQ'>¿Cómo obtener latitud-longitud(youtube)</a></li>
                                          </ul>"))
                           ),
                           
                           #Mapa para mandar a server
                           leafletOutput("map", width = "100%", height = 600)
                  ),
                 
                  # tabPanel: Biografías --------------------------------
                 
                  tabPanel("Biografias",icon = icon("user"),  
                           #Selector de personaje
                           selectInput("biografia_selector", "Selecciona un personaje:",
                                       choices = c("Albert Einstein", "Nikola Tesla", "Marie Curie")),
                           #Ponemos output de imagen y de texto
                           div(style = "display: flex",
                               div(style = "flex: 0 0 200px;", uiOutput("biografia_imagen")),
                               div(style = "margin-left: 20px; max-width: 300px;", uiOutput("biografia_texto"))
                           )
                  )
    )
)

              
      
#Parte server
server <- function(input, output,session) {
  
  
    observeEvent(input$file, {
      GDatasets[4] <- "datos_subidos.csv"
      updateSelectInput(session,"SelDataset",choices = GDatasets)
    })
  
    #Actualizar los “selectInput()” después de seleccionar otro dataset
    observeEvent(input$SelDataset, {
      if (input$SelDataset %in% c(GDatasets[1], GDatasets[3])) {
        Gdatos$datos = read.csv(input$SelDataset)
        Gdatos$Nombres  = names(Gdatos$datos)
        Gdatos$var_selec = c(2:length(Gdatos$Nombres))
      }
      if (input$SelDataset==GDatasets[2]) {
        Gdatos$datos = read.csv(input$SelDataset,sep=";")
        Gdatos$Nombres  = names(Gdatos$datos) 
        Gdatos$var_selec = c(2:length(Gdatos$Nombres))
      }

      #Si se añade un archivo nuevo
      if (input$SelDataset == "datos_subidos.csv") {
        Gdatos$datos = read.csv(input$file$datapath,header = input$header,sep=input$sep)
        Gdatos$Nombres  = names(Gdatos$datos)
        Gdatos$var_selec = c(1:length(Gdatos$Nombres))
      }
      
      # Visualizacion de la tabla de datos seleccionada ----------------------------
      output$tabla <- renderDT({
        datatable(Gdatos$datos,
                  class = 'cell-border stripe',
                  extensions = "Buttons",
                  options = list(
                    lengthMenu = c(15,30, 50, 200),
                    dom="lBfrtip",
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ),
                  filter = "bot",
        )
      })
      
      #Selector variable unitaria ----------------------------------------------
      shiny::updateSelectInput(session,"Selvariable01Uni","Selecciona variable",
                               choices = Gdatos$Nombres[Gdatos$var_selec],
                               selected = Gdatos$Nombres[Gdatos$var_selec[1]])
      #Selector variable dependiente Y
      shiny::updateSelectInput(session,
                               "Selvariable01Bid","Selecciona variable dependiente (Y)",
                               choices = Gdatos$Nombres[Gdatos$var_selec],
                               selected = Gdatos$Nombres[Gdatos$var_selec[1]])
      
      #Selector variable independiente X
      shiny::updateSelectInput(session,
                               "Selvariable02Bid","Selecciona variable independiente (X)",
                               choices = Gdatos$Nombres[Gdatos$var_selec],
                               selected = Gdatos$Nombres[Gdatos$var_selec[2]])
      
    })
    
  #Resumen numerico ----------------------------------------------------
  output$OUTResNum <- renderPrint({
    variable <- input$Selvariable01Uni
    cat("Variable =", variable, "\n\n")
    summary(Gdatos$datos[, variable])
  })  
  
  # Histograma y/o barplot -------------------------------------------
    output$unidim_plot <- renderPlotly({
      variable <- input$Selvariable01Uni
      
      #Coloreamos y cambiamos el nombre del grafico si es numerico o texto/factor
      if (is.numeric(Gdatos$datos[[variable]])) {
        col <- "blue"
        output$title_unidim <- renderUI({
          h3(paste("Histograma de", variable))
        })
      }
      else {
        col <- "green"
        output$title_unidim <- renderUI({
          h3(paste("Diagrama de barras de", variable))
        })
      }
      # Crear el histograma interactivo de la variable seleccionada con plotly
      plot_ly(x = Gdatos$datos[[variable]], type = "histogram",marker = list(color = col)) %>% 
        layout(xaxis = list(title = variable, showgrid = TRUE, gridcolor = "white"),
               yaxis = list(title = "Count", showgrid = TRUE, gridcolor = "white"),
               plot_bgcolor = "lightgrey",
               bargap=0.001)
      
    })
    
    
    
    # Crear regresión -----------------------------------------------------------------------------
    observeEvent(input$regression, {
      # Obtener las variables seleccionadas para la regresión
      y <- input$Selvariable01Bid
      x <- input$Selvariable02Bid
      
      
      # Realizar la regresión lineal simple
      #PROPUESTA DE MEJORA, REGRESION CON MUCHAS X
      lr <- lm(Gdatos$datos[[y]] ~ Gdatos$datos[[x]])
      
      # Mostrar el resumen de la regresión
      output$reg_sum <- renderPrint({
        cat("Regresión Lineal. Variable Y = ", y,",  Variable X = ",x, "\n\n")
        summary(lr)
      })
      
      # Crear el plot de la variable X y la variable Y
      output$reg_plot <- renderPlot({
        ggplot(Gdatos$datos, aes_string(x = x, y = y)) +
          geom_point() +
          labs(x = x, y = y, title = "Diagrama de dispersión")
      })
    })
    
  
    
    #MAPA --------------------------------------------------------------
    output$map <- renderLeaflet({
      cual = which(input$SelLugar01==GLugares)
      LAT = GLatLong$Lat[cual]
      LONG = GLatLong$Long[cual]
      ZOOM=18
      # Dibuja el mapa!
      leaflet() %>%
        setView(lng=LONG, lat=LAT, zoom=ZOOM ) %>%
        addProviderTiles("OpenStreetMap.Mapnik")
    })  
    
    #BIOGRAFIAS -------------------------------------------------------
    observeEvent(input$biografia_selector, {
      #c("Albert Einstein", "Nikola Tesla", "Marie Curie"))
      if (input$biografia_selector == "Albert Einstein") {
        
        output$biografia_imagen <- renderUI({
          img(src = "albert-einstein.jpeg", style = "width: 200px; height: auto;")
        })
        
        output$biografia_texto <- renderUI({
          HTML('<h3>Albert Einstein</h3>
              <p>Albert Einstein fue un físico alemán de origen judío, nacionalizado después suizo, 
               austriaco y estadounidense. Se le considera el científico más importante, conocido y popular del siglo XX.')
        })
      }
      if (input$biografia_selector == "Nikola Tesla") {
        
        output$biografia_imagen <- renderUI({
          img(src = "nikola-tesla.jpg", style = "width: 200px; height: auto;")
        })
        
        output$biografia_texto <- renderUI({
          HTML('<h3>Nikola Tesla</h3>
              <p>Nikola Tesla fue un inventor, ingeniero eléctrico y mecánico serbio nacionalizado estadounidense,
              célebre por sus contribuciones al diseño del moderno suministro de electricidad de corriente alterna.')
        })
      }
      if (input$biografia_selector == "Marie Curie") {
        
        output$biografia_imagen <- renderUI({
          img(src = "marie-curie.jpg", style = "width: 200px; height: auto;")
        })
        
        output$biografia_texto <- renderUI({
          HTML('<h3>Marie Curie</h3>
              <p>Maria Salomea Skłodowska-Curie, más conocida como Marie Curie o Madame Curie, fue una física y 
               química polaca nacionalizada francesa. Pionera en el campo de la radiactividad, es la primera y 
               única persona en recibir dos premios Nobel en distintas especialidades científicas: Física y Química.')
        })
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
