
library(shiny)
library(plotly)

shinyUI(fluidPage(
     
     titlePanel("Crear lineas de produccion y asignar modelos"),
     sidebarLayout(
          sidebarPanel(
               h4("Guarda el reporte de tiempos de estilos habilitados con CSV"),
               fileInput("browse", "Selecciona archivo CSV",
                         accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")
               ),
               checkboxInput("header", "Datos tienen encabezado", TRUE),
               checkboxInput("personas", "Convertir tiempos a personas (100 prs/hr)", FALSE),
               uiOutput("depto.select"),  #nuevo
               uiOutput("lineas.select"),  #nuevo
               downloadButton("download","Descargar asignacion")
          ),
          mainPanel(
               # h5("Esta versión permite agrupar 20 estilos, si necesitas agrupar más puedes comprar
               #    la suscripción en Apps/Comprar aplicaciones o enviarnos un correo en la cuenta 
               #    luis@magro.com.mx para ayudarte"),
               h5("Si tienes alguna duda de como funciona esta app, puedes enviarnos un correo a 
                  luis@magro.com.mx para ayudarte o puedes ver el articulo que explica su funcion y 
                  funcionamiento en", a("nuestros articulos", href = "http://www.magro.com.mx/index.php/news/7-lineasprodcalzado")),
               tabsetPanel(
                    tabPanel("Datos leidos",
                             DT::dataTableOutput("tabla_completa")
                             ),
                    tabPanel("Estadistica inicial", 
                             tableOutput("tablainicial"),
                             plotlyOutput("boxplotini"),
                             checkboxInput("same.scale.ini", "Usar escala independiente en cada grafico", FALSE),
                             plotlyOutput("graficoinicial", height = "800px")),
                    tabPanel("Lineas de produccion", 
                             column(6, 
                                    sliderInput("altura_cluster", "Indice de desviacion",
                                         min=2, max= 3000,
                                         step = 50, value = 500)),
                             column(6,
                                    p("Lineas de produccion a crear: "),
                                    verbatimTextOutput("lineas")),
                             plotOutput("dendograma", height = "800px")),
                    tabPanel("Modelos asignados", DT::dataTableOutput("tabla_asignacion",
                                                                      width = 400)),
                    tabPanel("Analisis Final y Medicion de mejora", 
                             column(6, h4("Analisis de los tiempos por puesto"),
                                    tableOutput("mejora")),
                             column(6, h4("Estilos asignados por linea"),
                                    tableOutput("total.fam")),
                             column(12,plotlyOutput("grafico.final", height = "1500px")),
                             uiOutput("seleccion_linea"),
                             checkboxInput("same.scale.fin", "Usar escala independiente en cada grafico", FALSE),
                             plotlyOutput("plot.por.linea", height = "600px"),
                             DT::dataTableOutput("desviaciones", width = 200))
               )
          )
     )
))
