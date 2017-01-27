
library(shiny)

shinyUI(fluidPage(
     
     titlePanel("Crear lineas de produccion y asignar modelos"),
     sidebarLayout(
          sidebarPanel(
               h4("Te invitamos a que definas tus lineas de produccion utilizando los datos de tu empresa."),
               h5("El archivo debe tener un formato de columnas como se muestra en la figura.  Tus datos 
                  pueden tener 3 o mas puestos o tipos de operador"),
               img(src= "http://www.magro.com.mx/images/formato.PNG", align = "left",
                   width = 200),
               fileInput("browse", "Selecciona archivo CSV",
                         accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")
               ),
               checkboxInput("header", "Datos tienen encabezado", TRUE),
               downloadButton("download","Descargar asignacion")
          ),
          mainPanel(
               # h5("Esta versión permite agrupar 20 estilos, si necesitas agrupar más puedes comprar
               #    la suscripción en Apps/Comprar aplicaciones o enviarnos un correo en la cuenta 
               #    luis@magro.com.mx para ayudarte"),
               h5("Si tienes alguna duda de como funciona esta app, puedes enviarnos un correo a 
                  luis@magro.com.mx para ayudarte o puedes ver el articulo que explica su funcion y 
                  funcionamiento en http://www.magro.com.mx/index.php/news/7-lineasprodcalzado"),
               tabsetPanel(
                    tabPanel("Datos leidos",DT::dataTableOutput("tabla_completa")),
                    tabPanel("Estadistica", 
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
                             plotOutput("dendograma")),
                    tabPanel("Modelos asignados", DT::dataTableOutput("tabla_asignacion",
                                                                      width = 400)),
                    tabPanel("Analisis Final y Medicion de mejora", 
                             tableOutput("mejora"),
                             tableOutput("total.fam"),
                             plotlyOutput("grafico.final", height = "1000px"),
                             DT::dataTableOutput("desviaciones", width = 200))
               )
          )
     )
))
