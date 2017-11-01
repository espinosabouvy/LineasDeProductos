
library(shiny)
library(plotly)

shinyUI(fluidPage(
     
     titlePanel("Diagnostico, analisis y mejoras de lineas de produccion - MAGRO V5.0 2017"),
     #titlePanel("Diagnostico, analisis y mejoras de lineas de produccion - PERUGIA"),
     sidebarLayout(
          sidebarPanel(
               # #fijar el ancho del left sidebar
               # tags$head(
               # tags$style(type="text/css", "select { max-width: 140px; }"),
               # tags$style(type="text/css", ".span4 { max-width: 190px; }"),
               # tags$style(type="text/css", ".well { max-width: 300px; }")
               # ),
               conditionalPanel(condition= "input.conditionedPanels != 'Analisis de personal'",
                    radioButtons("origen", "Selecciona",
                                 choices = list("Nuevos datos" = 1,
                                                         "Cargar simulacion" = 2)),
                    uiOutput("cargar.archivo"),   #archivo o simulacion
                    actionButton("guardar","Guardar..."), #guardar simulacion
                    uiOutput("status.select"), #selecionar status
                    uiOutput("planta.select"), #seleccionar planta
                    uiOutput("chk.todos.lineas"), #seleccionar todas las lineas
                    uiOutput("lineas.select"),  #seleccionar lineas
                    uiOutput("depto.select"),  #seleccionar depto
                    checkboxInput("agrupado", "Agrupar en una sola unidad de produccion", FALSE),
                    checkboxInput("personas", "Convertir tiempos a personas", TRUE),
                    uiOutput("prs.personas.ui"),   #pares a personas personalizado
                    h3("Configuracion general"),
                    sliderInput("horas.trabajo", "Horas trabajadas por dia",   #horas trabajo por dia
                                min = 1, max = 24, step = 0.1, value = 9.6),
                    sliderInput("eficiencia", "Eficiencia de balanceo/programacion",   #eficiencia de balanceo
                                min=10, max = 130, step = 5, value = 85),
                    sliderInput("precio.prom", "Truput promedio por par",  #truput promedio
                                min = 50, max = 500, step = 10, value = 300),   
                    sliderInput("sueldo.prom", "Sueldo semanal promedio",    #sueldo promedio
                                min = 500, max = 3000, step = 100, value = 1400),  
                    sliderInput("sl.graficos","Escalar altura de graficos",0.5,1.5,1,0.1)   #escalar graficos
               ),
               conditionalPanel(condition= "input.conditionedPanels == 'Analisis de personal'",
                    h3("Pares por producir por dia por unidad"),
                    uiOutput("lineas.pares"),
                    textInput("pares","Pares"),
                    actionButton("agregar","Agregar..."),
                    DT::dataTableOutput("por.producir")
               ),
               width = 3
          ),
          mainPanel(
               h5("Si tienes alguna duda de como funciona esta app, puedes enviarnos un correo a 
                  admin@magro.com.mx para ayudarte. Vrs-Ros5.0"),
               tabsetPanel(
                    tabPanel("Datos Leidos",
                             DT::dataTableOutput("tabla_completa")),
                    tabPanel("Calculos",
                             h2("Definicion de calculos"),
                             h3("ANALISIS DE ASIGNACION"),
                             h4("1. Se considera que todos los estilos se procesan en el departamento
                                PESPUNTE"),
                             h4("2. Se agregan todos los estilos existentes en PESPUNTE a todos los 
                                departamentos en todas las
                                funciones con valor = 0 (de otra forma, las funciones no existentes en
                                estos departamentos no son tomadas en cuenta, cuando en realidad tienen 
                                un tiempo de proceso 0)"),
                             h4("3. Al convertir tiempo en personas, se consideran para fabricar 100 pares
                                por hora"),
                             h3("ANALISIS DE PERSONAL"),
                             h4("1. Requiere un valor de pares por producir para cada linea de produccion"),
                             h4("2. Se calcula y rendondea hacia arriba las personas necesarias por par, se
                                calcula el promedio de personas y nuevamente se redondea hacia arriba, usando
                                los pares a producir en cada linea de produccion"),
                             h4("3. Los pares reales a producir se basan en la restriccion de personas de cada
                                puesto y se utiliza como base de calculo real"),
                             h4("4. La desviacion estandar arriba del promedio permiten no solo utilizar el 
                                promedio de personas requeridas por los estilos, si no aumentan en un
                                porcentaje la plantilla de personal por puesto para lograr mayor complimiento"),
                             h4("5. La facturacion utiliza los pares producidos por la restriccion por el 
                                precio promedio"),
                             h4("6. El costo semanal de mano de obra se obtiene de la plantilla por el 
                                sueldo promedio semanal"),
                             h3("ANALISIS DE DESVIACIONES"),
                             h4("1. El indicador de desviacion es la suma de la desviacion promedio de cada
                                funcion por linea"),
                             h4("2. El valor minimo y maximo es la suma de los maximos promedio de cada 
                                fucnion por linea"),
                             h4("3. El incremento en la capacidad de produccion es el porcentaje de disminucion
                                en el tiempo invertido por el estilo que más consume de cada unidad"),
                             h4("4. Incremento de facturacion semanal, incremento en capacidad de produccion
                                por pares producidos por 5 dias por precio promedio. (Requiere datos de pares a
                                producir por semana por linea)"),
                             h3("ANALISIS DE FLUJO"),
                             h4("1. El analisis de flujo considera 1000 pares por dia si no se definen 
                                pares por producir por unidad"),
                             h4("2. Al seleccionar mas de un estilo se considera producir una cantidad igual
                                de cada uno de ellos"),
                             h4("3. El incremento en la produccion y facturacion considera producir el estilo
                                seleccionado durante una semana")
                    ),
                    tabPanel("Analisis de asignacion", 
                             column(4,tableOutput("total.fam")),
                             column(12, p("Los siguientes graficos permiten entender la forma en que estan
                                          asignados los modelos a cada linea de produccion y revisar si la
                                          asignacion tiene errores, visualmente se puede verificar la 
                                          consistencia en la asignacion.")),
                             checkboxInput("same.scale.inicial", 
                                           "Usar escala independiente en cada grafico", FALSE),
                             column(12,uiOutput("grafico.inicial.ui"))

                             ),                    
                    tabPanel("Analisis de desviaciones",
                             column(4,uiOutput("seleccion_linea")),
                             column(8,sliderInput("quant", "Porcentaje para considerar desviacion",
                                                  min = 0, max = 49, step = 1, value = 10)),
                             column(3,h5("Desviacion al eliminar criticos"),
                                    verbatimTextOutput("nva.desviacion"),
                                    h5("Porcentaje de disminucion de desviacion"),
                                    verbatimTextOutput("pct.mejora"),
                                    h5("Porcentaje de incremento de produccion"),
                                    verbatimTextOutput("incr.produccion"),
                                    h4("Incremento en TRUPUT semanal (pesos)"),
                                    verbatimTextOutput("inc.facturacion")
                                    ),
                             column(3, h5("Mayores causantes de desviacion"),
                                    tableOutput("cancelar.criticos")),
                             column(6, h5("Indicadores de desviacion global de la unidad"),
                                    tableOutput("indicador.desviacion")),
                             checkboxInput("same.scale.fin", 
                                                    "Usar escala independiente en cada grafico", FALSE),
                             column(12,plotlyOutput("plot.por.linea", height = "800px")),
                             DT::dataTableOutput("desviaciones", width = 200)
                         ),
                    tabPanel("Analisis de personal",
                             column(3, sliderInput("sds","Desviaciones estandar arriba del promedio",
                                                   min=0,max = 3, step = 0.1, value = 0),
                                    checkboxInput("redondeo","Redondear personal",value = T)),
                             column(3,h4("Costo MO promedio"),
                                    verbatimTextOutput("mo.promedio")),
                             column(3, h4("TRUPUT"),
                                    verbatimTextOutput("incr.fact.plantilla")),
                             column(3, h4("Total MO"),
                                    verbatimTextOutput("total.mo")),
                             column(3, h4("TRUPUT menos MO"),
                                    verbatimTextOutput("fact.mo")),
                             column(3,h4("Total personas requeridas"),
                                    verbatimTextOutput("grantotal")),
                             column(3,h4("Estilos con cumplimiento min 95%"),
                                    verbatimTextOutput("estilos.con.cumplimiento")),
                             column(12, h4 ("Eficiencia/ Cumplimiento metas por estilo"),
                                    plotlyOutput("eficiencia.estilo")),
                             column(7,h4("Eficiencia esperada por unidad"),
                                    tableOutput("eficiencia.linea")),
                             column(5,h4("Produccion esperada por unidad"),
                                    tableOutput("meta.linea")),
                             column(12,h5(" ")),
                             column(5,h4("Personas por unidad"), 
                                    tableOutput("Totales.por.linea")),
                             column(7, h4("Total personas por puesto"),
                                    tableOutput("total_puesto")),
                             column(12,h4("Personal requerido por unidad de produccion")),
                             column(10,DT::dataTableOutput("PersonalPorlinea")),
                             column(12, h4("Tabla de resultados detallada"),
                                    DT::dataTableOutput("PersonalPorEstilo"))
                    ),
                    tabPanel("Analisis de flujo continuo",
                             column(3, uiOutput("flujo.linea"),
                                    h5("Movimientos requeridos"),
                                    tableOutput("tabla.movimientos")),
                             column(3, uiOutput("flujo.deptos")),
                             column(2, uiOutput("flujo.estilo"),
                                    sliderInput("cambios.max", "No. max de movimientos",
                                                min=1, max = 20, value = 10, step = 1),
                                    sliderInput("cumple.min", "Cumplimiento minimo",
                                                min=50, max = 100, value = 90, step = 1)),
                             column(2, h5("Cumplimiento esperado"),
                                    verbatimTextOutput("cumpl.meta"),
                                    h5("Cumplimiento mejorado"),
                                    verbatimTextOutput("cumpl.mejorado"),
                                    h4("Incremento de produccion semanal"),
                                    verbatimTextOutput("aumento.pares")),
                             column(2, h5("Eficiencia esperada"),
                                    verbatimTextOutput("ef.esperada"),
                                    h5("Eficiencia mejorada"),
                                    verbatimTextOutput("ef.mejorada"),
                                    h4("Incremento en facturacion semanal"),
                                    verbatimTextOutput("aumento.facturacion")),
                             column(12, plotlyOutput("plot.flujo")),
                             column(12, h4("Tabla de datos"),
                                    DT::dataTableOutput("tabla.plot")),
                             column(12, h4("Tabla de datos balanceada"),
                                    DT::dataTableOutput("balanceo"))
                    ),
                    id = "conditionedPanels"
               )
          )
     )
))
