library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)


shinyServer(function(input, output, session) {
     options(shiny.reactlog=TRUE)      
     options(shiny.maxRequestSize=15*1024^2)
     options(shiny.sanitize.errors = FALSE)

     
     #reset para graficos, lo requiere plotly
     pdf(NULL)
     
     #bandera para cargar parametros
     file.guardardo = FALSE
     
     #tabla vacia para cargar pares por linea
     tb.porproduc <- data.frame("Linea" = numeric(0), "Pares" = numeric(0))
     parametros <- NULL
     
      #ruta <- "/var/shiny-server/www/shiny-test/saved"  #server
      #rutacombo <- paste0(ruta, "/")  #server
      ruta <- ""  #local
      rutacombo <- ""  #local
     
     output$cargar.archivo <- renderUI({
          if (input$origen == 1) {
               fileInput("browse", "Selecciona archivo CSV ",
                         accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv",
                              ".Rdata")
               ) 
          }  else {
               #para cuando se usa local y en server
               if (ruta==""){
                    choices <- c("Simulaciones disponibles", 
                                 list.files(pattern="*Rdata"))
               } else {
                    choices <- c("Simulaciones disponibles",
                                 list.files(path = rutacombo, pattern="*Rdata"))
               }
               selectInput("archivo.sim","Selecciona una simulacion", choices,
                           multiple = F)
          }
     })
     
     observeEvent(input$guardar,{
          archivos <- c("datos","parametros", "tb.porproduc", "reporte.final.magro")
          
          datos <- lectura.inicial()
          parametros <- reactiveValuesToList(input)
          reporte.final.magro <- reporte.final()
          
          deptos <- paste0(input$depto.selected, collapse = "-")
          unidades <- paste0(input$lineas.selected, collapse = "-")
          #save(list = archivos , file = paste0(rutacombo,"test.Rdata"),
          save(list = archivos , file = paste0(rutacombo,deptos,"--",unidades,"--","(",Sys.Date(),")",".Rdata"),
               precheck = TRUE)
          showNotification("Simulacion guardada", duration = 3)
     })  
     
     lectura.params <- reactive({
          if (input$origen==1) return(NULL)
          
          #si no se ha seleccionado simulacion
          if (input$archivo.sim == "Simulaciones disponibles") return(NULL)
          
          #regresa la tabla original de tiempos
          load(paste0(rutacombo, input$archivo.sim))
          
          return(parametros)
     })
     
     lectura.pares.unidad <- reactive({
          if (input$origen==1) return(NULL)
          
          #si no se ha seleccionado simulacion
          if (input$archivo.sim == "Simulaciones disponibles") return(NULL)
          
          #regresa la tabla original de tiempos
          load(paste0(rutacombo, input$archivo.sim))
          
          return(tb.porproduc)
     })
     
     observeEvent(input$archivo.sim,{
          parametros <<- lectura.params()
          tb.porproduc <<- lectura.pares.unidad()
          
          #si no se ha seleccionado simulacion
          if (input$archivo.sim == "Simulaciones disponibles") return(NULL)
          
          if (is.null(parametros)){}else{
               
               #orden de actualizacion de parametros
               updateCheckboxInput(session, "personas",value = parametros$personas)
               updateSliderInput(session, "pares.personas",value = parametros$pares.personas)
               updateCheckboxInput(session, "agrupado",value = parametros$agrupado)
               updateSliderInput(session, "horas.trabajo",value = parametros$horas.trabajo)
               updateSliderInput(session, "sueldo.prom",value = parametros$sueldo.prom)
               updateSliderInput(session, "horas.trabajo",value = parametros$horas.trabajo)
               updateSliderInput(session, "eficiencia",value = parametros$eficiencia)
               updateSliderInput(session, "precio.prom",value = parametros$precio.prom)
               updateSliderInput(session, "sl.graficos",value = parametros$sl.graficos)
               
               #General - imprime tabla de pares por producir
               output$por.producir <- DT::renderDataTable({
                    DT::datatable(tb.porproduc, options = list(dom = 't'))
               })
          }
     })
     
     #lectura inicial de datos desde archivo y transformacion al formato requerido de estilo-puesto1-puesto2
     lectura.inicial <- reactive({
          require(dplyr)
          require(tidyr)
          
          if (input$origen==1){  #por archivo
               inFile <- NULL
               inFile <- input$browse          
               if (is.null(inFile))return(NULL)
               
               myClasses <- c(Estilo = "factor")
               
               #leer por tipo de archivo
               tiempos.raw <- read.csv(inFile$datapath, stringsAsFactors = F, na.strings = " -   ",
                                       colClasses = myClasses)
               
               tiempos <- tiempos.raw%>%
                    select("PLANTA" = Planta, "STATUS" = Status, "DEPTO" = Departamento, 
                           "ESTILO" = Estilo, "LINEA" = Unidad,
                           "FUNCION" = Puesto, "TIEMPO" = Tiempos )
               
               return(tiempos)   
          } else {  #por simulacion guardada
               
               #si no se ha seleccionado simulacion
               if (input$archivo.sim == "Simulaciones disponibles") return(NULL)
                    #regresa la tabla original de tiempos
                    load(paste0(rutacombo, input$archivo.sim))
                    return(datos)
          }
     })
     
     
     
     #filtrar status, planta, unidades, depto
     #llenar combo de status con el archivo preparado
     output$status.select <- renderUI({
          datos <- lectura.inicial()
          
          if (is.null(datos)){
               #sin archivo seleccionado
               return(NULL)
          }
          
          status <- unique(datos%>%select(STATUS))
          selectInput("status.selected", "Selecciona los status que quieres analizar", as.list(status),
                      multiple = T)
          
          #cargar seleccion de simulacion
          if(is.null("parametros")){
               selectInput("status.selected", "Selecciona los status que quieres analizar", as.list(status),
                           multiple = T)
          } else {
               selectInput("status.selected", "Selecciona los status que quieres analizar", as.list(status),
                           multiple = T, selected = parametros$status.selected)
          }
     })
     
     #llenar combo de planta de produccion
     output$planta.select <- renderUI({
          datos <- lectura.inicial()
          
          if (is.null(datos)){
               #sin archivo seleccionado
               return(NULL)
          }
          
          if (is.null(input$status.selected)) return(NULL)
          
          #solo las plantas que aparezcan en el status
          plantas <- unique(datos%>%filter(STATUS %in% input$status.selected)%>%select(PLANTA))
          
          if(is.null("parametros")){
               selectInput("plantas.selected", "Filtra las plantas que quieres analizar", as.list(plantas),
                           multiple = TRUE)
          } else {
               selectInput("plantas.selected", "Filtra las plantas que quieres analizar", as.list(plantas),
                           multiple = TRUE, selected = parametros$plantas.selected)
          }
          
          
     })
     
     #llenar combo de lineas ya asignadas, por si solo se quiere hacer el ejercicio con alguna linea
     output$lineas.select <- renderUI({
          datos <- lectura.inicial()
          
          if (is.null(datos)) return(NULL)
          
          if (is.null(input$status.selected)) return(NULL)
          if (is.null(input$plantas.selected)) return(NULL)
          
          lineas <<- unique(datos%>%filter(PLANTA %in% input$plantas.selected & 
                                                STATUS %in% input$status.selected)%>%select(LINEA))%>%arrange()
          
          if(is.null("parametros")){
               selectInput("lineas.selected", "Filtra las unidades que quieres analizar", as.list(lineas),
                           multiple = TRUE)
          } else {
               selectInput("lineas.selected", "Filtra las unidades que quieres analizar", as.list(lineas),
                           multiple = TRUE, selected = parametros$lineas.selected)
          }
          
          
     })
     
     #checkbox seleccionar todos
     output$chk.todos.lineas <- renderUI({
          datos <- lectura.inicial()
          if (is.null(datos)) return(NULL)
          
          
          if (is.null(input$status.selected)) return(NULL)
          if (is.null(input$plantas.selected)) return(NULL)
          
          actionLink("todas.lineas", "Seleccionar todas las unidades disponibles")
     })
     
     #seleccionar todas las lineas
     observeEvent(input$todas.lineas,{
          
          if (length(lineas)==0) return(NULL)
          updateSelectInput(session, "lineas.selected", selected = lineas$LINEA)
          
     })
     
     #llenar combo de departamentos con el archivo preparado
     output$depto.select <- renderUI({
          datos <- lectura.inicial()
          
          if (is.null(datos)){
               #sin archivo seleccionado
               return(NULL)
          }
          
          if (is.null(input$status.selected)) return(NULL)
          if (is.null(input$plantas.selected)) return(NULL)
          if (is.null(input$lineas.selected)) return(NULL)
          
          
          deptos <- unique(datos%>%filter(PLANTA %in% input$plantas.selected & 
                                               STATUS %in% input$status.selected & 
                                               LINEA %in% input$lineas.selected)%>%
                                select(DEPTO))
          
          if(is.null(parametros)){
               selectInput("depto.selected", "Selecciona los departamento que quieres analizar", as.list(deptos),
                           multiple = T)
               
          } else {
               selectInput("depto.selected", "Selecciona los departamento que quieres analizar", as.list(deptos),
                           multiple = T, selected = parametros$depto.selected)
          }
     })
     
     
     #personalizar pares a personas
     output$prs.personas.ui <- renderUI({
          if (input$personas) {
               sliderInput("pares.personas","Pares por hora",1,200,20,1,round = T)
          }
     })
     
     #REPORTE FINAL
     reporte.final <- function(personas.sin = TRUE){
          datos <- lectura.inicial()
          
          if (is.null(datos)) return(NULL)
          
          if (is.null(input$status.selected)) return(NULL)
          if (is.null(input$plantas.selected)) return(NULL)
          if (is.null(input$lineas.selected)) return(NULL)
          if (is.null(input$depto.selected)) return(NULL)
          
          #si se selecciona más de un departamento, los junta como si fuera uno solo
          if (length(input$depto.selected)>1){
               nombre.depto <- paste(input$depto.selected, collapse = "-")
               datos <- datos%>%
                    filter(DEPTO %in% input$depto.selected)
               datos$DEPTO <- nombre.depto
          } else {
               nombre.depto <- input$depto.selected
          }
          
          #convertir NAS en cero
          datos[is.na(datos)] <- 0
          
          #convertir tiempos en segundos
          datos$TIEMPO <- ceiling(datos$TIEMPO*60)
          
          #se corrigio para esta version, primero suma los tiempos y luego redondea personas
          #antes redondeaba, por lo tanto, creaba minimo 1 para cada renglon cargado
          #en los tiempos, aún cuando es correcto, puede generar información rara y depender
          #demasiado de la forma en que se cargan los tiempos en sistema, por ejemplo, si es
          #o no una secuencia o si agrupan funciones
          
          #agrega con cero las funciones, filtra linea y familias(si existen)
          #suma los tiempos redondeados en segundos
          temp <- datos%>%
               filter(PLANTA %in% input$plantas.selected &
                           DEPTO %in% nombre.depto &
                           LINEA %in% input$lineas.selected & 
                           STATUS %in% input$status.selected)%>%
               select(ESTILO, LINEA, FUNCION, TIEMPO)%>%
               group_by(ESTILO, LINEA, FUNCION)%>%
               summarise("TIEMPO" = ceiling(sum(TIEMPO)))
          
          #convertir tiempos en personas
          if (personas.sin == FALSE){
               if (input$personas){
                    temp$TIEMPO <- ceiling((temp$TIEMPO*input$pares.personas)/(input$eficiencia/100*3600))
               }
          }
          
          temp <- temp%>%
               spread(FUNCION,TIEMPO, drop = TRUE, fill = 0)
          
          #para que permita eliminar "LINEA" de los proximos reportes, donde no se usa
          datos <- ungroup(temp)
          
          #agrupar en una sola linea
          #cambia los datos de la columna linea todo a 1
          if(input$agrupado){
               datos$LINEA <- "AGRUPADA"
          }
          
          #convertir NAS en cero
          datos[is.na(datos)] <- 0
          
          #dejar solo las funciones que tienen suma de tiempos diferente de cero
          reporte.final.magro <- datos[c(rep(TRUE, 2L), colSums(datos[3L:ncol(datos)]) > 0)]
          
          return(reporte.final.magro)
          
     }
     
     
     #Datos leidos - tabla de estilos y tiempos
     output$tabla_completa <- DT::renderDataTable({
          tabla.raw <- reporte.final(personas.sin = FALSE)
          if(is.null(tabla.raw)) return(NULL)
          
          DT::datatable(tabla.raw, class = 'cell-border stripe' ,
                        filter = 'top', options = list(pageLength = 50),
                        selection = 'multiple', rownames = FALSE)
     })
     
     
     # General - Drop-down de linea a definir pares por producir
     output$lineas.pares <- renderUI({
          num.fam <- reporte.final()
          if(is.null(num.fam)) return(NULL)
          
          if(is.null(input$status.selected)) return(NULL)
          if(is.null(input$plantas.selected)) return(NULL)
          if(is.null(input$lineas.selected)) return(NULL)
          
          
          # num.fam <- num.fam%>%
          #      filter(STATUS %in% input$status.selected &
          #                  PLANTA %in% input$plantas.selected &
          #                  LINEA %in% input$lineas.selected)
          l.linea <- unique(num.fam$LINEA)%>%sort()
          
          selectInput("linea.seleccionada", "Unidad", as.list(l.linea))
     })
     
     
     #General - crea la tabla de pares por producir por linea
     observeEvent(input$agregar, {
          #se presiona sin datos
          if (input$pares=="") return(NULL)
          #agregar a la tabla los pares
          l.linea <- input$linea.seleccionada
          pares <- as.numeric(input$pares)
          
          temp <- data.frame("LINEA" = l.linea, "PARES" = pares)
          
          #quita la linea que existe (actualizar)
          tb.porproduc <- tb.porproduc%>%filter(LINEA != l.linea)
          tb.porproduc <- rbind(tb.porproduc, temp)%>%
               arrange(LINEA)
          
          #General - imprime tabla de pares por producir
          output$por.producir <- DT::renderDataTable({
               DT::datatable(tb.porproduc, options = list(dom = 't'))
          })
          
          tb.porproduc <<- tb.porproduc
     })
     
     # General - Drop-down de linea a filtrar
     output$seleccion_linea <- renderUI({
          num.fam <- reporte.final(personas.sin = FALSE)
          if(is.null(num.fam)) return(NULL)
          l.linea <- unique(num.fam$LINEA)%>%sort()
          selectInput("dataset", "Filtrar por unidad de produccion", as.list(l.linea))
     })   
     
     #grafico que crece según la cantidad de datos --------------------
     output$grafico.inicial.ui <- renderUI({
          altura <- altura_graficoinicial()
          plotlyOutput("grafico.inicial", height = altura * input$sl.graficos)
     })
     
     altura_graficoinicial <- reactive({
          #leer tabla
          tabla.raw <- reporte.final()
          if(is.null(tabla.raw)) return(NULL)
          
          #quitar la columna linea asignada
          tabla.raw2 <- tabla.raw%>%select(-LINEA)
          
          #crea tabla de 3 columnas ESTILO, PUESTO, TIEMPO
          tabla.ren <- gather(tabla.raw2, "PUESTO","TIEMPO",-1)
          
          pixeles <- length(unique(tabla.ren$PUESTO))*300
          
          return(pixeles)
     })
     
     #Desviaciones - escala independiente por grafico
     free.scale.inicial <- reactive({
          b.scales = "fixed"
          if (input$same.scale.inicial){
               b.scales = "free"
          }
          return(b.scales)
          
     })
     
     #Analisis de asignacion - Grafico por puesto
     output$grafico.inicial <- renderPlotly({
          reporte <- reporte.final(personas.sin = FALSE)
          if(is.null(reporte)) return(NULL)
          
          # pdf(NULL)
          
          #crea tabla de 3 columnas ESTILO, PUESTO, TIEMPO
          fin <- dim(reporte)[2]
          tabla.renglon <- gather(reporte, "PUESTO","TIEMPO",c(3:fin))
          
          unidades <- ifelse(input$personas, "Personas", "Segundos")
          
          tabla.renglon$UNIDAD <- tabla.renglon$LINEA
          
          lab="TIEMPO"
          if (input$personas) lab = "PERSONAS"
          
          #limites de cada grafico
          b.scales.ini <- free.scale.inicial()
          
          # grafico de desviaciones por puesto
          tabla.renglon$ESTILO <- as.character(tabla.renglon$ESTILO)
          tabla.renglon$ESTILOS <- reorder(tabla.renglon$ESTILO, tabla.renglon$UNIDAD)
          tabla.renglon$UNIDAD <- as.factor(tabla.renglon$UNIDAD)
          
          g <- ggplot(data = tabla.renglon, aes(ESTILOS, TIEMPO, colour = UNIDAD,
                                                text = paste(lab,":",TIEMPO))) + 
               geom_point() + 
               facet_grid(PUESTO~., as.table = F, scales = b.scales.ini) +
               xlab("") +
               ylab(ifelse(input$personas,"Personas", "Segundos por par"))  +
               ggtitle(ifelse(input$personas,paste("Personas para producir",input$pares.personas,
                                                   "pares por hora"), 
                              "Segundos para producir un par"))+
               theme(axis.text.x = element_text(angle = 90, hjust = 1, size=6),
                     axis.text.y = element_text(size=6),
                     strip.text.y = element_text(size = 6),
                     legend.title=element_blank())
          #scale_colour_brewer(palette = "Set3")
          g<- ggplotly(g, tooltip = c("colour","x"))
          dev.off()
          g
          
     })
     
     #Desviaciones - escala independiente por grafico
     free.scale.fin <- reactive({
          b.scales = "fixed"
          if (input$same.scale.fin){
               b.scales = "free"
          }
          return(b.scales)
          
     })
     
     
     #Desviaciones - grafico por linea, dependen del combobox cual mostrar
     obtener.criticos <- reactive({
          reporte <- reporte.final(personas.sin = FALSE)
          if(is.null(reporte)) return(NULL)
          
          limites <- input$quant
          
          linea <- input$dataset
          cols.usar <- c(3:(ncol(reporte)))
          f.plot <- reporte%>%filter(LINEA == linea)%>%
               gather("PUESTO","TIEMPO",cols.usar)
          
          
          #10% inferior y superior
          quant <- f.plot%>%
               group_by(PUESTO)%>%
               summarise("Promedio" = mean(TIEMPO),
                         "Q1" = quantile(TIEMPO, probs = limites/100), 
                         "Q4" = quantile(TIEMPO, probs = 1-(limites/100)))
          
          temp <- merge(f.plot, quant, by = "PUESTO")%>%
               mutate("Distancia" = sqrt((Promedio-TIEMPO)^2))
          
          #distancia maxima, tomando en cuanta todas las fracciones (critico)     
          critico <- temp%>%
               group_by(ESTILO)%>%
               summarise("Distancia" = sum(Distancia))%>%
               mutate("Q1" = quantile(Distancia, probs = limites/100), 
                      "Q4" = quantile(Distancia, probs = 1-(limites/100)),
                      "CRITICO" = ifelse(Distancia > Q4, "CRITICO", "NORMAL"))%>%
               select(ESTILO, CRITICO)
          
          for.plot <- merge(temp, critico, by = "ESTILO")%>%
               mutate("DESVIACION" = ifelse(CRITICO == "CRITICO", "CRITICO",
                                            ifelse(TIEMPO < Q1 | TIEMPO > Q4,"FUERA","NORMAL")))
     })
     
     #Desviaciones - Imprimir los estilos criticos segun el slider de porcentaje en los limites
     output$cancelar.criticos  <- renderTable({
          for.plot <- obtener.criticos()
          
          if (is.null(for.plot)) return(NULL)
          
          temp <- for.plot%>%
               filter(DESVIACION == "CRITICO")%>%
               select(ESTILO, "UNIDAD" =  LINEA)
          
          criticos <- unique(temp) 
          
          return(criticos)
          
     })
     
     #Desviaciones - Grafico por linea, dependen del combobox cual mostrar
     output$plot.por.linea <- renderPlotly({
          for.plot <- obtener.criticos()
          
          if (is.null(for.plot)) return(NULL)
          pdf(NULL)
          
          b.scales <- free.scale.fin()
          
          #cuando se cambian tiempos por personas
          unidades <- ifelse(input$personas, "Personas", "Segundos")
          
          lab="TIEMPO"
          if (input$personas) lab = "PERSONAS"
          
          p <- ggplot(for.plot, aes(ESTILO,TIEMPO, colour = DESVIACION,
                                    text = paste(lab,":",TIEMPO))) +
               geom_point() +
               facet_wrap(~PUESTO, ncol=2, strip.position = "right" ,scales = b.scales, as.table = T) +
               theme(strip.background = element_blank(), strip.placement = "outside") + 
               geom_hline(data = for.plot%>%
                               group_by(PUESTO)%>%
                               summarise("Promedio.real" = ceiling(mean(TIEMPO))),
                          aes(yintercept = Promedio.real,
                              text = paste(lab,":",Promedio.real)), col = "navy", lwd = 0.5) +
               ylab(unidades) + 
               ggtitle("Estilos a producir vs tiempo total de proceso por funcion") + 
               theme(axis.text.x = element_text(angle = 90, hjust = 1, size=6))
          g <- ggplotly(p, tooltip = c("x", "text"))         
          dev.off()
          g
     })
     
     #Desviaciones - Imprimir desviaciones por linea
     output$total.fam <- renderTable({
          reporte <- reporte.final(personas.sin = FALSE)
          if(is.null(reporte)) {
               return(NULL)
          } else {
               totales <- reporte%>%
                    group_by(LINEA)%>%
                    summarise("Estilos por unidad"= n())
          }
     })
     
     #Desviaciones - indicador general de desviacion (imprimir)
     output$indicador.desviacion <- renderTable({
          tabla <- fun.indicador.desviacion()
          if (is.null(tabla)) return(NULL)
          
          colnames(tabla)[2] <- ifelse(input$personas, "Personas", "Tiempo")
          
          return(tabla)
     })
     
     #Desviaciones - calcula tabla de desviaciones
     fun.indicador.desviacion <- reactive({
          reporte <- reporte.final(personas.sin = FALSE)
          if(is.null(reporte)) return(NULL)
          
          #indicador de desviacion (promedio/(max-min))
          #crea tabla de 3 columnas ESTILO, PUESTO, TIEMPO
          fin <- dim(reporte)[2]
          tabla.renglon <- gather(reporte, "PUESTO","TIEMPO",c(3:fin))
          
          indicador <- tabla.renglon%>%
               group_by(LINEA, PUESTO)%>%
               summarise("Prom" = ceiling(mean(TIEMPO)),
                         "Mini" = min(TIEMPO) ,
                         "Maxi" = max(TIEMPO))%>%
               group_by(LINEA)%>%
               summarise("Promedio" = sum(Prom),
                         "Minimo" = sum(Mini),
                         "Maximo" = sum(Maxi),
                         "Porcentaje desv." = round((Maximo-Minimo)/Promedio*100,2))
          return(indicador)
          
     })
     
     #Desviaciones - Imprimir valor de porcentaje de mejora
     output$pct.mejora <- renderPrint({
          mejora <- desviacion.mejorada()
          if(is.null(mejora)) return(NULL)
          
          anterior <- fun.indicador.desviacion()
          if(is.null(anterior)) return(NULL)
          
          ant <- anterior%>%
               filter(LINEA == input$dataset)
          
          cat(as.numeric(100-round(mejora$NvaDesviacion/ant[5]*100,2)))
          
     })
     
     #Desviaciones - Imprimir valor de desviacion mejorada
     output$nva.desviacion <- renderPrint({
          mejora <- desviacion.mejorada()
          if(is.null(mejora)) return(NULL)
          
          cat(as.numeric(mejora$NvaDesviacion))
     })
     
     #Desviaciones - Imprimir dato de aumento de produccion
     output$incr.produccion <- renderPrint({
          mejora <- desviacion.mejorada()
          if(is.null(mejora)) return(NULL)
          
          anterior <- fun.indicador.desviacion()
          if(is.null(anterior)) return(NULL)
          
          incr <- round((1-(mejora$Maximo/anterior%>%
                                 filter(LINEA == input$dataset)%>%
                                 select(Maximo)))*100,2)
          
          cat(as.numeric(incr))
     })
     
     
     #Desviaciones - indicador general de desviacion sin criticos
     desviacion.mejorada <- reactive({
          
          reporte <- obtener.criticos()
          if(is.null(reporte)) return(NULL)
          
          mejora <- reporte%>%
               filter(CRITICO != "CRITICO")%>%
               select(ESTILO, LINEA, PUESTO, TIEMPO)%>%
               group_by(LINEA, PUESTO)%>%
               summarise("Prom" = ceiling(mean(TIEMPO)),
                         "Mini" = min(TIEMPO) ,
                         "Maxi" = max(TIEMPO))%>%
               group_by(LINEA)%>%
               summarise("Promedio" = sum(Prom),
                         "Minimo" = sum(Mini),
                         "Maximo" = sum(Maxi),
                         "NvaDesviacion" = round((Maximo-Minimo)/Promedio*100,2))
          return(mejora)
     })
     
     #Desviaciones - tabla de desviaciones por linea
     output$desviaciones <- DT::renderDataTable({
          reporte <- reporte.final(personas.sin = FALSE)
          if(is.null(reporte)) return(NULL)
          
          #crea tabla de 3 columnas ESTILO, PUESTO, TIEMPO
          fin <- dim(reporte)[2]
          tabla.renglon <- gather(reporte, "PUESTO","TIEMPO",c(3:fin))
          
          desviaciones <- tabla.renglon%>%
               group_by(LINEA, PUESTO)%>%
               summarise("Promedio" = ceiling(mean(TIEMPO)),
                         "Desviacion" = round(sd(TIEMPO),2),
                         "Minimo" = min(TIEMPO) ,
                         "Maximo" = max(TIEMPO))
          DT::datatable(desviaciones, options = list(pageLength = 50))
          
     })
     
     #Flujo continuo -  Drop-down linea para revisar flujo
     output$flujo.linea <- renderUI({
          datos <- lectura.inicial()
          if(is.null(datos)) return(NULL)
          if(is.null(input$status.selected)) return(NULL)
          if(is.null(input$plantas.selected)) return(NULL)
          if(is.null(input$lineas.selected)) return(NULL)
          
          #filtrar lineas de los status y las plantas filtradas, si no, no existirá plantilla
          #de personal y hará calculos innecesarios, lento
          
          datos.fin <- datos%>%
               filter(STATUS %in% input$status.selected,
                      PLANTA %in% input$plantas.selected, 
                      LINEA %in% input$lineas.selected)
          
          lineas <- unique(datos.fin$LINEA)%>%sort()
          selectInput("cb.lineas.flujo", "Selecciona una unidad", as.list(lineas),
                      multiple = F)
     })     
     
     #Flujo continuo -  Drop-down de departamentos, para seleccionar para flujos
     output$flujo.deptos <- renderUI({
          datos <- lectura.inicial()
          if(is.null(datos)) return(NULL)
          if (is.null(input$cb.lineas.flujo)) return(NULL)
          
          datos.fin <- datos%>%
               filter(STATUS %in% input$status.selected,
                      PLANTA %in% input$plantas.selected, 
                      LINEA %in% input$cb.lineas.flujo)
          
          un.deptos <- unique(datos.fin$DEPTO)%>%sort()
          selectInput("cb.deptos.flujo", "Selecciona departamentos para revisar", as.list(un.deptos),
                      multiple = T)
     })
     
     #Flujo continuo -  Drop-down de estilos para flujo
     output$flujo.estilo <- renderUI({
          datos <- lectura.inicial()
          if(is.null(datos)) return(NULL)
          if (is.null(input$cb.lineas.flujo)) return(NULL)
          
          datos.fin <- datos%>%
               filter(STATUS %in% input$status.selected,
                      PLANTA %in% input$plantas.selected, 
                      LINEA %in% input$cb.lineas.flujo)
          
          estilos <- unique(datos.fin$ESTILO)%>%sort()
          selectInput("cb.estilos.flujo", "Selecciona un estilo", as.list(estilos),
                      multiple = T)
     })
     
     #Flujo continuo - calcula plantilla, completa las funciones con cero (basado en reporte final)
     reporte.flujo <- reactive({
          datos <- lectura.inicial()
          #que se haya leido un archivo
          if (is.null(datos)) return(NULL)
          
          #escoge uno o mas deptos y uno o mas estilos (una sola linea)
          if (is.null(input$cb.deptos.flujo)) return(NULL)
          if (is.null(input$cb.estilos.flujo)) return(NULL)
          
          #convertir NAS en cero
          datos[is.na(datos)] <- 0
          
          #convertir los tiempos en personas para 1000 pares al dia
          efic <- input$eficiencia/100
          hrs <- input$horas.trabajo
          
          #si se tiene el dato de demanda, lo hace con la demanda
          if (nrow(tb.porproduc%>%
                   filter(LINEA %in% input$cb.lineas.flujo))==0){
               datos$PARES <- 1000
               temp <- datos
          } else {
               temp <- merge(datos, tb.porproduc, by = "LINEA")
          }
          
          temp$PERSONAS <- ceiling(temp$TIEMPO*60*temp$PARES/(efic*hrs*3600))
          
          datos <- temp
          
          #agrega con cero las funciones, filtra linea y familias(si existen)
          temp <- datos%>%
               filter(STATUS %in% input$status.selected & 
                           PLANTA %in% input$plantas.selected &
                           DEPTO %in% input$cb.deptos.flujo &
                           LINEA %in% input$cb.lineas.flujo)%>%
               select(DEPTO, ESTILO, FUNCION, PERSONAS)%>%
               group_by(DEPTO, ESTILO, FUNCION)%>%
               summarise("PERSONAS" = sum(PERSONAS))
          
          #para que permita eliminar "LINEA" de los proximos reportes, donde no se usa
          datos <- ungroup(temp)
          
          #calcular plantillas
          plantilla <- datos%>%
               group_by(DEPTO, FUNCION)%>%
               summarise("PLANTILLA" = ceiling(mean(PERSONAS)))
          
          result <- merge(datos, plantilla, by = c("DEPTO", "FUNCION"))%>%
               select(DEPTO, ESTILO, FUNCION, PERSONAS, PLANTILLA)
     })  
     
     
     #Flujo continuo - metas y cumplimiento por depto
     full.flujo <- reactive({
          
          datos <- reporte.flujo()
          if(is.null(datos)) return(NULL)
          
          estilos = input$cb.estilos.flujo
          #cuantos estilos seleccionados
          cuantos <- length(input$cb.estilos.flujo)
          
          if (cuantos == 0) return(NULL)
          if (length(input$cb.lineas.flujo) == 0) return(NULL)
          
          #solo los estilos seleccionados
          datos <- datos%>%
               filter(ESTILO %in% estilos)
          
          #si no hay pares por producir por linea utilizar 100 pares por hora
          #if(nrow(tb.porproduc)==0) return(NULL)
          
          #acumular meta de los estilos seleccionados si es mas de uno
          if (cuantos > 1){
               tabla.plot <- datos%>%
                    group_by(DEPTO, FUNCION)%>%
                    summarise("PERSONAS" = ceiling(sum(PERSONAS)/cuantos),
                              "PLANTILLA" = min(sum(PLANTILLA)/cuantos),
                              "Pct.meta" = ifelse(PERSONAS == 0, 
                                                  ceiling(300), 
                                                  ceiling((PLANTILLA/(PERSONAS))*100)))%>%
                    mutate("ESTILO" = "AGRUPADO")
          } else {
               
               tabla.plot <- datos%>%
                    mutate("Pct.meta" = ifelse(PERSONAS == 0, 
                                               300, 
                                               ceiling((PLANTILLA/PERSONAS)*100)))
          }
          
          plot.final <- tabla.plot%>%
               arrange(DEPTO, FUNCION, Pct.meta)%>%
               mutate("DEPTOFUNC" = paste(DEPTO,"/",FUNCION),
                      "EFIC" = round(min(Pct.meta)/Pct.meta*100,0),
                      "DIF" = PERSONAS - PLANTILLA)
          
          return(plot.final)
          
     })
     
     #Flujo continuo - tabla del grafico (pendiente si presentar o no)
     output$tabla.plot <- DT::renderDataTable({
          plot.final <- full.flujo()
          
          if (is.null(plot.final))  return(NULL)
          
          plot.final <- plot.final%>%
               select(ESTILO, DEPTO, "PUESTO" = FUNCION, PERSONAS, PLANTILLA, 
                      "PORCENTAJE META" = Pct.meta, "PORCENTAJE EFICIENCIA" = EFIC)
          
          DT::datatable(plot.final, options = list(pageLength = 25))
     })
     
     #Flujo continuo - tabla completa de flujo (pendiente si presentar o no)
     output$tabla.full <- DT::renderDataTable({
          plot.final <- reporte.flujo()
          
          if (is.null(plot.final))  return(NULL)
          DT::datatable(plot.final, options = list(pageLength = 25))
     })
     
     #Flujo continuo - tabla completa de flujo balanceado (pendiente si presentar o no)
     output$balanceo <- DT::renderDataTable({
          balanceado <- balanceo()
          if (is.null(balanceado)) return(NULL)
          
          balanceado <- balanceado%>%
               select(ESTILO, DEPTO, FUNCION, PERSONAS, PLANTILLA, 
                      "PORCENTAJE META" = Pct.meta, 
                      "PORCENTAJE EFICIENCIA" = EFIC)
          
          DT::datatable(balanceado, options = list(pageLength = 25))
     })
     
     #Flujo continuo - realiza el balanceo para mejorar metas y eficiencia
     balanceo <- reactive({
          
          plot.final <- full.flujo()
          if (is.null(plot.final))  return(NULL)
          #cuantos estilos seleccionados
          if (length(input$cb.estilos.flujo) == 0) return(NULL)
          if (length(input$cb.lineas.flujo) == 0) return(NULL)
          
          
          cumplimiento = min(plot.final$Pct.meta)
          eficiencia = mean(plot.final$EFIC)
          if (is.infinite(cumplimiento)) return(NULL)
          
          #buscar cumplimineto y eficiencia mayor al 90 o 10 iteraciones
          iter = 0
          
          movimientos <- data.frame("Origen" = numeric(0),"Destino" = numeric(0))
          
          while((cumplimiento < input$cumple.min | eficiencia < .9) & iter < input$cambios.max){
               dona <- min(plot.final$DIF)
               recibe <- min(plot.final$Pct.meta)
               
               movimientos <- c(movimientos, data.frame("Origen" = dona, "Destino" = recibe))
               #movimientos <<- c(movimientos, data.frame("Origen" = dona, "Destino" = recibe))
               
               #pueden haber 2 que donan y 2 que reciben, se elige el primero
               funcionrecibe <- head(plot.final[plot.final$Pct.meta == recibe,]$FUNCION,1)
               funciondona <- head(plot.final[plot.final$DIF == dona,]$FUNCION,1)
               
               #if (dona >= 0) break
               
               #en el minimo, quien recibe +1, quien dona -1
               nva.plantilla.mas <- plot.final[plot.final$FUNCION == funcionrecibe,]$PLANTILLA + 1
               nva.plantilla.menos <- plot.final[plot.final$FUNCION == funciondona,]$PLANTILLA - 1
               
               #cuantas personas quiere el estilo en esos dos, donador y receptor
               personas.mas <- plot.final[plot.final$FUNCION == funcionrecibe,]$PERSONAS
               personas.menos <- plot.final[plot.final$FUNCION == funciondona,]$PERSONAS
               
               #nuevas metas
               nva.meta.mas <- round((nva.plantilla.mas /personas.mas)*100,2)
               nva.meta.menos <- ifelse(personas.menos ==0,300, round((nva.plantilla.menos /personas.menos)*100,2))
               
               #si ahora min meta es mayor (aun cuando le quitamos a una persona, actualiza)
               if(recibe < nva.meta.menos | nva.meta.menos %in% Inf){
                    nva.plantilla.mas -> plot.final[plot.final$FUNCION == funcionrecibe,]$PLANTILLA
                    nva.plantilla.menos -> plot.final[plot.final$FUNCION == funciondona,]$PLANTILLA
                    
                    plot.final <- plot.final%>%
                         mutate("Pct.meta" = ifelse(PERSONAS == 0, 
                                                    300, 
                                                    ceiling((PLANTILLA/PERSONAS)*100)))%>%
                         arrange(DEPTO, FUNCION, Pct.meta)%>%
                         mutate("DEPTOFUNC" = paste(DEPTO,"/",FUNCION),
                                "EFIC" = round(min(Pct.meta)/Pct.meta*100,0),
                                "DIF" = PERSONAS - PLANTILLA)
                    
               } else { break }
               
               #actualiza para el ciclo
               cumplimiento = min(plot.final$Pct.meta)
               eficiencia = mean(plot.final$EFIC)
               
               dona <- min(plot.final$DIF)
               recibe <- min(plot.final$Pct.meta)
               
               iter=iter+1
          }
          
          return(plot.final)
     })
     
     
     #Flujo continuo - Imprime cumplimiento de meta inicial
     output$cumpl.meta <- renderPrint({
          plot.final <- full.flujo()
          
          if (is.null(plot.final))  return(NULL)
          
          cat(paste(ceiling(min(plot.final$Pct.meta))),"%")
     })
     
     #Flujo continuo - Imprime aumento en pares producidos al balancear
     output$aumento.pares <- renderPrint({
          inicial <- full.flujo()
          balanceado <- balanceo()
          
          if (is.null(inicial))  return(NULL)
          if (is.null(balanceado))  return(NULL)
          
          mas.pares <- (((min(balanceado$Pct.meta))/ceiling(min(inicial$Pct.meta)))-1)*100*input$horas.trabajo*5
          
          cat(format(ceiling(mas.pares), decimal.mark=".",big.mark=",", small.mark=",", small.interval=3))
     })
     
     #Flujo continuo - Imprime aumento en facturacion al balancear
     output$aumento.facturacion <- renderPrint({
          inicial <- full.flujo()
          balanceado <- balanceo()
          
          if (is.null(inicial))  return(NULL)
          if (is.null(balanceado))  return(NULL)
          
          mas.pares <- (((min(balanceado$Pct.meta))/ceiling(min(inicial$Pct.meta)))-1)*100*input$horas.trabajo*5
          
          cat(format(ceiling(mas.pares)*input$precio.prom, decimal.mark=".",big.mark=",", small.mark=",", small.interval=3))
     })
     
     #Flujo continuo - Imprime cumplimiento mejorado al balancear
     output$cumpl.mejorado <- renderPrint({
          plot.final <- balanceo()
          
          if (is.null(plot.final))  return(NULL)
          
          cat(paste(ceiling(min(plot.final$Pct.meta))),"%")
     })
     
     #Flujo continuo - Imprime eficiencia esperada inicial
     output$ef.esperada <- renderPrint({
          plot.final <- full.flujo()
          
          if (is.null(plot.final))  return(NULL)
          
          cat(paste(ceiling(mean(plot.final$EFIC)),"%"))
     })
     
     #Flujo continuo - Imprime eficiencia mejorada con balanceo
     output$ef.mejorada <- renderPrint({
          plot.final <- balanceo()
          
          if (is.null(plot.final))  return(NULL)
          
          cat(paste(ceiling(mean(plot.final$EFIC)),"%"))
     })
     
     #Flujo continuo - tabla de movimientos requeridos con balanceo
     output$tabla.movimientos <- renderTable({
          datos.normal <- full.flujo()
          if (is.null(datos.normal))  return(NULL)
          datos.normal <- datos.normal%>%mutate("Datos" = "Base")
          datos.normal$PLANTILLA <- datos.normal$PLANTILLA * -1
          
          balanceado <- balanceo()
          balanceado <- balanceado%>%mutate("Datos" = "Balanceado")
          
          
          #junta las tablas
          result <- rbind(datos.normal, balanceado)%>%
               group_by(FUNCION)%>%
               summarise("CANTIDAD" = sum(PLANTILLA))%>%
               filter(CANTIDAD != 0)
          
          return(result)
          
     })
     
     #Flujo continuo - Observa cuando se seleccionan estilos
     observeEvent(input$cb.estilos.flujo, {
          
          #Flujo continuo - Imprime grafico de flujo continuo
          output$plot.flujo <- renderPlotly({
               
               datos.normal <- full.flujo()
               if (is.null(datos.normal))  return(NULL)
               datos.normal <- datos.normal%>%mutate("Datos" = "Base")
               
               balanceado <- balanceo()
               balanceado <- balanceado%>%mutate("Datos" = "Balanceado")
               
               #META BASE
               intercepts <- datos.normal%>%
                    group_by(ESTILO, DEPTO)%>%
                    filter(Pct.meta > 0)%>%
                    summarise("Meta.real" = min(Pct.meta))
               
               #META BALANCEO
               intercepts.b <- balanceado%>%
                    group_by(ESTILO, DEPTO)%>%
                    filter(Pct.meta > 0)%>%
                    summarise("Meta.real" = min(Pct.meta))
               
               para.plot <- rbind(datos.normal, balanceado)
               
               g <- ggplot(para.plot, aes(DEPTOFUNC, Pct.meta, colour = Datos, group = Datos)) + 
                    geom_point(size = 2) + geom_line() + 
                    scale_x_discrete(labels = substr(para.plot$FUNCION,1,5)) +
                    geom_hline(data = intercepts, aes(yintercept =  Meta.real, colour = DEPTO))+
                    geom_hline(data = intercepts.b, aes(yintercept =  Meta.real, colour = DEPTO))+
                    expand_limits(y = c(0,100)) + 
                    ggtitle("Porcentaje de cumplimiento de meta por funcion") + 
                    xlab("FUNCIONES") + 
                    ylab("CUMPLIMIENTO DE META")
               dev.off()
               g
          })
          
     })
     
     
     #General - Oberva boton agregar y actualiza todo lo referente a analisis de personal
     #cuando se carga un meta de pares por linea
     actualiza.pares <- reactive({
          list(input$archivo.sim, input$agregar)
     })
     
     observeEvent(actualiza.pares(), {
          
          #Analisis de desviaciones - incremento en facturacion quitando criticos
          output$inc.facturacion <- renderPrint({
               mejora <- desviacion.mejorada()
               if(is.null(mejora)) return(NULL)
               
               #si no hay pares por producir por linea no hace el calculo de personas
               if(nrow(tb.porproduc)==0) return(NULL)               
               
               anterior <- fun.indicador.desviacion()
               if(is.null(anterior)) return(NULL)
               
               incr <- round((1-(mejora$Maximo/anterior%>%
                                      filter(LINEA == input$dataset)%>%
                                      select(Maximo)))*100,2)
               
               l.actual <- input$dataset
               par.fam <- tb.porproduc%>%
                    filter(LINEA == l.actual)%>%
                    select(PARES)
               
               if (nrow(par.fam)==0) return(cat("Sin pares por producir"))
               
               mas.fact <- as.numeric(ceiling((par.fam * (incr/100) * as.numeric(input$precio.prom))*5))
               
               cat(format(mas.fact, decimal.mark=".",big.mark=",", small.mark=",", small.interval=3))
          })
          
          #Analisis de personal - genera plantilla basica por linea, puesto, base de las demas
          calcular.plantilla <- reactive({
               temp <- reporte.final()
               if(is.null(temp)) return(NULL)
               #si no hay pares por producir por linea no hace el calculo
               if(nrow(tb.porproduc)==0) return(NULL)
               
               fin <- dim(temp)[2]
               efic <- input$eficiencia/100
               hrs <- input$horas.trabajo
               sds <- input$sds
               
               #prueba sin redondear personas
               tabla.renglon <- gather(temp, "PUESTO","TIEMPO",c(3:fin))%>%
                    merge(tb.porproduc, by = "LINEA")%>%
                    mutate("PERSONAS" = ceiling(TIEMPO*PARES/(efic*hrs*3600)))%>%
                    group_by(LINEA, PUESTO)%>%
                    summarise("TIEMPO" = ceiling(mean(TIEMPO)),
                              "PARES" = min(PARES),
                              "PERSONAS" = ifelse(n()==1,PERSONAS,
                                                  ifelse(input$redondeo, 
                                                         ceiling(mean(PERSONAS+(sds*sd(PERSONAS)))),
                                                         round(mean(PERSONAS+(sds*sd(PERSONAS))),2))))
               return(tabla.renglon)
          })
          
          #Analisis de personal - Tabla por linea-puesto
          output$PersonalPorlinea <- DT::renderDataTable({
               tabla.renglon <- calcular.plantilla()
               if (is.null(tabla.renglon)) return(NULL)
               
               DT::datatable(tabla.renglon, class = 'cell-border stripe' ,
                             options = list(pageLength = 50),
                             selection = 'multiple', rownames = FALSE)
               
          })
          
          
          #Analisis de personal - plantilla basica, Calcula restriccion y eficiencia por funcion
          eficiencia.funcion <- reactive({
               temp <- reporte.final()
               if(is.null(temp)) return(NULL)
               
               #si no hay pares por producir por linea no hace el calculo
               if(nrow(tb.porproduc)==0) return(NULL)
               
               fin <- dim(temp)[2]
               efic <- input$eficiencia/100
               hrs <- input$horas.trabajo
               sds <- input$sds
               
               #primero convierte a personas y luego redondea
               plantilla <- gather(temp, "PUESTO","TIEMPO",c(3:fin))%>%
                    merge(tb.porproduc, by = "LINEA")%>%
                    mutate("PERSONAS" = ceiling(TIEMPO*PARES/(efic*hrs*3600)))%>%
                    group_by(LINEA, PUESTO)%>%
                    summarise("TIEMPO" = ceiling(mean(TIEMPO)),
                              "PARES" = min(PARES),
                              "PLANTILLA" = ifelse(n()==1,PERSONAS,ceiling(mean(PERSONAS+(sds*sd(PERSONAS))))))%>%
                    select(LINEA, PUESTO, PARES, PLANTILLA)
               
               
               
               tabla.renglon <- gather(temp, "PUESTO","TIEMPO",c(3:fin))%>%
                    merge(plantilla, by = c("LINEA","PUESTO"))%>%
                    mutate("PERSONAS" = ceiling(TIEMPO*PARES/(efic*hrs*3600)),
                           "META" = ceiling(ifelse(PERSONAS==0,100,PLANTILLA/PERSONAS*100)),
                           "PARES.PRODUCCION" = ceiling(PARES*META/100),
                           "CAPACIDAD.FUNCION" = ifelse(PARES<PARES.PRODUCCION, PARES, PARES.PRODUCCION))
               
               meta.estilo <- tabla.renglon%>%
                    group_by(ESTILO)%>%
                    summarise("PROD.RESTRICCION" = min(CAPACIDAD.FUNCION))
               
               result <- merge(tabla.renglon, meta.estilo, by = "ESTILO")%>%
                    mutate("EFICIENCIA" =ceiling(ifelse(PERSONAS==0,0, PROD.RESTRICCION/PARES.PRODUCCION*100)),
                           "APROVECHAMIENTO" = round(ifelse(PERSONAS==0,0, EFICIENCIA/100*PLANTILLA),2)) 
               
               
               return(result)
          })
          
          
          #Analisis de personal - Personal por linea con eficiencias
          output$PersonalPorEstilo <- DT::renderDataTable({
               result <- eficiencia.funcion()
               if (is.null(result)) return(NULL)
               
               DT::datatable(result, class = 'cell-border stripe' ,
                             filter = 'top', options = list(pageLength = 50),
                             selection = 'multiple', rownames = FALSE)
               
          })
          
          #Analisis de personal - eficiencia por estilo (para graficar)
          output$eficiencia.estilo <- renderPlotly({
               pdf(NULL)
               
               temp <- eficiencia.funcion()
               if(is.null(temp)) return(NULL)
               
               #grafico de eficiencia
               eficiencia.por.estilo <- temp%>%
                    group_by(ESTILO)%>%
                    summarise("DISPONIBLE" = sum(PLANTILLA),
                              "UTILIZADO" = sum(APROVECHAMIENTO),
                              "PROD.RESTRICCION" = ceiling(mean(PROD.RESTRICCION)),
                              "PAR.PRESUP" = min(PARES))%>%
                    mutate("EFICIENCIA" = ceiling(UTILIZADO/DISPONIBLE*100),
                           "META" = ceiling(PROD.RESTRICCION/PAR.PRESUP*100))%>%
                    select(ESTILO, EFICIENCIA, META)%>%
                    gather("DATO","PORCENTAJE", 2:3)%>%
                    mutate("NIVEL" = ifelse(DATO=="EFICIENCIA",
                                            ifelse(PORCENTAJE > 85, "A-BUENO", 
                                                   ifelse(PORCENTAJE > 50, "B-REGULAR","C-CRITICO")),
                                            ifelse(PORCENTAJE >= 95, "A-BUENO", 
                                                   ifelse(PORCENTAJE >= 90, "B-REGULAR","C-CRITICO"))))
               
               paleta <- c("darkgreen","gold2","red")
               
               g <- ggplotly(
                    ggplot(eficiencia.por.estilo, aes(ESTILO, PORCENTAJE, colour = NIVEL, 
                                                      shape = DATO), alpha = 0.5) + 
                         geom_point() + 
                         scale_color_manual(values = paleta) + 
                         expand_limits(y=c(0,100)) + 
                         ylab("EFICIENCIA/ CUMPLIMIENTO DE METAS") +
                         xlab("") +
                         theme(axis.text.x = element_text(angle = 90, hjust = 1, size=6))
                    
               )
               dev.off()
               g
               
          })
          
          output$estilos.con.cumplimiento <- renderPrint({
               temp <- eficiencia.funcion()
               if(is.null(temp)) return(NULL)
               
               #grafico de eficiencia
               eficiencia.por.estilo <- temp%>%
                    group_by(ESTILO)%>%
                    summarise("DISPONIBLE" = sum(PLANTILLA),
                              "UTILIZADO" = sum(APROVECHAMIENTO),
                              "PROD.RESTRICCION" = ceiling(mean(PROD.RESTRICCION)),
                              "PAR.PRESUP" = min(PARES))%>%
                    mutate("EFICIENCIA" = ceiling(UTILIZADO/DISPONIBLE*100),
                           "META" = ceiling(PROD.RESTRICCION/PAR.PRESUP*100))%>%
                    select(ESTILO, EFICIENCIA, META)%>%
                    gather("DATO","PORCENTAJE", 2:3)%>%
                    mutate("NIVEL" = ifelse(DATO=="EFICIENCIA",
                                            ifelse(PORCENTAJE > 85, "A-BUENO", 
                                                   ifelse(PORCENTAJE > 50, "B-REGULAR","C-CRITICO")),
                                            ifelse(PORCENTAJE >= 95, "A-BUENO", 
                                                   ifelse(PORCENTAJE >= 90, "B-REGULAR","C-CRITICO"))))
               
               result <- nrow(eficiencia.por.estilo%>%filter(DATO == "META" & PORCENTAJE >= 95))
               total <- nrow(eficiencia.por.estilo)/2
               final <- paste(result,"de",total,"-",floor(result/total*100),"%")
               cat(final)
               
          })
          
          #Analisis de personal - eficiencia por estilo (para graficar)
          output$eficiencia.linea <- renderTable({
               temp <- eficiencia.funcion()
               if(is.null(temp)) return(NULL)
               
               eficiencia.por.linea <- temp%>%
                    group_by(LINEA, ESTILO)%>%
                    summarise("DISPONIBLE" = sum(PLANTILLA),
                              "UTILIZADO" = sum(APROVECHAMIENTO))%>%
                    mutate("TEMP" = ceiling(UTILIZADO/DISPONIBLE*100))%>%
                    group_by(LINEA)%>%
                    summarise("EFIC. PROMEDIO(%)" = ceiling(mean(TEMP)),
                              "EFIC. MEDIANA(%)" = ceiling(quantile(TEMP, probs = 0.5)),
                              "DESV EFIC.(%)" = round(sd(TEMP),2))
               
          })
          
          #Analisis de personal - META por estilo (para graficar)
          output$meta.linea <- renderTable({
               temp <- eficiencia.funcion()
               if(is.null(temp)) return(NULL)
               
               meta.por.linea <- temp%>%
                    group_by(LINEA)%>%
                    summarise("PROMEDIO" = ceiling(mean(PROD.RESTRICCION)),
                              "MEDIANA" = ceiling(quantile(PROD.RESTRICCION, probs = 0.5)),
                              "DESVIACION" = round(sd(PROD.RESTRICCION),2),
                              "PAR.PRESUP" = min(PARES))%>%
                    mutate("CUMPLIMIENTO" = ceiling(PROMEDIO/PAR.PRESUP*100))%>%
                    select("PARES PROMEDIO" = PROMEDIO, "PARES MEDIANA" = MEDIANA,
                           "CUMPLE META(%)" = CUMPLIMIENTO, "DESVIACION PARES" = DESVIACION)
               
          })
          
          
          #Analisis de personal - Imprime personas por funcion
          output$total_puesto <- renderTable({
               tabla.totales <- calcular.plantilla()
               if (is.null(tabla.totales)) return(NULL)
               
               result <- tabla.totales%>%
                    group_by(PUESTO)%>%
                    summarise("PERSONAS" = sum(PERSONAS))
               
          })
          
          #Analisis de personal - Imprime Total por linea de produccion
          output$Totales.por.linea <- renderTable({
               tabla.totales <- calcular.plantilla()
               if (is.null(tabla.totales)) return(NULL)
               
               tabla.renglon <- tabla.totales%>%
                    group_by(LINEA)%>%
                    summarise("PERSONAS" = sum(PERSONAS))
               
               sueldo <- input$sueldo.prom
               
               #calcula produccion promedio real por la eficiencia de balanceo
               efic <- eficiencia.funcion()
               if(is.null(efic)) return(NULL)
               
               prod.prom <- efic%>%
                    group_by(LINEA)%>%
                    summarise("PROD.REAL" = ceiling(mean(PROD.RESTRICCION)))
               
               
               result <- merge(tabla.renglon, prod.prom, by = "LINEA")%>%
                    mutate("COSTO.PAR" = round((sueldo*PERSONAS)/(PROD.REAL*5),2))%>%
                    select(LINEA, PERSONAS, COSTO.PAR)
               
               return(result)
               
          })
          
          #Analisis de personal - costo promedio de mano de obra
          output$mo.promedio <- renderPrint({
               tabla.totales <- calcular.plantilla()
               if (is.null(tabla.totales)) return(NULL)
               
               tabla.renglon <- tabla.totales%>%
                    group_by(LINEA)%>%
                    summarise("PERSONAS" = sum(PERSONAS))
               
               sueldo <- input$sueldo.prom
               
               #calcula produccion promedio real por la eficiencia de balanceo
               efic <- eficiencia.funcion()
               if(is.null(efic)) return(NULL)
               
               prod.prom <- efic%>%
                    group_by(LINEA)%>%
                    summarise("PROD.REAL" = ceiling(mean(PROD.RESTRICCION)))
               
               total.pares <- sum(prod.prom$PROD.REAL)
               
               result <- merge(tabla.renglon, prod.prom, by = "LINEA")%>%
                    mutate("COSTO.PAR" = round((sueldo*PERSONAS)/(PROD.REAL*5),2),
                           "PRECIO.POND" = COSTO.PAR*PROD.REAL)
               
               
               cat(round(sum(result$PRECIO.POND)/total.pares,2))
               
          })
          
          #Analisis de personal - incremento en la facturacion por plantilla basica correcta
          output$incr.fact.plantilla <- renderPrint({
               tabla.totales <- calcular.plantilla()
               if (is.null(tabla.totales)) return(NULL)
               
               tabla.renglon <- tabla.totales%>%
                    group_by(LINEA)%>%
                    summarise("PERSONAS" = sum(PERSONAS))
               
               
               #calcula produccion promedio real por la eficiencia de balanceo
               efic <- eficiencia.funcion()
               if(is.null(efic)) return(NULL)
               
               prod.prom <- efic%>%
                    group_by(LINEA)%>%
                    summarise("PROD.REAL" = ceiling(mean(PROD.RESTRICCION)))
               
               total.pares <- sum(prod.prom$PROD.REAL)
               
               cat(format(total.pares*5*input$precio.prom, decimal.mark=".",big.mark=",", small.mark=",", small.interval=3))
               
          })
          #Analisis de personal - facturacion menos costo de mo
          output$fact.mo <- renderPrint({
               tabla.totales <- calcular.plantilla()
               if (is.null(tabla.totales)) return(NULL)
               
               tabla.renglon <- tabla.totales%>%
                    group_by(LINEA)%>%
                    summarise("PERSONAS" = sum(PERSONAS))
               
               costo.mo <- sum(tabla.renglon$PERSONAS)*input$sueldo.prom
               
               #calcula produccion promedio real por la eficiencia de balanceo
               efic <- eficiencia.funcion()
               if(is.null(efic)) return(NULL)
               
               prod.prom <- efic%>%
                    group_by(LINEA)%>%
                    summarise("PROD.REAL" = ceiling(mean(PROD.RESTRICCION)))
               
               total.pares <- sum(prod.prom$PROD.REAL)
               
               cat(format(total.pares*5*input$precio.prom - costo.mo, decimal.mark=".",big.mark=",", small.mark=",", small.interval=3))
               
          })
          
          #Analisis de personal - facturacion menos costo de mo
          output$total.mo <- renderPrint({
               tabla.totales <- calcular.plantilla()
               if (is.null(tabla.totales)) return(NULL)
               
               tabla.renglon <- tabla.totales%>%
                    group_by(LINEA)%>%
                    summarise("PERSONAS" = sum(PERSONAS))
               
               costo.mo <- sum(tabla.renglon$PERSONAS)*input$sueldo.prom
               
               cat(format(costo.mo, decimal.mark=".",big.mark=",", small.mark=",", small.interval=3))
               
          })
          
          
          #analisiS de personal - Gran total de personas en la plantilla
          output$grantotal <- renderPrint({
               tabla.totales <- calcular.plantilla()
               if (is.null(tabla.totales)) return(NULL)
               
               cat(round(sum(tabla.totales$PERSONAS)))
          })
     })
     
})
