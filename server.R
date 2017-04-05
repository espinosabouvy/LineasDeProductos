
library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(cluster)


shinyServer(function(input, output, session) {
     #permite archivos de hasta 15MB
     options(shiny.maxRequestSize=15*1024^2)
     
     datasetInput <- reactive({
          reporte <- reporte.final()
          if(is.null(reporte)) return(NULL)
          
          return(reporte)
     })
     
     output$download <- downloadHandler(
          
          filename = function() { paste("Modelos asignados por linea", '.csv', sep='') },
          content = function(file) {
               write.csv(datasetInput(), file, row.names = F)
          }
     )
     
     #lectura inicial de datos y transformacin al formato requerido de estilo-puesto1-puesto2
     lectura.inicial <- reactive({
          require(dplyr)
          require(tidyr)
          
          inFile <- NULL
          inFile <- input$browse          
          #inFile$datapath <- "tiempos.csv"
          
          if (is.null(inFile)){
               #sin archivo seleccionado
               return(NULL)
          }
          
          #leer por tipo de archivo
          tiempos.raw <- read.csv(inFile$datapath, 
                                  header = TRUE, na.strings = c("NA",""))
          
          #limpiar el formato, actual (LINEA, VCESTIL, PARES, FAMPESP, FAMMONT, DEPTO, FUNCION, TIEMPO,
          #PERSONAS, META)
          deptos.usar <- c("CORTE","CORTE Y PREPARA", "ENSAMBLES", "FAMILIA", "FORRADOS", 
                           "PLANTA", "RAYADO Y RESACA", "SUELA")
          
          tiempos.raw <- tiempos.raw%>%
               select(DEPTO, VCESTIL, FAMPESP, FUNCION, TIEMPO)%>%
               filter(DEPTO %in% deptos.usar)%>%
               filter(TIEMPO > 0)
          
          #sin linea, es linea 0
          tiempos.raw[is.na(tiempos.raw$FAMPESP),]$FAMPESP <- 0
          
          #agrupar pespuntadores y preliminares
          tiempos.raw$FUNCION <- ifelse(grepl("PESPUNTADOR", 
                                              tiempos.raw$FUNCION),"PESPUNTADOR",
                                        paste(tiempos.raw$FUNCION))
          tiempos.raw$FUNCION <- ifelse(grepl("CA-PES", 
                                              tiempos.raw$FUNCION),"PESPUNTADOR",
                                        paste(tiempos.raw$FUNCION))
          #corregir del sistema, mientras, debe desaparecer
          tiempos.raw$FUNCION <- ifelse(grepl("PRECONFORM", 
                                              tiempos.raw$FUNCION),"PRELIMINAR",
                                        paste(tiempos.raw$FUNCION))
          tiempos.raw$FUNCION <- ifelse(grepl("PRELIM", 
                                              tiempos.raw$FUNCION),"PRELIMINAR",
                                        paste(tiempos.raw$FUNCION))
          tiempos.raw$FUNCION <- ifelse(grepl("CA-PRE", 
                                              tiempos.raw$FUNCION),"PRELIMINAR",
                                        paste(tiempos.raw$FUNCION))
          tiempos.raw$FUNCION <- ifelse(grepl("C-PREA", 
                                              tiempos.raw$FUNCION),"PRELIMINAR",
                                        paste(tiempos.raw$FUNCION))
          tiempos.raw$FUNCION <- ifelse(grepl("CA-COR", 
                                              tiempos.raw$FUNCION),"CORTADOR PIEL",
                                        paste(tiempos.raw$FUNCION))
          tiempos.raw$FUNCION <- ifelse(grepl("CORTADOR FLASH", 
                                              tiempos.raw$FUNCION),"CORTADOR PIEL",
                                        paste(tiempos.raw$FUNCION))
          tiempos.raw$FUNCION <- ifelse(grepl("PRECONF", 
                                              tiempos.raw$FUNCION),"PRECONFORMADOR",
                                        paste(tiempos.raw$FUNCION))
          tiempos.raw$FUNCION <- ifelse(grepl("CA-PREC", 
                                              tiempos.raw$FUNCION),"PRECONFORMADOR",
                                        paste(tiempos.raw$FUNCION))
          tiempos.raw$FUNCION <- ifelse(grepl("CA-DOB", 
                                              tiempos.raw$FUNCION),"DOBLILLADOR",
                                        paste(tiempos.raw$FUNCION))
          tiempos.raw$FUNCION <- ifelse(grepl("CA-REB", 
                                              tiempos.raw$FUNCION),"REBAJADOR",
                                        paste(tiempos.raw$FUNCION))
          tiempos.raw$FUNCION <- ifelse(grepl("REBAJADOR PIEL", 
                                              tiempos.raw$FUNCION),"REBAJADOR",
                                        paste(tiempos.raw$FUNCION))
          tiempos.raw[tiempos.raw$FUNCION == "FORRAR" & tiempos.raw$DEPTO == "PLANTA",]$FUNCION <- "RIBETEAR"
          
          #quitar comodines
          tiempos.raw <- tiempos.raw[!grepl("COMODIN+",  tiempos.raw$FUNCION),]
          names(tiempos.raw) <- make.names(names(tiempos.raw))
          tiempos <- tiempos.raw%>%
               select(DEPTO, "ESTILO" = VCESTIL, "LINEA" = FAMPESP, FUNCION, TIEMPO)
          
          #hacer correcciones al archivo y llenar combo para seleccionar depto
          if (is.null(tiempos)){
               #sin archivo seleccionado
               return(NULL)
          }
          
          #llenar combo de departamentos con el archivo preparado
          output$depto.select <- renderUI({
               deptos <- unique(tiempos$DEPTO)
               selectInput("depto.selected", "Selecciona el departamento que quieres analizar", as.list(deptos))
          })
          
          #llenar combo de departamentos con el archivo preparado
          output$lineas.select <- renderUI({
               lineas <- unique(tiempos$LINEA)%>%sort()
               selectInput("lineas.selected", "Filtra los departamentos que quieres analizar", as.list(lineas),
                           multiple = TRUE, selected = lineas)
          })

          return(tiempos)
     })
     

     #genera todos los puestos para todos los estilos del depto con tiempo = 0
     reporte.corregido <- reactive({
          datos <- lectura.inicial()
          
          if (is.null(datos)){
               #sin archivo seleccionado
               return(NULL)
          }

          #completar con cero las funciones que existen en cada depto y el estilo no las tiene
          #primero se debe agregar todos los estilos a todas los deptos
          estilos.fam <- unique(datos[datos$DEPTO == "FAMILIA",2:3])

          #verificar que esa linea tenga ese departamento
          if (nrow(datos[datos$DEPTO== input$depto.selected,]) == 0) return(NULL)
          
          # #convertir tiempos en personas
          if (input$personas){
               datos$TIEMPO <- ceiling(datos$TIEMPO*1000/(.85*9.5*3600))
          }
          
          #agrega con cero las funciones, filtra linea y familias(si existen)
          temp <- datos%>%
               filter(DEPTO %in% input$depto.selected & 
                           LINEA %in% input$lineas.selected)%>%
               select(ESTILO, FUNCION, TIEMPO)%>%
               group_by(ESTILO, FUNCION)%>%
               summarise("TIEMPO" = sum(TIEMPO))%>%
               spread(FUNCION,TIEMPO, drop = FALSE, fill = 0)
          datos <- merge(estilos.fam, temp, by = "ESTILO")


          #convertir NAS en cero
          datos[is.na(datos)]<-0

          #estilo y linea como factor
          datos[,1] <- as.factor(datos[,1])
          datos[,2] <- as.factor(datos[,2])
          #nas a cero
          datos[is.na(datos)]<-0

          #limitar cantidad de modelos
          #datos<- datos[1:20,]


          return(datos)
          
     })  
     
     #Datos leidos - tabla inicial de datos leidos despues de seleccionar departamento a revisar
     output$tabla_completa <- DT::renderDataTable({
          tabla.raw <- reporte.corregido()
          if(is.null(tabla.raw)) return(NULL)
          
          DT::datatable(tabla.raw, class = 'cell-border stripe' ,
                        filter = 'top', options = list(pageLength = 50),
                        selection = 'multiple', rownames = FALSE)
     })

     #Estadistica inicial - TABLA INICIAL DE PROMEDIOS, DESVIACIONES Y RANGOS
     output$tablainicial <- renderTable({
          #leer tabla
          tabla.raw <- reporte.corregido()
          if(is.null(tabla.raw)) return(NULL)
          
          #quitar la columna linea asignada
          tabla.raw <- tabla.raw%>%select(-LINEA)
          
          #crea tabla de 3 columnas ESTILO, PUESTO, TIEMPO
          tabla.ren <- gather(tabla.raw, "PUESTO","TIEMPO",-1)
          
          data.frame(tabla.ren%>%
                          group_by(PUESTO)%>%
                          summarise("Promedio" = ceiling(mean(TIEMPO)),
                                    "Desviacion" = round(sd(TIEMPO),2),
                                    "Minimo" = min(TIEMPO) ,
                                    "Maximo" = max(TIEMPO),
                                    "Porcentaje" = ceiling(Desviacion/Promedio*100)))
     })
     
     #Estadistica inicial - grafico inicial de desviaciones, sin asignacion
     output$graficoinicial <- renderPlotly({
          #escalas libres en grafico
          b.scales = "fixed"
          if (input$same.scale.ini){
               b.scales = "free"
          }

          #leer tabla
          tabla.raw <- reporte.corregido()
          if(is.null(tabla.raw)) return(NULL)
          
          #quitar la columna linea asignada
          tabla.raw <- tabla.raw%>%select(-LINEA)
          
          #crea tabla de 3 columnas ESTILO, PUESTO, TIEMPO
          tabla.ren <- gather(tabla.raw, "PUESTO","TIEMPO",-1)
          
          # grafico de desviaciones por puesto
          ggplotly(
               ggplot(data = tabla.ren, aes(ESTILO, TIEMPO)) + 
                    geom_point(col = "navy", alpha = 0.5) + 
                    facet_grid(PUESTO~., scales = b.scales)+ 
                    geom_hline(data = tabla.ren%>%
                                    group_by(PUESTO)%>%
                                    summarise("Promedio" = mean(TIEMPO)),
                               aes(yintercept = Promedio), 
                               col = "red", lwd = 1) + 
                    xlab("Modelos") +
                    ylab("Segundos por par")  +
                    ggtitle("Tiempo (segundos) para producir un par")
          )
     })
     
     #Estadistica inicial - boxplot de desviaciones del departamento, sin asignacion
     output$boxplotini <- renderPlotly({
          
          #leer tabla
          tabla.raw <- reporte.corregido()
          if(is.null(tabla.raw)) return(NULL)
          
          #quitar la columna linea asignada
          tabla.raw <- tabla.raw%>%select(-LINEA)
          
          #crea tabla de 3 columnas ESTILO, PUESTO, TIEMPO
          tabla.ren <- gather(tabla.raw, "PUESTO","TIEMPO",-1)
          
          # grafico de desviaciones por puesto
          print(
               ggplotly(
                    ggplot(data = tabla.ren, aes(PUESTO, TIEMPO)) + 
                         geom_boxplot(col = "navy") + 
                         xlab("Modelos") +
                         ylab("Segundos por par")  +
                         ggtitle("Tiempo (segundos) para producir un par")
               )
          )
     })
     
     
     #Lineas de produccion - grafica el dendograma
     output$dendograma <- renderPlot({
          tabla.raw <- reporte.corregido()
          if(is.null(tabla.raw)) return(NULL)
          
          arbol <- dendograma()
          if(is.null(arbol)) return(NULL)
          
          g <- plot(arbol, labels = tabla.raw[,1], axes = T, main = "Cluster de agrupamiento", 
                    xlab = "Modelos", ylab = "Indice de desviacion")
          return(g + abline(h = input$altura_cluster, col="red"))
     })
     
     #Lineas de produccion - escribe el nUmero de lineas que se crean en cada altura
     output$lineas <- renderText({
          arbol <- dendograma()
          if(is.null(arbol)) return(NULL)
          max(cutree(arbol, h=input$altura_cluster))
     })
     
     #obtinee los datos de distancias del dendograma y modifica el slider
     dendograma <- reactive({
          
          #leer tabla
          tabla.raw <- reporte.corregido()
          if(is.null(tabla.raw)) return(NULL)
          
          #quitar la columna linea asignada
          tabla.raw <- tabla.raw%>%select(-LINEA)
          
          set.seed(8)
          distancias <- dist(tabla.raw[,-1], method = "euclidian")
          clust <- hclust(distancias)
          maxx <- ceiling(max(clust$height))
          updateSliderInput(session, "altura_cluster", max = maxx)
          updateSliderInput(session, "altura_cluster", value = maxx*0.9)
          updateSliderInput(session, "altura_cluster", step = ceiling(maxx/100))
          return(clust)
     })
     
     
     #Lineas de produccion - calcula las asignaciones por kmeans de acuerdo con input de familias deseadas
     reporte.final <- reactive({
          
          #HASTA QUE NO SE SELECCIONE UN ARCHIVO
          tabla.raw <- reporte.corregido()
          if(is.null(tabla.raw)) return(NULL)
          
          #quitar la columna linea asignada
          tabla.raw <- tabla.raw%>%select(-LINEA)
          
          arbol <- dendograma()
          if(is.null(arbol)) return(NULL)
          nolineas <- max(cutree(arbol, h=input$altura_cluster))
          
          set.seed(88)
          gr <- tabla.raw[c(2:dim(tabla.raw)[2])]
          k <- kmeans(gr, centers = nolineas ,nstart = 10, iter.max = 100)
          agrupacion.final <- cbind(tabla.raw, "LINEA" = k$cluster)
          reporte <- agrupacion.final%>%
               select(LINEA, ESTILO)%>%
               arrange(LINEA)
          
          #agregar cluster a tabla por renglon
          merge(tabla.raw, reporte, by = "ESTILO")
          
          #debe agregar la linea anterior... o puede no usarla
          
     })

     #TABLA DE ESTILOS ASIGNADOS
     output$tabla_asignacion <- DT::renderDataTable({
          reporte <- reporte.final()
          if(is.null(reporte.final)) return(NULL)
          
          DT::datatable(reporte, options = list(pageLength = 25))
     })
     
     # Drop-down de linea a filtrar despues de definir las lineas de produccion
     output$seleccion_linea <- renderUI({
          num.fam <- reporte.final()
          if(is.null(num.fam)) return(NULL)
          l.linea <- unique(num.fam$LINEA)%>%sort()
          selectInput("dataset", "Filtrar por linea de produccion", as.list(l.linea))
     })
     

     
     #grafico por linea, dependen del combobox cual mostrar
     output$plot.por.linea <- renderPlotly({
          reporte <- reporte.final()
          if(is.null(reporte)) return(NULL)
          
          #escalas libres para grafico final de desviaciones por puesto
          b.scales = "fixed"
          if (input$same.scale.fin) b.scales = "free"
          
          linea <- input$dataset
          cols.usar <- c(2:(ncol(reporte)-1))
          for.plot <- reporte%>%filter(LINEA == linea)%>%
               gather("PUESTO","TIEMPO",cols.usar)

          ggplotly(
          ggplot(for.plot, aes(ESTILO,TIEMPO)) +
               geom_point() +
               facet_wrap(~PUESTO, scales = b.scales) +
               geom_hline(data = for.plot%>%
                               group_by(PUESTO)%>%
                               summarise("Promedio" = mean(TIEMPO)),
                          aes(yintercept = Promedio), col = "red", lwd = 1)
          )
     })
     
     #ANALISIS FINAL
     output$total.fam <- renderTable({
          reporte <- reporte.final()
          if(is.null(reporte)) {
               return(NULL)
          } else {
               totales <- reporte%>%
                    group_by(LINEA)%>%
                    summarise("Estilos por linea"= n())
          }
     })
     
     #tabla outliers (pendiente)
     output$outliers <- renderTable({
          reporte <- reporte.final()
          if(is.null(reporte)) {
               return(NULL)
          } else {
               
          }
     })
     
     #tabla de mejora por la asigancion
     output$mejora <- renderTable({
          reporte <- reporte.final()
          if(is.null(reporte)) {
               return(NULL)
          } else {
               #crea tabla de 3 columnas ESTILO, PUESTO, TIEMPO
               fin <- dim(reporte)[2]-1
               tabla.totales <- gather(reporte, "PUESTO","TIEMPO",c(2:fin))
               final <-tabla.totales%>%
                    group_by(LINEA, PUESTO)%>%
                    summarise("Promedio" = ceiling(mean(TIEMPO, na.rm = T)),
                              "Desviacion" = ceiling(sd(TIEMPO, na.rm = T)),
                              "Porcentaje" = round(Desviacion/Promedio*100,2))
               final[is.na(final)] <- 0
               final%>%
                    group_by(PUESTO)%>%
                    summarise("Promedio" = ceiling(mean(Promedio)), 
                              "Desviacion" = ceiling(sd(Desviacion)) , 
                              "Porcentaje" = round(Desviacion/Promedio*100,2))
          }
          
     })
     
     #tabla de desviaciones por linea-puesto
     output$desviaciones <- DT::renderDataTable({
          reporte <- reporte.final()
          if(is.null(reporte)) {
               return(NULL)
          } else {
               #crea tabla de 3 columnas ESTILO, PUESTO, TIEMPO
               fin <- dim(reporte)[2]-1
               tabla.renglon <- gather(reporte, "PUESTO","TIEMPO",c(2:fin))
               
               desviaciones <- tabla.renglon%>%
                    group_by(LINEA, PUESTO)%>%
                    summarise("Promedio" = ceiling(mean(TIEMPO)),
                              "Desviacion" = round(sd(TIEMPO),2),
                              "Minimo" = min(TIEMPO) ,
                              "Maximo" = max(TIEMPO))
               DT::datatable(desviaciones, options = list(pageLength = 50))
          }
     })
     

     
     output$grafico.final <- renderPlotly({
          reporte <- reporte.final()
          if(is.null(reporte)) {
               return(NULL)
          } else {
               #crea tabla de 3 columnas ESTILO, PUESTO, TIEMPO
               fin <- dim(reporte)[2]-1
               tabla.renglon <- gather(reporte, "PUESTO","TIEMPO",c(2:fin))
               
               # grafico de desviaciones por puesto
               ggplotly(
                    ggplot(data = tabla.renglon, aes(ESTILO, TIEMPO, colour = factor(LINEA))) + 
                         geom_point() + 
                         facet_grid(PUESTO~., as.table = F, scales = "free") +
                         xlab("Estilos") +
                         ylab("Segundos por par")  +
                         #ggtitle("Dispersion de tiempo (segundos) para producir un par")+
                         theme(axis.text=element_text(size=8))
               )
          } 
          
          
     })
     
     
     
     
})
