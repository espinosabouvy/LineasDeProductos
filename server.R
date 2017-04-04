
library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(cluster)


shinyServer(function(input, output, session) {
     
     # #generar
     # datasetInput <- reactive({
     #      reporte <- reporte.final()
     #      if(is.null(reporte)) return(NULL)
     # 
     #      return(reporte)
     # })
     
     #descargar asignacion de linea (resultado de la app)
     output$download <- downloadHandler(
          filename = function() { paste("Modelos asignados por linea", '.csv', sep='') },
          content = function(file) {
               write.csv(datasetInput(), file, row.names = F)
          }
     )
     
     #convertir la informacion del sistema de Perugia a formato requerido
     importar.convertir <- reactive({
          # 
          # lectura.inicial <- reactive({
          #      require(dplyr)
          #      require(tidyr)
          #      
          #      inFile <- NULL
          #      inFile <- input$browse          
          #      #inFile$datapath <- "tiempos.csv"
          #      
          #      if (is.null(inFile)){
          #           #sin archivo seleccionado
          #           return(NULL)
          #      }
          #      
          #      #leer por tipo de archivo
          #      tiempos.raw <- read.csv(inFile$datapath, 
          #                              header = TRUE, na.strings = c("NA",""))
          #      
          #      #limpiar el formato, actual (LINEA, VCESTIL, PARES, FAMPESP, FAMMONT, DEPTO, FUNCION, TIEMPO,
          #      #PERSONAS, META)
          #      deptos.usar <- c("CORTE","CORTE Y PREPARA", "ENSAMBLES", "FAMILIA", "FORRADOS", 
          #                       "PLANTA", "RAYADO Y RESACA",
          #                       "SUELA")
          #      
          #      tiempos.raw <- tiempos.raw%>%
          #           select(DEPTO, VCESTIL, FAMPESP, FUNCION, TIEMPO)%>%
          #           filter(DEPTO %in% deptos.usar)%>%
          #           filter(TIEMPO > 0)
          #      
          #      
          #      #agrupar pespuntadores y preliminares
          #      tiempos.raw$FUNCION <- ifelse(grepl("PESPUNTADOR", 
          #                                          tiempos.raw$FUNCION),"PESPUNTADOR",
          #                                    paste(tiempos.raw$FUNCION))
          #      tiempos.raw$FUNCION <- ifelse(grepl("CA-PES", 
          #                                          tiempos.raw$FUNCION),"PESPUNTADOR",
          #                                    paste(tiempos.raw$FUNCION))
          #      #corregir del sistema, mientras, debe desaparecer
          #      tiempos.raw$FUNCION <- ifelse(grepl("PRECONFORM", 
          #                                          tiempos.raw$FUNCION),"PRELIMINAR",
          #                                    paste(tiempos.raw$FUNCION))
          #      tiempos.raw$FUNCION <- ifelse(grepl("PRELIM", 
          #                                          tiempos.raw$FUNCION),"PRELIMINAR",
          #                                    paste(tiempos.raw$FUNCION))
          #      tiempos.raw$FUNCION <- ifelse(grepl("CA-PRE", 
          #                                          tiempos.raw$FUNCION),"PRELIMINAR",
          #                                    paste(tiempos.raw$FUNCION))
          #      tiempos.raw$FUNCION <- ifelse(grepl("C-PREA", 
          #                                          tiempos.raw$FUNCION),"PRELIMINAR",
          #                                    paste(tiempos.raw$FUNCION))
          #      tiempos.raw$FUNCION <- ifelse(grepl("CA-COR", 
          #                                          tiempos.raw$FUNCION),"CORTADOR PIEL",
          #                                    paste(tiempos.raw$FUNCION))
          #      tiempos.raw$FUNCION <- ifelse(grepl("CORTADOR FLASH", 
          #                                          tiempos.raw$FUNCION),"CORTADOR PIEL",
          #                                    paste(tiempos.raw$FUNCION))
          #      tiempos.raw$FUNCION <- ifelse(grepl("PRECONF", 
          #                                          tiempos.raw$FUNCION),"PRECONFORMADOR",
          #                                    paste(tiempos.raw$FUNCION))
          #      tiempos.raw$FUNCION <- ifelse(grepl("CA-PREC", 
          #                                          tiempos.raw$FUNCION),"PRECONFORMADOR",
          #                                    paste(tiempos.raw$FUNCION))
          #      tiempos.raw$FUNCION <- ifelse(grepl("CA-DOB", 
          #                                          tiempos.raw$FUNCION),"DOBLILLADOR",
          #                                    paste(tiempos.raw$FUNCION))
          #      tiempos.raw$FUNCION <- ifelse(grepl("CA-REB", 
          #                                          tiempos.raw$FUNCION),"REBAJADOR",
          #                                    paste(tiempos.raw$FUNCION))
          #      tiempos.raw$FUNCION <- ifelse(grepl("REBAJADOR PIEL", 
          #                                          tiempos.raw$FUNCION),"REBAJADOR",
          #                                    paste(tiempos.raw$FUNCION))
          #      tiempos.raw[tiempos.raw$FUNCION == "FORRAR" & tiempos.raw$DEPTO == "PLANTA",]$FUNCION <- "RIBETEAR"
          #      
          #      #quitar comodines
          #      tiempos.raw <- tiempos.raw[!grepl("COMODIN+",  tiempos.raw$FUNCION),]
          #      names(tiempos.raw) <- make.names(names(tiempos.raw))
          #      tiempos <- tiempos.raw%>%
          #           select(DEPTO, "ESTILO" = VCESTIL, "LINEA" = FAMPESP, FUNCION, TIEMPO)
          #      
          #      #hacer correcciones al archivo y llenar combo para seleccionar depto
          #      if (is.null(tiempos)){
          #           #sin archivo seleccionado
          #           return(NULL)
          #      }
          #      
          #      #llenar combo de departamentos con el archivo preparado
          #      output$depto.select <- renderUI({
          #           deptos <- unique(tiempos$DEPTO)
          #           selectInput("depto.selected", "Selecciona el departamento que quieres analizar", as.list(deptos))
          #      })
          #      
          #      
          #      #llenar combo de lineas de produccion con el archivo preparado
          #      output$fams.select <- renderUI({
          #           fams <- unique(tiempos$LINEA)%>%sort()
          #           selectInput("fams.selected", "Selecciona las lineas que quieres analizar", as.list(fams), multiple = TRUE)
          #      })
          #      
          #      
          #      return(tiempos)
     })
     
     #lectura inicial, requiere el formato original (modificando 27/03/2017)
     leer.archivo <- reactive({

          inFile <- NULL
          inFile <- input$browse          
          #inFile$datapath <- "tiempos.csv"
          
          if (is.null(inFile)){
               #sin archivo seleccionado
               return(NULL)
          }
          
          #leer por tipo de archivo
          datos <- read.csv(inFile$datapath, 
                            header = input$header, na.strings = c("NA",""))
          #convertir NAS en cero
          datos[is.na(datos)]<-0
          names(datos)[1] <- "ESTILO"
          #estilo como factor
          datos[,1] <- as.factor(datos[,1])
          #nas a cero
          datos[is.na(datos)]<-0
          
          #limitar cantidad de modelos
          #datos<- datos[1:20,]
          
          return(datos)
          
     })
     
     
     #Tabla final de estilos asignadados por linea de produccion
     output$tabla_asignacion <- DT::renderDataTable({
          reporte <- reporte.final()
          if(is.null(reporte.final)) return(NULL)
          
          DT::datatable(reporte, options = list(pageLength = 25))
     })
     
     # Drop-down de linea a filtrar
     output$seleccion_linea <- renderUI({
          num.fam <- reporte.final()
          if(is.null(num.fam)) return(NULL)
          l.linea <- unique(num.fam$LINEA)%>%sort()
          selectInput("dataset", "Filtrar por linea de produccion", as.list(l.linea))
     })
     
     
     #escala diferente para cada grafico
     free.scale.fin <- reactive({
          b.scales = "fixed"
          if (input$same.scale.fin){
               b.scales = "free"
          }
          return(b.scales)
          
     })
     
     #muestra la asigancion por linea (usa combobox para filtrar)
     output$plot.por.linea <- renderPlotly({
          reporte <- reporte.final()
          if(is.null(reporte)) return(NULL)
          b.scales <- free.scale.fin()
          
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
     
     #Tabla de estilos asignados a cada familia
     output$total.fam <- renderTable({
          reporte <- reporte.final()
          if(is.null(reporte)) {
               return(NULL)
          } else {
               totales <- reporte%>%
                    group_by(LINEA)%>%
                    summarise("Estilos por familia"= n())
          }
     })
     
     #calcula la mejora por asignacion de estilos a lineas de produccion
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
     
     #tabla final de desviaciones por linea
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
     
     #personal por linea (se quita de este reporte)
     output$Porlinea <- DT::renderDataTable({
          temp <- reporte.final()
          if(is.null(temp)) return(NULL)
          fin <- dim(temp)[2]-1
          efic <- input$eficiencia/100
          hrs <- input$horas.trabajo
          familias <- max(temp$LINEA)
          prs <- input$pares.hora/familias
          
          tabla.renglon <- gather(temp, "PUESTO","TIEMPO",c(2:fin))%>%
               group_by(LINEA, PUESTO)%>%
               summarise("TIEMPO.PROMEDIO" = round(mean(TIEMPO),2))%>%
               mutate("PERSONAS" = ceiling(TIEMPO.PROMEDIO*prs/(efic*hrs*3600)))
          DT::datatable(tabla.renglon, options = list(pageLength = 50))
          
     })
     
     #personal por puesto (se quita de este reporte)
     output$total_puesto <- renderTable({
          temp <- reporte.final()
          if(is.null(temp)) return(NULL)
          fin <- dim(temp)[2]-1
          efic <- input$eficiencia/100
          hrs <- input$horas.trabajo
          familias <- max(temp$LINEA)
          prs <- input$pares.hora/familias
          
          tabla.puesto <- gather(temp, "PUESTO","TIEMPO",c(2:fin))%>%
               group_by(LINEA, PUESTO)%>%
               summarise("TIEMPO.PROMEDIO" = round(mean(TIEMPO),2))%>%
               mutate("PERSONAS" = ceiling(TIEMPO.PROMEDIO*prs/(efic*hrs*3600)))%>%
               group_by(PUESTO)%>%
               summarise("PERSONAS" = sum(PERSONAS))
          
     })
     
     #personal total por puesto (se quita de este reporte)
     output$Totales <- renderTable({
          temp <- reporte.final()
          if(is.null(temp)) return(NULL)
          fin <- dim(temp)[2]-1
          efic <- input$eficiencia/100
          hrs <- input$horas.trabajo
          familias <- max(temp$LINEA)
          prs <- input$pares.hora/familias
          
          tabla.totales <- gather(temp, "PUESTO","TIEMPO",c(2:fin))%>%
               group_by(LINEA, PUESTO)%>%
               summarise("TIEMPO" = round(mean(TIEMPO),2))%>%
               mutate("PERSONAS" = ceiling(TIEMPO*prs/(efic*hrs*3600)))%>%
               summarise("PERSONAS" = ceiling(sum(PERSONAS)))
          
     })
     
     #total de personas requeridas (se quita para utilizar version de analisis)
     output$grantotal <- renderPrint({
          temp <- reporte.final()
          if(is.null(temp)) return(NULL)
          fin <- dim(temp)[2]-1
          efic <- input$eficiencia/100
          hrs <- input$horas.trabajo
          familias <- max(temp$LINEA)
          prs <- input$pares.hora/familias
          
          tabla.totales <- gather(temp, "PUESTO","TIEMPO",c(2:fin))%>%
               group_by(LINEA, PUESTO)%>%
               summarise("TIEMPO" = round(mean(TIEMPO),2))%>%
               mutate("PERSONAS" = ceiling(TIEMPO*prs/(efic*hrs*3600)))%>%
               summarise("PERSONAS" = ceiling(sum(PERSONAS)))
          cat(sum(tabla.totales[,2]))
     })
     
     #grafico final de la asignacion, muestra graficamente la asignacion por puesto
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
     
     #crear la asignacion por kmean de las lineas
     reporte.final <- reactive({
          
          tabla.raw <- leer.archivo()
          if(is.null(tabla.raw)) return(NULL)
          
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
          
     })
     
     
     #tabla incial de lo leido
     output$tabla_completa <- DT::renderDataTable({
          tabla.raw <- leer.archivo()
          if(is.null(tabla.raw)) return(NULL)
          
          DT::datatable(tabla.raw, options = list(pageLength = 10))
     })
     
     #grafico de dendograma
     dendograma <- reactive({
          
          #leer tabla
          tabla.raw <- leer.archivo()
          if(is.null(tabla.raw)) return(NULL)
          
          set.seed(8)
          distancias <- dist(tabla.raw[,-1], method = "euclidian")
          clust <- hclust(distancias)
          maxx <- ceiling(max(clust$height))
          updateSliderInput(session, "altura_cluster", max = maxx)
          updateSliderInput(session, "altura_cluster", value = maxx*0.9)
          return(clust)
     })
     
     #diferente escala en cada grafico
     free.scale.ini <- reactive({
          b.scales = "fixed"
          if (input$same.scale.ini){
               b.scales = "free"
          }
          return(b.scales)
          
     })
     
     #grafica el dendograma
     output$dendograma <- renderPlot({
          tabla.raw <- leer.archivo()
          if(is.null(tabla.raw)) return(NULL)
          arbol <- dendograma()
          if(is.null(arbol)) return(NULL)
          
          g <- plot(arbol, labels = tabla.raw[,1], axes = T, main = "Cluster de agrupamiento", 
                    xlab = "Modelos", ylab = "Indice de desviacion")
          return(g + abline(h = input$altura_cluster, col="red"))
     })
     
     #escribe el numero de lineas que se crean en cada altura
     output$lineas <- renderText({
          arbol <- dendograma()
          if(is.null(arbol)) return(NULL)
          max(cutree(arbol, h=input$altura_cluster))
     })
     
     #TABLA INICIAL DE PROMEDIOS, DESVIACIONES Y RANGOS
     output$tablainicial <- renderTable({
          
          #leer tabla
          tabla.raw <- leer.archivo()
          if(is.null(tabla.raw)) return(NULL)
          
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
     
     #grafico de desviaciones por puesto
     output$graficoinicial <- renderPlotly({
          b.scales <- free.scale.ini()
          
          #leer tabla
          tabla.raw <- leer.archivo()
          if(is.null(tabla.raw)) return(NULL)
          
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
     
     #boxplot para ver desviacion entre los modelos
     output$boxplotini <- renderPlotly({
          
          #leer tabla
          tabla.raw <- leer.archivo()
          if(is.null(tabla.raw)) return(NULL)
          
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
     
})
