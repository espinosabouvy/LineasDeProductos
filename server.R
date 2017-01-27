
library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(cluster)

shinyServer(function(input, output, session) {
     
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
     
     leer.archivo <- reactive({
          # input$file1 will be NULL initially. After the user selects
          # and uploads a file, it will be a data frame with 'name',
          # 'size', 'type', and 'datapath' columns. The 'datapath'
          # column will contain the local filenames where the data can
          # be found.
          
          
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
          
          #limitar cantidad de modelos
          #datos<- datos[1:20,]
          
          return(datos)
          
     })  
     #TABLA DE ESTILOS ASIGNADOS
     output$tabla_asignacion <- DT::renderDataTable({
          reporte <- reporte.final()
          if(is.null(reporte.final)) return(NULL)
          
          DT::datatable(reporte, options = list(pageLength = 25))
     })
     
     #ANALISIS FINAL
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
     
     #tabla outliers
     output$outliers <- renderTable({
          reporte <- reporte.final()
          if(is.null(reporte)) {
               return(NULL)
          } else {
               
          }
     })
     
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
               ggplot(data = tabla.renglon, aes(PUESTO, TIEMPO)) + 
                    geom_boxplot(col = "navy", outlier.colour = "red") + 
                    facet_wrap(~LINEA, scales = "free", ncol = 2) +
                    xlab("Puestos") +
                    ylab("Segundos por par")  +
                    ggtitle("Dispersion de tiempo (segundos) para producir un par")+
                    theme(axis.text=element_text(size=10))
               )
          }
     })
     
     
     reporte.final <- reactive({
          
          #HASTA QUE NO SE SELECCIONE UN ARCHIVO
          tabla.raw <- leer.archivo()
          if(is.null(tabla.raw)) return(NULL)
          
          arbol <- dendograma()
          if(is.null(arbol)) return(NULL)
          nolineas <- max(cutree(arbol, h=input$altura_cluster))
          
          set.seed(88)
          gr <- tabla.raw[c(2:dim(tabla.raw)[2])]
          k <- kmeans(gr, centers = nolineas ,nstart = 10, iter.max = 100)
          agrupacion.final <- cbind(tabla.raw, "LINEA" = k$cluster)
          reporte <- agrupacion.final%>%select(LINEA, ESTILO)%>%
               arrange(LINEA)
          
          #agregar cluster a tabla por renglon
          merge(tabla.raw, reporte, by = "ESTILO")
          
     })
     
     #TABLA COMPLETA DE ESTILOS Y TIEMPOS
     output$tabla_completa <- DT::renderDataTable({
          tabla.raw <- leer.archivo()
          if(is.null(tabla.raw)) return(NULL)
          
          DT::datatable(tabla.raw, options = list(pageLength = 10))
     })
     
     #GRAFICO DE CLUSTERS
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
     
     free.scale.ini <- reactive({
          b.scales = "fixed"
          if (input$same.scale.ini){
               b.scales = "free"
          }
          return(b.scales)
          
     })
     
     #grafica el dendograma
     output$dendograma <- renderPlot({
          arbol <- dendograma()
          if(is.null(arbol)) return(NULL)
          g <- plot(arbol, axes = T, main = "Cluster de agrupamiento", 
                    xlab = "Modelos", ylab = "Indice de desviaci?n")
          return(g + abline(h = input$altura_cluster, col="red"))
     })
     
     #escribe el n?mero de lineas que se crean en cada altura
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
     
     #GRAFICO INICIAL
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
               facet_wrap(~PUESTO, scales = b.scales)+ 
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
     #GRAFICO INICIAL
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
