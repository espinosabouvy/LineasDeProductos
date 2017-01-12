numlineas <- function(archivo = "tiempos.csv"){
     require(cluster)
     require(ggplot2)
     require(dplyr)
     require(tidyr)
     require(gridExtra)
     
     #cargar csv
     #en renglones
     tiempos.raw <- read.csv(archivo)
     
     #revisar nas (para cada puesto debe existir dato, si no existe, debe ser cero)
     #convertir a tabla, convertirá los vacios en nas, se deben convertir a cero
     tiempos.col <- spread(data = tiempos.raw, key = PUESTO, value = SEGUNDOS.POR.PAR)
     tiempos.col[is.na(tiempos.col)]<- 0
     tiempos.col <<- tiempos.col
     
     #convertir en renglones para analisis, pero no para k-means
     tiempos.ren  <<- gather(tiempos.col, PUESTO, SEGUNDOS.POR.PAR, -ESTILO)
     
     #hclust
     set.seed(8)
     
     distancias <- dist(tiempos.col[-1], method = "euclidian")
     arbol <<- hclust(distancias)
     
     nrd = 500
     ngr = 300
     nbl = 1500 
     
     plot(arbol, axes = T, main = "Cluster de agrupamiento", 
          xlab = "Modelos", ylab = "Indice de desviación")
     abline(h = nrd, col="red") + 
          abline(h = ngr, col = "green") + 
          abline(h = nbl, col = "blue")
     
     rd <- max(unique(cutree(arbol, h= nrd)))
     gr <- max(unique(cutree(arbol, h= ngr)))
     bl <- max(unique(cutree(arbol, h= nbl)))
     
     #familias a crear en el análisis
     familias <<- rd
     
     
}

asignacion <- function(fams = familias){
     set.seed(88)
     gr <- tiempos.col[c(2:dim(tiempos.col)[2])]
     k <<- kmeans(gr, centers = fams ,nstart = 10, iter.max = 100)
     agrupacion.final <- cbind(tiempos.col, "LINEA.PRODUCCION" = k$cluster)
     reporte <<- agrupacion.final%>%select(LINEA.PRODUCCION, ESTILO)%>%
          arrange(LINEA.PRODUCCION)
     
     #agregar cluster a tabla por renglon
     tiempos.ren.cluster <<- merge(tiempos.ren, reporte, by = "ESTILO")%>%
          arrange(LINEA.PRODUCCION)
     ctrs <<- data.frame("LINEA.PRODUCCION" = seq(1:fams), k$centers)%>%
          gather("PUESTO","SEGUNDOS.POR.PAR",-1)%>%
          arrange(LINEA.PRODUCCION)
     
     aux <- tiempos.ren.cluster%>%
          group_by(LINEA.PRODUCCION, PUESTO)%>%
          summarise("media" = mean(SEGUNDOS.POR.PAR),
                    "sd" = sd(SEGUNDOS.POR.PAR),
                    "mas" = media + 2*sd,
                    "menos" = ifelse(media - 2*sd <0,0,media - 2*sd))
     
     plotting <- merge(tiempos.ren.cluster, aux, by = c("LINEA.PRODUCCION","PUESTO"))%>%
          mutate("outliers" = ifelse(SEGUNDOS.POR.PAR < menos | SEGUNDOS.POR.PAR > mas,
                                      1,0))
     
     g <- list()
     
     for (i in 1:fams){

          temp <- plotting%>%
               filter(LINEA.PRODUCCION== i)
          tempcenters <- ctrs%>%
               filter(LINEA.PRODUCCION== i)

          g[[i]] <- ggplot(data = temp,
                 aes(factor(ESTILO), SEGUNDOS.POR.PAR, label = ESTILO)) +
               geom_point(col = "navy") +
               geom_point(data = temp%>%filter(outliers == 1),
                          aes(factor(ESTILO), SEGUNDOS.POR.PAR), col = "red") +
               geom_text(data = temp%>%filter(outliers == 1),
                         aes(label=ESTILO),hjust=0,vjust=0, size = 3) +
               geom_hline(data = tempcenters,
                          aes(yintercept = SEGUNDOS.POR.PAR), lwd = 1, col = "pink")+
               facet_grid(~PUESTO, scales = "free") +
               scale_colour_discrete(name="LINEA") +
               xlab("ESTILO") +
               ggtitle(paste("LINEA PRODUCCION",i))
     }
     do.call(grid.arrange, g)
}