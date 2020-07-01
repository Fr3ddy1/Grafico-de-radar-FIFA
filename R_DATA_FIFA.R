#DATA PARA GRAFICO DE RADAR
library(highcharter)
library(reshape2)
# library(rvest)
# 
# #PAGINA A ANALIZAR
# #UTILIZO BUSQUEDA DE TRABAJO DE DATA SCIENTIST EN VENEZUELA
# webpage <- read_html("https://www.fifaindex.com/es/player/235212/achraf-hakimi/")
# 
# results <- webpage %>% html_nodes("div") 
# results[[1]]

#DATA 2018
data_18 <- read.csv("CompleteDataset.csv")

#DATA 2019
data_19 <- read.csv("~/data.csv")

#POR ESTRUCTURA DE LA DATA USARE LA DEL 2018
data_18 <- data_18[,c(2,3,14:22,32:47)]

d_18 <- data_18[1:10,c(1,3,4,23,25,26,17,20)]

rownames(d_18) <- d_18[,1]
d_18 <- d_18[,-1]
d_18$Acceleration <- as.numeric(as.character(d_18$Acceleration))
d_18$Aggression <- as.numeric(as.character(d_18$Aggression))
d_18$Stamina <- as.numeric(as.character(d_18$Stamina))
d_18$Strength <- as.numeric(as.character(d_18$Strength))
d_18$Vision <- as.numeric(as.character(d_18$Vision))
d_18$Positioning <- as.numeric(as.character(d_18$Positioning))
d_18$Shot.power <- as.numeric(as.character(d_18$Shot.power))


#melt(d_18)
#radarchart(d_18)
d_radar <- d_18
d_18 <- t(d_18)


d <- data.frame("feature"=c("Velocidad","aceleracion","potencia","defensa","pase"),"score"=c(100,80,50,96,50))

hc <- highchart() %>%
  hc_chart(polar = T) %>% 
  hc_xAxis(categories = d[,1], 
           labels = list(style = list(fontSize= '14px')), title =NULL, tickmarkPlacement = "on", lineWidth = 0) %>% 
  hc_plotOptions(series = list(marker = list(enabled = F))) %>% 
  hc_yAxis(gridLineInterpolation = "polygon", lineWidth = 0, min = 0) %>% 
  hc_add_series(name = "Jugador x", d[,2], type ="area", color = "#4472c4", pointPlacement = "on")

hc

#EJEMPLO CON GGPLOT
# devtools::install_github("ricardo-bion/ggradar", 
#                          dependencies = TRUE)

library(ggplot2)
library(ggradar)
suppressPackageStartupMessages(library(dplyr))
library(scales)

mtcars %>%
  add_rownames( var = "group" ) %>%
  mutate_each(funs(rescale), -group) %>%
  tail(4) %>% select(1:10) -> mtcars_radar

ggradar(as.data.frame(mtcars_radar[1:3,]))


#EJEMPLO FIFA
rownames(d_18) <- c("Aceleración","Agresividad","Resistencia",
                    "Fuerza","Visión","Posicionamiento","Potencia de disparo")

hc <- highchart() %>%
  hc_chart(polar = T) %>% 
  hc_xAxis(categories = rownames(d_18), 
           labels = list(style = list(fontSize= '14px')), title =NULL, tickmarkPlacement = "on", lineWidth = 0) %>% 
  hc_plotOptions(series = list(marker = list(enabled = F))) %>% 
  hc_yAxis(gridLineInterpolation = "polygon", lineWidth = 0, min = 0) %>% 
  hc_add_series(name = colnames(d_18)[2], as.numeric(d_18[,2]), type ="area", color = "#4472c4", pointPlacement = "on")

hc

#PRUEBA GRAFICO DE RADAR
d_radar$name <- row.names(d_radar)
d_radar <- d_radar[,c(8,1:7)]
row.names(d_radar) <- NULL

ggradar(d_radar[1:3,c(1,2,3,4,5)],grid.max = 105) 



# Library
library(fmsb)

# Create data: note in High school for several students
set.seed(99)
data <- as.data.frame(matrix( sample( 0:20 , 15 , replace=F) , ncol=5))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding" )
rownames(data) <- paste("mister" , letters[1:3] , sep="-")

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
data <- rbind(rep(20,5) , rep(0,5) , data)

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

# plot with default options:
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)

# Add a legend
legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)

#CREO DATA FIFA
names(d_radar) <- c("Aceleración","Agresividad","Resistencia",
                    "Fuerza","Visión","Posicionamiento","Potencia de disparo")

rownames(d_radar)[c(4,10)] <- c("L. Suárez","G. Higuaín")

d_radar <- rbind.data.frame(rep(100,7),rep(0,7),d_radar)

# plot with default options:
radarchart( d_radar  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)

# Add a legend
legend(x=0.7, y=1, legend = rownames(d_radar[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)






