# Redes-Agroecosistemas
Redes de interaciones frugivoras en un agroecosistema
###############################################
#Para una mayor abreviacion en los codigos se va utilizar las siguientes convenciones
#Zona1 = primer sistema; 95% de cobertura forestal con un 5% de pastizales 
#Zona2 = segundo sistema; 50% de cobertura forestal con un 50% de área agrosilvopastoril
#Zona3 = tercer sistema; 15% de cobertura forestal con un 85% de área agrosilvopastoril
###############################################

###############################################
###############################################
###############################################
#AIC primer sistema; 95% de cobertura forestal con un 5% de pastizales 
###############################################

DatoscurvaZona1 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/DatoscurvaZona1.txt", 
                              header=FALSE)

allZona1 <- specaccum(DatoscurvaZona1, method = "random")
plot(allZona1, ci.type = "poly", col = "antiquewhite", lwd = 2, ci.lty = 0, ylim = c(0, 120), 
     ci.col = "gray", main = " Figura 1. Curva de acumulación de interacciones primer sistema; 95% de cobertura forestal con un 5% de pastizales", 
     xlab = "Eventos de muestreo", 
     ylab = "Diferentes interacciones en pares")
boxplot(allZona1, col = "#AD9024", add = TRUE, pch = "+")
