
#*******************************************************************
# REDES DE INTERACCIÓN MUTUALISTAS ENTRE PLANTAS ORNITÓCORAS Y AVES FRUGÍVORAS 
# EN UN BOSQUE ANDINO TRANSFORMADO
# 
#Sergio Giovanny Bernal*, Astrid Viviana Ramírez Castro, Angela Parrado-Rosselli
#*******************************************************************





#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#1. Representatividad del Muestreo
#Cargar archivos con registros mensuales
#1.1 La tabla contiene los datos de registro semanal de las interacciones
#1.2 cada zona se registra por separado
#1.3 El texto se guarda en formato txt
#######################################################################################################################

library(lattice)
library(permute)
library(vegan)

###############################################
###############################################
###############################################
#AIC Zona 1  95% CVB 
###############################################

DatoscurvaZona1 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/DatoscurvaZona1.txt", 
                              header=FALSE)

allZona1 <- specaccum(DatoscurvaZona1, method = "random")
plot(allZona1, ci.type = "poly", col = "antiquewhite", lwd = 2, ci.lty = 0, ylim = c(0, 120), 
     ci.col = "gray", main = "Curva de acumulación de interacciones Zona 1 con 95% CVB", 
     xlab = "Eventos de muestreo", 
     ylab = "Diferentes interacciones en pares")
boxplot(allZona1, col = "#AD9024", add = TRUE, pch = "+")

allZona1

IACZona1 <- specpool(DatoscurvaZona1) 
IACZona1

###############################################################################
#Resultado: chao: 118.2321 y chao.se: 4.966566
#Interpretacion: Riqueza de interacciones observada es de 111 y la esperada es de 118 con
#una desviacion de 4.9.
#allZona1: representa como la riqueza de interacciones se va aplanando en la curva a partir 
#del muestreo #18
###############################################################################

IAC.CHAO.Zona1 = 111*100/118.2321
IAC.CHAO.Zona1

###############################################################################
#Interpretacion: El muestreo recoje el 93.88313% de la Riqueza de interacciones 
#presente en la zona 1
###############################################################################


###############################################
###############################################
###############################################
#AIC Zona 2  50% CVB 
###############################################

DatoscurvaZona2 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/DatoscurvaZona2.txt", 
                              header=FALSE)

allZona2 <- specaccum(DatoscurvaZona2, method = "random")
plot(allZona2, ci.type = "poly", col = "antiquewhite", lwd = 2, ci.lty = 0, ylim = c(0, 250), 
     ci.col = "gray", main = "Curva de acumulación de interacciones Zona 2 con 50% CVB", 
     xlab = "Eventos de muestreo", 
     ylab = "Diferentes interacciones en pares")
boxplot(allZona2, col = "#325F8B", add = TRUE, pch = "+")

allZona2

IACZona2 <- specpool(DatoscurvaZona2) 
IACZona2

###############################################################################
#Resultado: chao: 252.0319 y chao.se: 5.443631
#Interpretacion: Riqueza de interacciones observada es de 241 y la esperada es de 252 con
#una desviacion de 5.4
#allZona2: representa como la riqueza de interacciones se va aplanando en la curva a partir 
#del muestreo #18
###############################################################################

IAC.CHAO.Zona2 = 241*100/252.0319
IAC.CHAO.Zona2

###############################################################################
#Interpretacion: El muestreo recoje el 95.62282% de la Riqueza de interacciones 
#presente en la zona 12
###############################################################################



###############################################
###############################################
###############################################
#AIC Zona 3  15% CVB 
###############################################

DatoscurvaZona3 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/DatoscurvaZona3.txt", 
                              header=FALSE)

allZona3 <- specaccum(DatoscurvaZona3, method = "random")
plot(allZona3, ci.type = "poly", col = "antiquewhite", lwd = 2, ci.lty = 0, ylim = c(0, 170), 
     ci.col = "gray", main = "Curva de acumulación de interacciones Zona 3 con 15% CVB", 
     xlab = "Eventos de muestreo", 
     ylab = "Diferentes interacciones en pares")
boxplot(allZona3, col = "#E76B49", add = TRUE, pch = "+")

allZona3

IACZona3 <- specpool(DatoscurvaZona3) 
IACZona3

###############################################################################
#Resultado: chao: 164.8202 y chao.se: 3.411226
#Interpretacion: Riqueza de interacciones observada es de 159 y la esperada es de 164.8 con
#una desviacion de 3.41
#allZona3: representa como la riqueza de interacciones se va aplanando en la curva a partir 
#del muestreo #18
###############################################################################

IAC.CHAO.Zona3 = 159*100/164.8202
IAC.CHAO.Zona3

###############################################################################
#Interpretacion: El muestreo recoje el 96.46876% de la Riqueza de interacciones 
#presente en la zona 3
#Para consultar colores en la paleta ir a https://r-charts.com/es/paletas-colores/
###############################################################################












#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#2. Graficas ponderadas de las redes de interaccion frugivoras 
#Cargar archivos con registros del conteo a proposito del consumo de frutos
#1.1 La tabla contiene los datos de registro total de las interacciones
#1.2 cada zona se registra por separado
#1.3 Las tablas se guardan en formato txt
#######################################################################################################################
###############################################################################
#############################Importar el archivo a R###########################
###############################################################################
#Importar archivo desde 
#########################
####Import Dataset#######
####*from Text base*#####
######################### 

library(networkD3)

###############################################
###############################################
###############################################
#RED Zona 1  95% CVB 
###############################################

links1 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/links1.txt")

nodes1 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/nodes1.txt")

sankeyNetwork(Links = links1, Nodes = nodes1, 
              Source = "source", Target = "target",
              Value = "value", NodeID = "especie",
              colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"),
              fontSize= 11, margin = list(left = 1100, fontFamily = NULL, 
              nodeWidth = 100, nodePadding = 10, margin = NULL,
              height = NULL, width = NULL, iterations = 50, sinksRight = FALSE))


###############################################
###############################################
###############################################
#RED Zona 2  50% CVB 
###############################################

links2 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/links2.txt")

nodes2 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/nodes2.txt")

sankeyNetwork(Links = links2, Nodes = nodes2, 
              Source = "source", Target = "target",
              Value = "value", NodeID = "especie",
              colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"),
              fontSize= 11, margin = list(left = 1100, fontFamily = NULL, 
              nodeWidth = 100, nodePadding = 10, margin = NULL,
              height = NULL, width = NULL, iterations = 50, sinksRight = FALSE))

###############################################
###############################################
###############################################
#RED Zona 3  15% CVB 
###############################################

links3 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/links3.txt")

nodes3 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/nodes3.txt")

sankeyNetwork(Links = links3, Nodes = nodes3, 
              Source = "source", Target = "target",
              Value = "value", NodeID = "especie",
              colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"),
              fontSize= 11, margin = list(left = 1100, fontFamily = NULL, 
              nodeWidth = 100, nodePadding = 10, margin = NULL,
              height = NULL, width = NULL, iterations = 50, sinksRight = FALSE))

#######################################################################################################################
#######################################################################################################################






#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#3. Metricas de Redes 
#Cargar archivos con registros del conteo a proposito del consumo de frutos
#1.1 La tabla contiene los datos de registro total de las interacciones
#1.2 cada zona se registra por separado
#1.3 Las tablas se guardan en formato txt
#1.4 Las matriz de contingencia es la tabla de referencia
#######################################################################################################################

library(bipartite)

###############################################
###############################################
###############################################
#Metricas Zona 1  95% CVB 
###############################################

Zona1 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Zona1.txt", row.names=1)

NODF.Zona1 = networklevel(Zona1, index = 'NODF')
H2.Zona1 =networklevel(Zona1, index = 'H2')

MetricasZona1 = rbind(NODF.Zona1,H2.Zona1)
MetricasZona1

###############################################
###############################################
###############################################
#AIC Zona 2  50% CVB 
###############################################

Zona2 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Zona2.txt", row.names=1)

NODF.Zona2 = networklevel(Zona2, index = 'NODF')
H2.Zona2 =networklevel(Zona2, index = 'H2')

MetricasZona2 = rbind(NODF.Zona2,H2.Zona2)
MetricasZona2

###############################################
###############################################
###############################################
#Metricas Zona 3  15% CVB 
###############################################

Zona3 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Zona3.txt", row.names=1)

NODF.Zona3 = networklevel(Zona3, index = 'NODF')
H2.Zona3 =networklevel(Zona3, index = 'H2')

MetricasZona3 = rbind(NODF.Zona3,H2.Zona3)
MetricasZona3

###############################################
###############################################
###############################################
#Metricas totales 
###############################################

MetricasTotales = rbind(MetricasZona1, MetricasZona2, MetricasZona3)
MetricasTotales

#Anidamiento: Describe cualitativamente y cuantitativamente 
#el cómo se mantienen las relaciones entre especies en una comunidad.
#
#Especialización H2´: Indica que tan diversas son esas interacciones 
#considerando toda la matriz y cada especie.
#
#######################################################################################################################
#######################################################################################################################



networklevel(Zona1, index = 'connectance')
networklevel(Zona2, index = 'connectance')
networklevel(Zona3, index = 'connectance')

networklevel(Zona1, index = 'vulnerability')
networklevel(Zona2, index = 'vulnerability')
networklevel(Zona3, index = 'vulnerability')

networklevel(Zona1, index = 'robustness')
networklevel(Zona2, index = 'robustness')
networklevel(Zona3, index = 'robustness')


networklevel(Zona1)
networklevel(Zona2)
networklevel(Zona3)


#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#4. Significancia de Metricas utilizando modelos Nulos
#######################################################################################################################
# 1. unir las tres zonas
# 2. renombrar el archivo
# 3. transformar vectores
# 4. crear vector con resultados con cada una de las metricas para todas las zonas
# 5. Metricas a evaluar connectance, NODF, H2, 
# 6. HL Plantas y ll aves

webs.names <- c("Zona1", "Zona2", "Zona3")

webs <- lapply(c("Zona1", "Zona2", "Zona3"), get)

names(webs) <- webs.names

net.metrics.NODF <- lapply(webs, networklevel, index = 'NODF') 

net.metrics.H2 <- lapply(webs, networklevel, index = 'H2')

net.metrics.connectance <- lapply(webs, networklevel, index = 'connectance')

net.metrics.connectance <- lapply(webs, networklevel, index = 'vulnerability')

net.metrics.connectance <- lapply(webs, networklevel, index = 'robustness')

#Luego, crearemos modelos nulos, que son redes nuevas que se han 
#aleatorizado hasta cierto punto (en función de las redes observadas originales) 
#Usaremos el modelo nulos que funcionan para redes ponderadas:
#r2dtabla
# Usamos la función nullmodel()para crear nuestros nulos 
#(establecemos el número de nulos en 2000) y podemos usar el methodargumento para 
#especificar el tipo de modelo nulo.

net.nulls.r2d <- lapply(webs, nullmodel, method = "r2dtable", N = 2000) 

#Luego, necesitamos calcular los mismos índices para los diferentes nulos 
#(como ya hicimos con las redes observadas: 'NODF'.....) 
#creados por cada tipo de modelo nulo. Dado que habrá muchas repeticiones, 
#comenzamos definiendo una función para cada índice donde podemos especificar 
#un tipo de modelo nulo para generar la distribución nula.

# Null distribution function for NODF - calculates the network NODF 
#for each null (using a particular null method) for each site 

net.null.NODF = function(nulls){
  net.null.metric <- list()
  for (i in 1:length(nulls)) {
    net.null.metric[[i]] = do.call('rbind', 
                                   lapply(nulls[[i]], networklevel, index = 'NODF'))
  }
  names(net.null.metric) <- webs.names
  return(net.null.metric)
}

# Null distribution function for H2 - calculates the network H2 
#for each null (using a particular null method) for each site 

net.null.H2 = function(nulls){
  net.null.metric <- list()
  for (i in 1:length(nulls)) {
    net.null.metric[[i]] = do.call('rbind', 
                                   lapply(nulls[[i]], networklevel, index = 'H2'))
  }
  names(net.null.metric) <- webs.names
  return(net.null.metric)
}

# Null distribution function for connectance - calculates the network connectance 
#for each null (using a particular null method) for each site 

net.null.connectance = function(nulls){
  net.null.metric <- list()
  for (i in 1:length(nulls)) {
    net.null.metric[[i]] = do.call('rbind', 
                                   lapply(nulls[[i]], networklevel, index = 'connectance'))
  }
  names(net.null.metric) <- webs.names
  return(net.null.metric)
}

# Null distribution function for vulnerability - calculates the network vulnerability 
#for each null (using a particular null method) for each site 

net.null.vulnerability = function(nulls){
  net.null.metric <- list()
  for (i in 1:length(nulls)) {
    net.null.metric[[i]] = do.call('rbind', 
                                   lapply(nulls[[i]], networklevel, index = 'vulnerability'))
  }
  names(net.null.metric) <- webs.names
  return(net.null.metric)
}

# Null distribution function for robustness - calculates the network robustness 
#for each null (using a particular null method) for each site 

net.null.robustness = function(nulls){
  net.null.metric <- list()
  for (i in 1:length(nulls)) {
    net.null.metric[[i]] = do.call('rbind', 
                                   lapply(nulls[[i]], networklevel, index = 'robustness'))
  }
  names(net.null.metric) <- webs.names
  return(net.null.metric)
}





# Time consuming step!

r2d.NODF <- net.null.NODF(net.nulls.r2d)

r2d.H2 <- net.null.H2(net.nulls.r2d)

r2d.connectance <- net.null.connectance(net.nulls.r2d)

r2d.vulnerability <- net.null.vulnerability(net.nulls.r2d)

r2d.robustness <- net.null.robustness(net.nulls.r2d)

#A continuación, definiremos una función para calcular el puntaje z al comparar 
#la red observada con las redes nulas.

# Z-score function for comparing different networks

net.zscore = function(obsval, nullval) {
  (obsval - mean(nullval))/sd(nullval)  
} 

#Luego, aplicamos la net.zscore()función para obtener nuestros puntajes z para 
#cada sitio de la red.

# Function that perform z-score calculation of NODF using the observed and null networks
NODF.zscore = function(nulltype){
  net.NODF.zscore <- list() 
  for(i in 1:length(net.metrics.NODF)){
    net.NODF.zscore[[i]] = net.zscore(net.metrics.NODF[[i]]['NODF'], 
                                      nulltype[[i]][ ,'NODF'])
  }
  names(net.NODF.zscore) <- webs.names
  return(net.NODF.zscore)
}

# Function that perform z-score calculation of H2 using the observed and null networks
H2.zscore = function(nulltype){
  net.H2.zscore <- list() 
  for(i in 1:length(net.metrics.H2)){
    net.H2.zscore[[i]] = net.zscore(net.metrics.H2[[i]]['H2'], 
                                      nulltype[[i]][ ,'H2'])
  }
  names(net.H2.zscore) <- webs.names
  return(net.H2.zscore)
}

# Function that perform z-score calculation of connectance using the observed and null networks
connectance.zscore = function(nulltype){
  net.connectance.zscore <- list() 
  for(i in 1:length(net.metrics.connectance)){
    net.connectance.zscore[[i]] = net.zscore(net.metrics.connectance[[i]]['connectance'], 
                                    nulltype[[i]][ ,'connectance'])
  }
  names(net.connectance.zscore) <- webs.names
  return(net.connectance.zscore)
}

# Function that perform z-score calculation of vulnerability using the observed and null networks
vulnerability.zscore = function(nulltype){
  net.vulnerability.zscore <- list() 
  for(i in 1:length(net.metrics.vulnerability)){
    net.vulnerability.zscore[[i]] = net.zscore(net.metrics.vulnerability[[i]]['vulnerability'], 
                                             nulltype[[i]][ ,'vulnerability'])
  }
  names(net.vulnerability.zscore) <- webs.names
  return(net.vulnerability.zscore)
}

# Function that perform z-score calculation of robustness using the observed and null networks
robustness.zscore = function(nulltype){
  net.robustness.zscore <- list() 
  for(i in 1:length(net.metrics.robustness)){
    net.robustness.zscore[[i]] = net.zscore(net.metrics.robustness[[i]]['robustness'], 
                                             nulltype[[i]][ ,'robustness'])
  }
  names(net.robustness.zscore) <- webs.names
  return(net.robustness.zscore)
}



r2d.NODF.zscore <- NODF.zscore(r2d.NODF)
r2d.H2.zscore <- H2.zscore(r2d.H2)
r2d.connectance.zscore <- connectance.zscore(r2d.connectance)
r2d.vulnerability.zscore <- vulnerability.zscore(r2d.vulnerability)
r2d.robustness.zscore <- robustness.zscore(r2d.robustness)


#Por último, calculamos el valor p de dos caras para la importancia de la 
#propiedad de la red para metricas. 
#Comenzamos definiendo una función a continuación y luego la aplicamos para
#r2d y métricas en los nulos.

# Function that adds p-values according to the obtained z-scores

add.pvalues = function(net.metric.zscore){
  
  # Change the output class from list of a list into a matrix
  net.metric.zscore <- do.call('rbind', net.metric.zscore) 
  
  # Convert z-scores to p-values (two-sided)
  net.metric.pvalue <- 2*pnorm(-abs(net.metric.zscore))
  
  # Change matrix into a dataframe
  net.metric.pvalue <- as.data.frame(as.table(net.metric.pvalue))
  colnames(net.metric.pvalue) <- c('site', 'metric', 'pvalue')
  
  net.metric.pvalue <- within(net.metric.pvalue, {
    significance <- ifelse(pvalue <= 0.001, "***", 
                           ifelse(pvalue <= 0.01, "**",
                                  ifelse(pvalue <= 0.05, "*", "not significant")))
  })
  return(net.metric.pvalue)
} 

# Add the p-values to our NODF results
r2d.test.NODF <- add.pvalues(r2d.NODF.zscore)

# Add the p-values to our H2 results
r2d.test.H2 <- add.pvalues(r2d.H2.zscore)

# Add the p-values to our H2 results
r2d.test.connectance <- add.pvalues(r2d.connectance.zscore)

# Add the p-values to our H2 results
r2d.test.vulnerability <- add.pvalues(r2d.vulnerability.zscore)

# Add the p-values to our H2 results
r2d.test.robustness <- add.pvalues(r2d.robustness.zscore)

# Print the NODF, H2, connectance, vulnerability and robustness results

print(r2d.test.NODF)
print(r2d.test.H2)
print(r2d.test.connectance)
print(r2d.test.vulnerability)
print(r2d.test.robustness)

##########################################################################################
##########################################################################################













#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#5. Significancia de la red utilizando modelos Nulos
#######################################################################################################################

#*******************************************************************
# test de fisher
#*******************************************************************

library(stats)

###############################################
###############################################
###############################################
#fisher Zona 1  95% CVB 
###############################################

fisher.test(Zona1,simulate.p.value=TRUE)

###############################################
###############################################
###############################################
#fisher Zona 2  50% CVB 
###############################################

fisher.test(Zona2,simulate.p.value=TRUE)

###############################################
###############################################
###############################################
#fisher Zona 3  15% CVB 
###############################################

fisher.test(Zona3,simulate.p.value=TRUE)

#Interpretacion: calculamos el valor p de la matriz de contingencias con 2000 redes al azar 
#encontrando que las redes no presentan interacciones al azar sino que si hay preferencia o . 
#interacciones que presentan una direccion o preferencia dada la dieta de las aves 
#las razones son desconocidas pero se debe empiezar a sugerir explicaciones.



##########################################################################################
##########################################################################################











#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#6. modularidad
#######################################################################################################################

#
#Crear Objeto de clase modulweb
#

###############################################
###############################################
###############################################
#Modularidad Zona 1  95% CVB 
###############################################

Modularidad.zona1 = computeModules(Zona1, method="Beckett",
                                   deep = FALSE, deleteOriginalFiles = TRUE,
                                   steps = 1000000, tolerance = 1e-10, experimental 
                                   = FALSE, forceLPA=FALSE)

plotModuleWeb(Modularidad.zona1, plotModules = TRUE, displayAlabels = TRUE,
              displayBlabels = TRUE, labsize = 0.5, xlabel = "", ylabel = "",
              square.border = "gray98", fromDepth = 0, upToDepth = -1)


###############################################
###############################################
###############################################
#Modularidad Zona 2  50% CVB 
###############################################

Modularidad.zona2 = computeModules(Zona2, method="Beckett", 
                                   deep = FALSE, deleteOriginalFiles = TRUE,
                                   steps = 1000000, tolerance = 1e-10, experimental 
                                   = FALSE, forceLPA=FALSE)

plotModuleWeb(Modularidad.zona2, plotModules = TRUE, displayAlabels = TRUE,
              displayBlabels = TRUE, labsize = 0.5, xlabel = "", ylabel = "",
              square.border = "gray98", fromDepth = 0, upToDepth = -1)


###############################################
###############################################
###############################################
#Modularidad Zona 3  15% CVB 
###############################################


Modularidad.zona3 = computeModules(Zona3, method="Beckett", 
                                   deep = FALSE, deleteOriginalFiles = TRUE,
                                   steps = 1000000, tolerance = 1e-10, experimental 
                                   = FALSE, forceLPA=FALSE)

plotModuleWeb(Modularidad.zona3, plotModules = TRUE, displayAlabels = TRUE,
              displayBlabels = TRUE, labsize = 0.5, xlabel = "", ylabel = "",
              square.border = "gray98", fromDepth = 0, upToDepth = -1)


##########################################################################################
##########################################################################################








##########################################################################################
##########################################################################################
#
#7. EXTINCIONES
#
#Calcula las consecuencias de eliminar una especie de una red bipartita. 
#Con funcionalidad de trazado y estimación de pendientes.

#La función escala las secuencias de extinción a valores 
#entre 0 y 1 para cada participante. El eje x del gráfico 
#muestra la proporción de participantes exterminados, mientras 
#que el eje y muestra la proporción de extinciones secundarias. 
#Dado que estas curvas generalmente siguen una función hiperbólica 
#(ver ejemplos en Memmott et al. 2004), esto se ajusta a los datos.

#En la actualidad, solo se ajusta una función de tipo y \ sim 1 - x ^ a 
#(usando nls), es decir, sin compensación. Aunque normalmente esta 
#función ofrece muy buenos ajustes, compruebe el gráfico y juzgue usted mismo.
#El ajuste de esta función simple hace que su parámetro sea "a" una medida 
#de vulnerabilidad a la extinción. Cuanto más graduales son las extinciones 
#secundarias, menor es el valor absoluto de "a". O, dicho de otra manera, 
#los valores absolutos grandes de "a" indican una extinción muy abrupta, 
#indicativo de una alta redundancia inicial en la red.

#Resultados
#proporción de especies en otro nivel trófico aún vivas
#proporción de extinciones primarias
#
######################################################
#CREAR OBJETO CLASE BIPARTITE CON FUNCION EXTINCION
########################################################
############################################################################

#Zona 1 

############################################################################
######################################################
######################################################
#Extinciones primaria de plantas mayormente conectadas
######################################################

e = extinction(Zona1, participant = "both", method = "random", ext.row=NULL, 
               ext.col=NULL)

w.blueZona1 <- extinction(Zona1, participant="lower", 
                          method="abun")

ex.BlueZona1 <- second.extinct(w.blueZona1, 
                               participant="lower", method="random", nrep=100, 
                               details=TRUE)
ex.BlueZona1 <- second.extinct(w.blueZona1, 
                               participant="lower", method="random", nrep=100, 
                               details=FALSE)
slope.bipartite(ex.BlueZona1)

###Guardar archivo 
#(Ext.Zona1.plant.Mas.conn)
#######
######################################################
######################################################
#Extinciones primaria de plantas menormente conectadas
######################################################
ex.BlueZona1 <- second.extinct(Zona1, 
                               participant="lower", method="random", nrep=100, 
                               details=TRUE)

ex.BlueZona1 <- second.extinct(Zona1, 
                               participant="lower", method="random", nrep=100, 
                               details=FALSE)
slope.bipartite(ex.BlueZona1)

###Guardar archivo 
#(Ext.Zona1.plant.Menos.conn)
#######

######################################################
######################################################
#Extinciones primaria de aves mayormente conectadas
######################################################

e = extinction(Zona1, participant = "both", method = "random", ext.row=NULL, 
               ext.col=NULL)

w.blueZona1 <- extinction(Zona1, participant="higher", 
                          method="abun")

ex.BlueZona1 <- second.extinct(w.blueZona1, 
                               participant="higher", method="random", nrep=100, 
                               details=TRUE)
ex.BlueZona1 <- second.extinct(w.blueZona1, 
                               participant="higher", method="random", nrep=100, 
                               details=FALSE)
slope.bipartite(ex.BlueZona1)

###Guardar archivo 
#(Ext.Zona1.aves.Mas.conn)
#######
######################################################
######################################################
#Extinciones primaria de aves menormente conectadas
######################################################
ex.BlueZona1 <- second.extinct(Zona1, 
                               participant="higher", method="random", nrep=100, 
                               details=TRUE)
ex.BlueZona1 <- second.extinct(Zona1, 
                               participant="higher", method="random", nrep=100, 
                               details=FALSE)
slope.bipartite(ex.BlueZona1)

###Guardar archivo 
#(Ext.Zona1.aves.Menos.conn)
#######

######################################################
######################################################
######################################################

######################################################
#CREAR OBJETO CLASE BIPARTITE CON FUNCION EXTINCION
########################################################
############################################################################

#Zona 2 

############################################################################
######################################################
######################################################
#Extinciones primaria de plantas mayormente conectadas
######################################################

e = extinction(Zona2, participant = "both", method = "random", ext.row=NULL, 
               ext.col=NULL)

w.blueZona2 <- extinction(Zona2, participant="lower", 
                          method="abun")

ex.BlueZona2 <- second.extinct(w.blueZona2, 
                               participant="lower", method="random", nrep=100, 
                               details=TRUE)
ex.BlueZona2 <- second.extinct(w.blueZona2, 
                               participant="lower", method="random", nrep=100, 
                               details=FALSE)
slope.bipartite(ex.BlueZona2)

###Guardar archivo 
#(Ext.Zona2.plant.Mas.conn)
#######
######################################################
######################################################
#Extinciones primaria de plantas menormente conectadas
######################################################
ex.BlueZona2 <- second.extinct(Zona2, 
                               participant="lower", method="random", nrep=100, 
                               details=TRUE)

ex.BlueZona2 <- second.extinct(Zona2, 
                               participant="lower", method="random", nrep=100, 
                               details=FALSE)
slope.bipartite(ex.BlueZona2)

###Guardar archivo 
#(Ext.Zona2.plant.Menos.conn)
#######

######################################################
######################################################
#Extinciones primaria de aves mayormente conectadas
######################################################

e = extinction(Zona2, participant = "both", method = "random", ext.row=NULL, 
               ext.col=NULL)

w.blueZona2 <- extinction(Zona2, participant="higher", 
                          method="abun")

ex.BlueZona2 <- second.extinct(w.blueZona2, 
                               participant="higher", method="random", nrep=100, 
                               details=TRUE)
ex.BlueZona2 <- second.extinct(w.blueZona2, 
                               participant="higher", method="random", nrep=100, 
                               details=FALSE)
slope.bipartite(ex.BlueZona2)

###Guardar archivo 
#(Ext.Zona2.aves.Mas.conn)
#######
######################################################
######################################################
#Extinciones primaria de aves menormente conectadas
######################################################
ex.BlueZona2 <- second.extinct(Zona2, 
                               participant="higher", method="random", nrep=100, 
                               details=TRUE)
ex.BlueZona2 <- second.extinct(Zona2, 
                               participant="higher", method="random", nrep=100, 
                               details=FALSE)
slope.bipartite(ex.BlueZona2)

###Guardar archivo 
#(Ext.Zona2.aves.Menos.conn)
#######

######################################################
######################################################
######################################################

######################################################
#CREAR OBJETO CLASE BIPARTITE CON FUNCION EXTINCION
########################################################
############################################################################

#Zona 3 

############################################################################
######################################################
######################################################
#Extinciones primaria de plantas mayormente conectadas
######################################################

e = extinction(Zona3, participant = "both", method = "random", ext.row=NULL, 
               ext.col=NULL)

w.blueZona3 <- extinction(Zona3, participant="lower", 
                          method="abun")

ex.BlueZona3 <- second.extinct(w.blueZona3, 
                               participant="lower", method="random", nrep=100, 
                               details=TRUE)
ex.BlueZona3 <- second.extinct(w.blueZona3, 
                               participant="lower", method="random", nrep=100, 
                               details=FALSE)
slope.bipartite(ex.BlueZona3)

###Guardar archivo 
#(Ext.Zona3.plant.Mas.conn)
#######
######################################################
######################################################
#Extinciones primaria de plantas menormente conectadas
######################################################
ex.BlueZona1 <- second.extinct(Zona1, 
                               participant="lower", method="random", nrep=100, 
                               details=TRUE)

ex.BlueZona1 <- second.extinct(Zona1, 
                               participant="lower", method="random", nrep=100, 
                               details=FALSE)
slope.bipartite(ex.BlueZona1)

###Guardar archivo 
#(Ext.Zona3.plant.Menos.conn)
#######

######################################################
######################################################
#Extinciones primaria de aves mayormente conectadas
######################################################

e = extinction(Zona1, participant = "both", method = "random", ext.row=NULL, 
               ext.col=NULL)

w.blueZona1 <- extinction(Zona1, participant="higher", 
                          method="abun")

ex.BlueZona1 <- second.extinct(w.blueZona1, 
                               participant="higher", method="random", nrep=100, 
                               details=TRUE)
ex.BlueZona1 <- second.extinct(w.blueZona1, 
                               participant="higher", method="random", nrep=100, 
                               details=FALSE)
slope.bipartite(ex.BlueZona1)

###Guardar archivo 
#(Ext.Zona3.aves.Mas.conn)
#######
######################################################
######################################################
#Extinciones primaria de aves menormente conectadas
######################################################
ex.BlueZona1 <- second.extinct(Zona1, 
                               participant="higher", method="random", nrep=100, 
                               details=TRUE)
ex.BlueZona1 <- second.extinct(Zona1, 
                               participant="higher", method="random", nrep=100, 
                               details=FALSE)
slope.bipartite(ex.BlueZona1)

###Guardar archivo 
#(Ext.Zona3.aves.Menos.conn)
#######

################################################################################
################################################################################
################################################################################

#################################################################################################################
#######################################################################################################################
#######################################################################################################################
#8. correspondencias
#######################################################################################################################
################################################################################
################################################################################

library(factoextra)
library(FactoMineR)
library(igraph)
library(gplots)
library(ape)


###############################################
###############################################
###############################################
#correspondencias Zona 1  95% CVB 
###############################################

fisher.test(Zona1,simulate.p.value=TRUE)
Zona1.ca <- CA(Zona1, graph = FALSE)
print(Zona1.ca)
fviz_screeplot(Zona1.ca, addlabels = TRUE, ylim = c(0, 20))
Zona1eig.val <- get_eigenvalue(Zona1.ca)
Zona1eig.val

fviz_ca_biplot(Zona1.ca, col.row = "cos2",
               gradient.cols = c("#FF3E96", "#008B00", "#6A5ACD"), 
               repel = TRUE)
fviz_ca_biplot(Zona1.ca, col.col = "cos2",
               gradient.cols = c("#FF3E96", "#008B00"), 
               repel = TRUE)

###############################################
###############################################
###############################################
#correspondencias Zona 2  50% CVB 
###############################################

fisher.test(Zona2,simulate.p.value=TRUE)
Zona2.ca <- CA(Zona2, graph = FALSE)
print(Zona2.ca)
fviz_screeplot(Zona2.ca, addlabels = TRUE, ylim = c(0, 18))
Zona2eig.val <- get_eigenvalue(Zona2.ca)
Zona2eig.val

fviz_ca_biplot(Zona2.ca, col.row = "cos2",
               gradient.cols = c("#FF3E96", "#008B00", "#6A5ACD"), 
               repel = TRUE)
fviz_ca_biplot(Zona2.ca, col.col = "cos2",
               gradient.cols = c("#FF3E96", "#008B00"), 
               repel = TRUE)

###############################################
###############################################
###############################################
#correspondencias Zona 3  15% CVB 
###############################################

fisher.test(Zona3,simulate.p.value=TRUE)
Zona3.ca <- CA(Zona3, graph = FALSE)
print(Zona3.ca)
fviz_screeplot(Zona3.ca, addlabels = TRUE, ylim = c(0, 60))
Zona3eig.val <- get_eigenvalue(Zona3.ca)
Zona3eig.val

fviz_ca_biplot(Zona3.ca, col.row = "cos2",
               gradient.cols = c("#FF3E96", "#008B00", "#6A5ACD"), 
               repel = TRUE)
fviz_ca_biplot(Zona3.ca, col.col = "cos2",
               gradient.cols = c("#FF3E96", "#008B00"), 
               repel = TRUE)

##########################################################################################
##########################################################################################



################################################################################
##### 9. AGRUPACION
################################################################################
################################################################################
################################################################################
################################################################################

##Cargar base de datos "Son los datos del archivo ZONA.CA"

################################################################################
#####                           Zona1                                      #####
################################################################################

baseagrupacionZona1 = rbind(Zona1.ca$row$coord,Zona1.ca$col$coord)

dZona1 = dist(baseagrupacionZona1)

cZona1=hclust(dZona1,method="ward.D2")

fviz_nbclust(baseagrupacionZona1, kmeans, method = "silhouette",k.max = 15)

resZona1.hk <-hkmeans(baseagrupacionZona1, 11)

grafagrupacionzona1 = fviz_cluster(resZona1.hk, ellipse.type = "convex", repel = 
                                     TRUE,ggtheme = theme_minimal())

grafagrupacionzona1$layers[[2]]$aes_params$fontface <- "italic"
grafagrupacionzona1

################################################################################
################################################################################
#####                           Zona2                                      #####
################################################################################

baseagrupacionZona2 = rbind(Zona2.ca$row$coord,Zona2.ca$col$coord)

dZona2 = dist(baseagrupacionZona2)

cZona2=hclust(dZona2,method="ward.D2")

fviz_nbclust(baseagrupacionZona2, kmeans, method = "silhouette",k.max = 15)

resZona2.hk <-hkmeans(baseagrupacionZona2, 11)

grafagrupacionzona2 = fviz_cluster(resZona2.hk, ellipse.type = "convex", repel = 
                                     TRUE,ggtheme = theme_minimal())

grafagrupacionzona2$layers[[2]]$aes_params$fontface <- "italic"
grafagrupacionzona2

################################################################################
################################################################################
#####                           Zona3                                      #####
################################################################################

baseagrupacionZona3 = rbind(Zona3.ca$row$coord,Zona3.ca$col$coord)

dZona3 = dist(baseagrupacionZona3)

cZona3=hclust(dZona3,method="ward.D2")

fviz_nbclust(baseagrupacionZona3, kmeans, method = "silhouette",k.max = 15)

resZona3.hk <-hkmeans(baseagrupacionZona3, 11)

grafagrupacionzona3 = fviz_cluster(resZona3.hk, ellipse.type = "convex", repel = 
                                     TRUE,ggtheme = theme_minimal())

grafagrupacionzona3$layers[[2]]$aes_params$fontface <- "italic"
grafagrupacionzona3

################################################################################

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#10. Modelo polinomial grado dos y comparacion entre modelos
#######################################################################################################################

#*#*******************************************************************
#*#**************************************** Archivo para modelos *****
#*#*******************************************************************
#*#***** variable respuesta es la metrica especializacion d *********************
#*#*******************************************************************

# cargar los siguientes archivos. b (hace referencia a la misma matriz al contrario)
library(GGally)

Zona1 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Zona1.txt", row.names=1)
Zona1b <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Zona1b.txt", row.names=1)
Zona2 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Zona2.txt", row.names=1)
Zona2b <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Zona2b.txt", row.names=1)
Zona3 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Zona3.txt", row.names=1)
Zona3b <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Zona3b.txt", row.names=1)

VariablesModelo <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/VariablesModelo.txt", 
                              row.names=1)

especializacionZona1 = dfun(Zona1)
especializacionZona1b = dfun(Zona1b)

especializacionZona2 = dfun(Zona2)
especializacionZona2b = dfun(Zona2b)

especializacionZona3 = dfun(Zona3)
especializacionZona3b = dfun(Zona3b)

dZ1 = especializacionZona1[["d"]]
dZ1b = especializacionZona1b[["d"]]
names(dZ1)
names(dZ1b)

dZ2 = especializacionZona2[["d"]]
dZ2b = especializacionZona2b[["d"]]
names(dZ2)
names(dZ2b)

dZ3 = especializacionZona3[["d"]]
dZ3b = especializacionZona3b[["d"]]
names(dZ3)
names(dZ3b)

################################################################################
############################# Datos de variable respuesta especializacion d ####
################################################################################

EspdAvesyPlantastodaszonas = c(dZ1, dZ1b, dZ2, dZ2b, dZ3, dZ3b)

################################################################################
###### Datos de variables explicativas dimensiones de las correspondencias #####
################################################################################

baseagrupacion = rbind(baseagrupacionZona1, baseagrupacionZona2, baseagrupacionZona3)

################################################################################
############################# Archivo completo  VariablesRedes #################
################################################################################
#Unir con cbind los archivos especializacion, VariablesModelo, cordenadas#######

VariablesRedes = cbind(EspdAvesyPlantastodaszonas, VariablesModelo, baseagrupacion)


#*******************************************************************
# Renombrar variables "el archivo no lee variables con numeros Dim 1 a Dimuno"
#*******************************************************************
names(VariablesRedes)
names(VariablesRedes) = c("Especialización", "Interacciones", "Socios",
                          "Abundancias", "CVB", "Grupo", 
                          "Dimuno", "Dimdos", "Dimtres", "Dimcuatro", "Dimcinco")
names(VariablesRedes)
View(VariablesRedes)

################################################################################
############################# correlogramas ####################################
############################ por defecto pierson ###############################
################################################################################

ggpairs(VariablesRedes, aes(color = Grupo, alpha = 0.5),
        upper = list(continuous = wrap("cor", size = 3.5)))

################################################################################
############################# Modelo 6 regresion lineal multiple ###############
############################# con una variable categorica (grupo) ##############
################################################################################

modelo6=lm(Especialización~Interacciones + Socios+ Abundancias + CVB 
           + Dimuno +Dimdos +Dimtres +Dimcuatro + Dimcinco 
           + factor(Grupo), data=VariablesRedes)

anova(modelo6)
step(modelo6)

################################################################################
############################# Modelo 6 regresion lineal multiple ###############
############################# con una variable categorica (grupo) ##############
################################################################################

modelo6step=lm(Especialización~ Socios+ Abundancias + CVB 
           + Dimuno +Dimdos +Dimtres + Dimcinco 
           + factor(Grupo), data=VariablesRedes)

anova(modelo6step)

################################################################################
############################# Modelo polinomial grado dos        ###############
############################# con una variable categorica (grupo) ##############
################################################################################

mod2 <- lm(Especialización ~ Interacciones+ I(Interacciones^2) + Socios +I(Socios^2)
           + Abundancias + I(Abundancias^2) + CVB + I(CVB^2)
           + Dimuno + I(Dimuno^2)+ Dimdos + I(Dimdos^2) + Dimtres + I(Dimtres^2) 
           + Dimcuatro + I(Dimcuatro^2)+ Dimcinco + I(Dimcinco^2)+ factor(Grupo), data=VariablesRedes)

anova(mod2)

#*******************************************************************
# Distribucion de datos entre las diferentes variables y modelos
#*******************************************************************

ggplot(VariablesRedes,aes(Interacciones, Especialización,color=Grupo)
       )+geom_point()+stat_smooth(method = "lm", formula = y ~ x, size = 1)

ggplot(VariablesRedes,aes(Interacciones, Especialización,color=Grupo)) +
  geom_point() + stat_smooth(method='lm', formula=y~x, size = 2, col='violetred') +
  stat_smooth(method='lm', formula=y~x+I(x^2), size = 2, col='purple') +
  theme_light()

ggplot(VariablesRedes,aes(Socios, Especialización,color=Grupo)) +
  geom_point() + stat_smooth(method='lm', formula=y~x, size = 2, col='violetred') +
  stat_smooth(method='lm', formula=y~x+I(x^2), size = 2, col='purple') +
  theme_light()

ggplot(VariablesRedes,aes(Abundancias, Especialización,color=Grupo)) +
  geom_point() + stat_smooth(method='lm', formula=y~x, size = 2, col='violetred') +
  stat_smooth(method='lm', formula=y~x+I(x^2), size = 2, col='purple') +
  theme_light()

ggplot(VariablesRedes,aes(CVB, Especialización,color=Grupo)) +
  geom_point() + stat_smooth(method='lm', formula=y~x, size = 2, col='violetred') +
  stat_smooth(method='lm', formula=y~x+I(x^2), size = 2, col='purple') +
  theme_light()

ggplot(VariablesRedes,aes(Dimuno, Especialización,color=Grupo)) +
  geom_point() + stat_smooth(method='lm', formula=y~x, size = 2, col='violetred') +
  stat_smooth(method='lm', formula=y~x+I(x^2), size = 2, col='purple') +
  theme_light()

ggplot(VariablesRedes,aes(Dimdos, Especialización,color=Grupo)) +
  geom_point() + stat_smooth(method='lm', formula=y~x, size = 2, col='violetred') +
  stat_smooth(method='lm', formula=y~x+I(x^2), size = 2, col='purple') +
  theme_light()

ggplot(VariablesRedes,aes(Dimtres, Especialización,color=Grupo)) +
  geom_point() + stat_smooth(method='lm', formula=y~x, size = 2, col='violetred') +
  stat_smooth(method='lm', formula=y~x+I(x^2), size = 2, col='purple') +
  theme_light()

ggplot(VariablesRedes,aes(Dimcuatro, Especialización,color=Grupo)) +
  geom_point() + stat_smooth(method='lm', formula=y~x, size = 2, col='violetred') +
  stat_smooth(method='lm', formula=y~x+I(x^2), size = 2, col='purple') +
  theme_light()

ggplot(VariablesRedes,aes(Dimcinco, Especialización,color=Grupo)) +
  geom_point() + stat_smooth(method='lm', formula=y~x, size = 2, col='violetred') +
  stat_smooth(method='lm', formula=y~x+I(x^2), size = 2, col='purple') +
  theme_light()

#*******************************************************************
# CRITERIOS DE COMPARACION DE MODELOS
#*******************************************************************

summary(modelo6)$r.squared
summary(mod2)$r.squared

summary(modelo6)$adj.r.squared
summary(mod2)$adj.r.squared


AIC(modelo6, mod2)
BIC(modelo6, mod2)

par(mfrow=c(1, 2))

plot(modelo6, which=1, caption='Modelo lineal') 
plot(mod2, which=1, caption='Modelo polinomio grado dos')


#*******************************************************************
# TABLA DE ANALISIS DE VARIANZA
#*******************************************************************

library(car)

anova(modelo6, modelo6step,  mod2) 

#*******************************************************************
# VERIFICACIÓN DE SUPUESTOS EN LOS RESIDUALES
#*******************************************************************
summary(mod2)
summary(modelo6)

#*******************************************************************
# NORMALIDAD
#*******************************************************************

par(mfrow=c(1,3))
hist(modelo6$res, col = "springgreen4")
boxplot(modelo6$res, col = "salmon")
qqPlot(modelo6$res) 
shapiro.test(modelo6$res)

par(mfrow=c(1,3))
hist(mod2$res, col = "springgreen4")
boxplot(mod2$res, col = "salmon")
qqPlot(mod2$res) 
shapiro.test(mod2$res)

#******************************************************************
#***p-value debe ser menor o igual al porcentaje del error dispuesto a asumir
#*valor !! e error analizar la normalidad 
#* hipotesis nula asume que los datos tienen distribucion normal y la H1 
#* asume que los datos no tienen distribucion normal
#* 
#* 0.01 p valor rechaza la hipotesis es decir que los datos no son normales****
#******************************************************************

