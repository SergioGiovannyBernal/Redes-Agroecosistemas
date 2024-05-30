---
  title: "Material Suplementario 2023"
author: "Sergio Bernal"
date: "2023-02-03"
output: html_document
---
  REDES DE INTERACCIÓN MUTUALISTAS ENTRE PLANTAS ORNITÓCORAS Y AVES FRUGÍVORAS 
EN UN BOSQUE ANDINO TRANSFORMADO (Material Suplementario)

Sergio Giovanny Bernal (gbernals@udistrital.edu.co), Astrid Viviana Ramírez Castro, Angela Parrado-Rosselli 

Para una mayor abreviacion en los codigos se va utilizar las siguientes convenciones
Sistema1 = primer sistema; 95% de cobertura forestal con un 5% de pastizales 
Sistema2 = segundo sistema; 50% de cobertura forestal con un 50% de área agrosilvopastoril
Sistema3 = tercer sistema; 15% de cobertura forestal con un 85% de área agrosilvopastoril

Material Suplementario 1. Representatividad del Muestreo IAC (Curva de acumulacion de interacciones)


###############################################
###############################################
library(lattice)
library(permute)
library(vegan)

###############################################
###############################################
###############################################
#AIC Sistema1 
###############################################


###############################################
#Paso 1. Cargar archivo 
###############################################

DatosCurvaSistema1 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Archivo completo de redes guardado/CurvaSistema1.txt", header=FALSE)
DatosCurvaSistema1
View(DatosCurvaSistema1)

allSistema1 <- specaccum(DatosCurvaSistema1, method = "random")
plot(allSistema1, ci.type = "poly", col = "antiquewhite", lwd = 2, ci.lty = 0, ylim = c(0, 120), 
     ci.col = "gray", main = " Figura 1. IAC Sistema 1", 
     xlab = "Eventos de muestreo", 
     ylab = "Diferentes interacciones en pares")
boxplot(allSistema1, col = "#AD9024", add = TRUE, pch = "+")

allSistema1

IACSistema1 <- specpool(DatosCurvaSistema1) 
IACSistema1

###############################################################################
#Resultado: chao: 118.2321 y chao.se: 4.966566
#Interpretacion: Riqueza de interacciones observada es de 111 y la esperada es de 118 con
#una desviacion de 4.9.
#allSistema1: representa como la riqueza de interacciones se va aplanando en la curva a partir 
#del muestreo #18
###############################################################################

IAC.CHAO.Zona1 = 111*100/118.2321
IAC.CHAO.Zona1

###############################################################################
#Interpretacion: El muestreo recoje el 93.88313% de la Riqueza de interacciones 
#presente en el sistema 1
###############################################################################


###############################################
###############################################
###############################################
#AIC Sistema2  
###############################################

DatosCurvaSistema2 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Archivo completo de redes guardado/CurvaSistema2.txt", header=FALSE)

allSistema2 <- specaccum(DatosCurvaSistema2, method = "random")
plot(allSistema2, ci.type = "poly", col = "antiquewhite", lwd = 2, ci.lty = 0, ylim = c(0, 250), 
     ci.col = "gray", main = " Figura 2. IAC Sistema 2", 
     xlab = "Eventos de muestreo", 
     ylab = "Diferentes interacciones en pares")
boxplot(allSistema2, col = "#325F8B", add = TRUE, pch = "+")

allSistema2

IACSistema2 <- specpool(DatosCurvaSistema2) 
IACSistema2

###############################################################################
#Resultado: chao: 252.0319 y chao.se: 5.443631
#Interpretacion: Riqueza de interacciones observada es de 241 y la esperada es de 252 con
#una desviacion de 5.4
#allSistema2: representa como la riqueza de interacciones se va aplanando en la curva a partir 
#del muestreo #18
###############################################################################

IAC.CHAO.Sistema2 = 241*100/252.0319
IAC.CHAO.Sistema2

###############################################################################
#Interpretacion: El muestreo recoje el 95.62282% de la Riqueza de interacciones 
#presente en el sistema 2
###############################################################################


###############################################
###############################################
###############################################
#AIC Sistema3 
###############################################

DatosCurvaSistema3 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Archivo completo de redes guardado/CurvaSistema3.txt", header=FALSE)

allSistema3 <- specaccum(DatosCurvaSistema3, method = "random")
plot(allSistema3, ci.type = "poly", col = "antiquewhite", lwd = 2, ci.lty = 0, ylim = c(0, 170), 
     ci.col = "gray", main = " Figura 3. IAC Sistema 3", 
     xlab = "Eventos de muestreo", 
     ylab = "Diferentes interacciones en pares")
boxplot(allSistema3, col = "#E76B49", add = TRUE, pch = "+")

allSistema3

IACSistema3 <- specpool(DatosCurvaSistema3) 
IACSistema3

###############################################################################
#Resultado: chao: 164.8202 y chao.se: 3.411226
#Interpretacion: Riqueza de interacciones observada es de 159 y la esperada es de 164.8 con
#una desviacion de 3.41
#allSistema3: representa como la riqueza de interacciones se va aplanando en la curva a partir 
#del muestreo #18
###############################################################################

IAC.CHAO.Sistema3 = 159*100/164.8202
IAC.CHAO.Sistema3

###############################################################################
#Interpretacion: El muestreo recoje el 96.46876% de la Riqueza de interacciones 
#presente en el Sistema 3
#Para consultar colores en la paleta ir a https://r-charts.com/es/paletas-colores/
###############################################################################

Material Suplementario 2. Graficas ponderadas de las redes de consumo de frutos 

library(networkD3)

###############################################
###############################################
###############################################
#Red Sistema1
###############################################

links1 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Archivo completo de redes guardado/links1.txt")

nodes1 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Archivo completo de redes guardado/nodes1.txt")

sankeyNetwork(Links = links1, Nodes = nodes1, 
              Source = "source", Target = "target",
              Value = "value", NodeID = "especie",
              colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"),
              fontSize= 11, margin = list(left = 100, fontFamily = NULL, 
              nodeWidth = 100, nodePadding = 10, margin = NULL,
              height = NULL, width = NULL, iterations = 50, curvature = 0.5, sinksRight = FALSE))

sankeyNetwork(Links = links1, Nodes = nodes1, 
              Source = "source", Target = "target",
              Value = "value", NodeID = "especie",
              colourScale = FALSE,
              fontSize= 11, margin = list(left = 100, fontFamily = NULL, 
              nodeWidth = 100, nodePadding = 10, margin = NULL,
              height = NULL, width = NULL, iterations = 50, curvature = 0.5, sinksRight = FALSE))


sankeyNetwork (Links=sankey_data$links, Nodes=sankey_data$nodes, 
  Source="source_id", Target="target_id", Value="value", 
  NodeID="name", colourScale=sankey_data$colour_scale,
  fontSize=14, fontFamily="Arial", units=units)

sankeyNetwork(Links=sankey_data$links, Nodes=sankey_data$nodes, 
              Source="source_id", Target="target_id", Value="value", 
              NodeID="name", colourScale=sankey_data$colour_scale,
              fontSize=14, fontFamily="Arial", units=units
)


###############################################
###############################################
###############################################
#Red Sistema2
###############################################

links2 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Archivo completo de redes guardado/links2.txt")

nodes2 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Archivo completo de redes guardado/nodes2.txt")

sankeyNetwork(Links = links2, Nodes = nodes2, 
              Source = "source", Target = "target",
              Value = "value", NodeID = "especie",
              colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"),
              fontSize= 11, margin = list(left = 100, fontFamily = NULL, 
                                          nodeWidth = 100, nodePadding = 10, margin = NULL,
                                          height = NULL, width = NULL, iterations = 50, sinksRight = FALSE))

###############################################
###############################################
###############################################
#Red Sistema3
###############################################

links3 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Archivo completo de redes guardado/links3.txt")

nodes3 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Archivo completo de redes guardado/nodes3.txt")

sankeyNetwork(Links = links3, Nodes = nodes3, 
              Source = "source", Target = "target",
              Value = "value", NodeID = "especie",
              colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"),
              fontSize= 11, margin = list(left = 100, fontFamily = NULL, 
                                          nodeWidth = 100, nodePadding = 10, margin = NULL,
                                          height = NULL, width = NULL, iterations = 50, sinksRight = FALSE))



#*******************************************************************
# test de fisher en la red "Matrices de contingencias"
#*******************************************************************

library(stats)

###############################################
###############################################
###############################################
#fisher Sistema1
###############################################

fisher.test(Sistema1,simulate.p.value=TRUE)

###############################################
###############################################
###############################################
#fisher Sistema2
###############################################

fisher.test(Sistema2,simulate.p.value=TRUE)

###############################################
###############################################
###############################################
#fisher Sistema3
###############################################

fisher.test(Sistema3,simulate.p.value=TRUE)

#Interpretacion: calculamos el valor p de la matriz de contingencias con 2000 redes al azar 
#encontrando que las redes no presentan interacciones al azar sino que si hay preferencia o . 
#interacciones que presentan una direccion o preferencia dada la dieta de las aves 
#las razones son desconocidas pero se debe empiezar a sugerir explicaciones.


#############################################################################################################

Material Suplementario 3. Metricas de especializacion H2

library(bipartite)

###############################################
###############################################
###############################################
#Metrica Especializacion H2 Sistema1
###############################################

Sistema1 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Archivo completo de redes guardado/Sistema1.txt", row.names=1)

H2.Sistema1 =networklevel(Sistema1, index = 'H2')
Shannondiversity.Sistema1 = networklevel(Sistema1, index = 'Shannon diversity')
NODF.Sistema1 = networklevel(Sistema1, index = 'NODF')
connectance.Sistema1 =networklevel(Sistema1, index = 'connectance')

MetricasSistema1 = rbind(NODF.Sistema1, H2.Sistema1, connectance.Sistema1, Shannondiversity.Sistema1)
MetricasSistema1

###############################################
###############################################
###############################################
#Metrica Especializacion H2 Sistema2
###############################################

Sistema2 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Archivo completo de redes guardado/Sistema2.txt", row.names=1)

H2.Sistema2 =networklevel(Sistema2, index = 'H2')
Shannondiversity.Sistema2 = networklevel(Sistema2, index = 'Shannon diversity')
NODF.Sistema2 = networklevel(Sistema2, index = 'NODF')
connectance.Sistema2 =networklevel(Sistema2, index = 'connectance')

MetricasSistema2 = rbind(NODF.Sistema2, H2.Sistema2, connectance.Sistema2, Shannondiversity.Sistema2)
MetricasSistema2

 
###############################################
###############################################
###############################################
#Metrica Especializacion H2 Sistema3
###############################################

Sistema3 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Archivo completo de redes guardado/Sistema3.txt", row.names=1)

H2.Sistema3 =networklevel(Sistema3, index = 'H2')
Shannondiversity.Sistema3 = networklevel(Sistema3, index = 'Shannon diversity')
NODF.Sistema3 = networklevel(Sistema3, index = 'NODF')
connectance.Sistema3 =networklevel(Sistema3, index = 'connectance')

MetricasSistema3 = rbind(NODF.Sistema3, H2.Sistema3, connectance.Sistema3, Shannondiversity.Sistema3)
MetricasSistema3

#############################################################################################################
#Especialización H2´: Indica que tan diversas son esas interacciones 
#considerando toda la matriz y cada especie.
#############################################################################################################

#############################################################################################################
#Nulo Especialización H2´
#############################################################################################################
#############################################################################################################

webs.names <- c("Zona1", "Zona2", "Zona3")

webs <- lapply(c("Zona1", "Zona2", "Zona3"), get)

names(webs) <- webs.names

net.metrics.NODF <- lapply(webs, networklevel, index = 'NODF') 

net.metrics.H2 <- lapply(webs, networklevel, index = 'H2')

net.metrics.connectance <- lapply(webs, networklevel, index = 'connectance')

net.metrics.Shannon.diversity <- lapply(webs, networklevel, index = 'Shannon diversity')

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

# Null distribution function for Shannon diversity - calculates the network Shannon diversity 
#for each null (using a particular null method) for each site 

net.null.Shannon.diversity = function(nulls){
  net.null.metric <- list()
  for (i in 1:length(nulls)) {
    net.null.metric[[i]] = do.call('rbind', 
                                   lapply(nulls[[i]], networklevel, index = 'Shannon diversity'))
  }
  names(net.null.metric) <- webs.names
  return(net.null.metric)
}

# Time consuming step!

r2d.NODF <- net.null.NODF(net.nulls.r2d)

r2d.H2 <- net.null.H2(net.nulls.r2d)

r2d.connectance <- net.null.connectance(net.nulls.r2d)

r2d.Shannon.diversity <- net.null.Shannon.diversity(net.nulls.r2d)

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

# Function that perform z-score calculation of Shannon diversity using the observed and null networks
Shannon.diversity.zscore = function(nulltype){
  net.Shannon.diversity.zscore <- list() 
  for(i in 1:length(net.metrics.Shannon.diversity)){
    net.Shannon.diversity.zscore[[i]] = net.zscore(net.metrics.Shannon.diversity[[i]]['Shannon diversity'], 
                                                   nulltype[[i]][ ,'Shannon diversity'])
  }
  names(net.Shannon.diversity.zscore) <- webs.names
  return(net.Shannon.diversity.zscore)
}

r2d.NODF.zscore <- NODF.zscore(r2d.NODF)
r2d.H2.zscore <- H2.zscore(r2d.H2)
r2d.connectance.zscore <- connectance.zscore(r2d.connectance)
r2d.Shannon.diversity.zscore <- Shannon.diversity.zscore(r2d.Shannon.diversity)

#Por último, calculamos el valor p de dos caras para la importancia de la 
#propiedad de la red para NODF, H2 y robustness. 
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

# Add the p-values to our connectance results
r2d.test.connectance <- add.pvalues(r2d.connectance.zscore)

# Add the p-values to our Shannon.diversity results
r2d.test.Shannon.diversity <- add.pvalues(r2d.Shannon.diversity.zscore)

# Print the NODF, H2 and connectance results

print(r2d.test.NODF)
print(r2d.test.H2)
print(r2d.test.connectance)
print(r2d.test.Shannon.diversity)

##########################################################################################
##########################################################################################
##########################################################################################

Material Suplementario 4. Modularidad

###############################################
###############################################
###############################################
#Modularidad Sistema1
#
###############################################

Modularidad.Sistema1 = computeModules(Sistema1, method="Beckett",
                                      deep = FALSE, deleteOriginalFiles = TRUE,
                                      steps = 1000000, tolerance = 1e-10, experimental 
                                      = FALSE, forceLPA=FALSE)

plotModuleWeb(Modularidad.Sistema1)

nullsModularidad.Sistema1 <- nullmodel(Sistema1, N=100, method="r2d")

modulesSistema1.nulls <- sapply(nullsModularidad.Sistema1, computeModules)

likeSistema1.nulls <- sapply(modulesSistema1.nulls, function(x) x@likelihood)

(z <- (Modularidad.Sistema1@likelihood - mean(likeSistema1.nulls))/sd(likeSistema1.nulls))

###############################################
###############################################
###############################################
#Modularidad Sistema2
###############################################

Modularidad.Sistema2 = computeModules(Sistema2, method="Beckett",
                                      deep = FALSE, deleteOriginalFiles = TRUE,
                                      steps = 1000000, tolerance = 1e-10, experimental 
                                      = FALSE, forceLPA=FALSE)

plotModuleWeb(Modularidad.Sistema2)

nullsModularidad.Sistema2 <- nullmodel(Sistema2, N=100, method="r2d")

modulesSistema2.nulls <- sapply(nullsModularidad.Sistema2, computeModules)

likeSistema2.nulls <- sapply(modulesSistema2.nulls, function(x) x@likelihood)

(z <- (Modularidad.Sistema2@likelihood - mean(likeSistema2.nulls))/sd(likeSistema2.nulls))

###############################################
###############################################
###############################################
#Modularidad Sistema3
###############################################

Modularidad.Sistema3 = computeModules(Sistema3, method="Beckett",
                                      deep = FALSE, deleteOriginalFiles = TRUE,
                                      steps = 1000000, tolerance = 1e-10, experimental 
                                      = FALSE, forceLPA=FALSE)

plotModuleWeb(Modularidad.Sistema3)

nullsModularidad.Sistema3 <- nullmodel(Sistema3, N=100, method="r2d")

modulesSistema3.nulls <- sapply(nullsModularidad.Sistema3, computeModules)

likeSistema3.nulls <- sapply(modulesSistema3.nulls, function(x) x@likelihood)

(z <- (Modularidad.Sistema3@likelihood - mean(likeSistema3.nulls))/sd(likeSistema3.nulls))

##########################################################################################

##########################################################################################
##########################################################################################

Material Suplementario 5. Anidamiento

#Metrica de anidamiento nest.smdm
##########################################################################################
#Compute modularity : https://github.com/gabrielmfelix/Restricted-Null-Model/blob/master/CompoundTest.R
##########################################################################################
##########################################################################################
##########################################################################################

##########################################################################################
#Anidamiento Sistema1
##########################################################################################
##########################################################################################
##########################################################################################

Mod <- bipartite::computeModules(Sistema1)

#Recover the partitions
Part <- bipartite::module2constraints(Mod)
row.Part <- Part[1:nrow(Sistema1)]
col.Part <- Part[(nrow(Sistema1)+1):(nrow(Sistema1)+ncol(Sistema1))]

#Test for the significance of modularity with a Monte Carlo procedure

#Generate randomized matrices
nulls <- nullmodel(Sistema1, N=9, method="r2d")

#Calculate the modularity of the randomized matrices
mod.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(mod.nulls, function(x) x@likelihood)

#Calculate the z-score of the randomized distribution
(z <- (Mod@likelihood - mean(like.nulls))/sd(like.nulls))

#Plot the observed modularity value against the distribution of randomized values
plot(density(like.nulls), xlim=c(min((Mod@likelihood), min(like.nulls)), max((Mod@likelihood), max(like.nulls))), 
     main="Observed vs. randomized")
abline(v=(Mod@likelihood), col="red", lwd=2)    

#Estimate the P-value
mean(like.nulls)
sd(like.nulls)
Mod@likelihood
praw <- sum(like.nulls>(Mod@likelihood)) / length(like.nulls)
ifelse(praw > 0.5, 1-praw, praw)

############### 3. NESTEDNESS ANALYSIS ############### 

#Calculate the desired nestedness metric (here WNODA) for the original network.
obs <- unlist(bipartite::nest.smdm(x = Sistema1, 
                                   constraints = Part, #Input the modular structured recovered from step 2
                                   weighted = T, #By considering the edge weights, you are choosing WNODA
                                   decreasing = "abund"))

#Check the scores
obs

############### 4. RESTRICTED NULL MODEL ANALYSIS ############### 

#Calculate the same nestedness metric for all randomized networks
null <- sapply(nulls, function(x) bipartite::nest.smdm(x = x, constraints = Part, weighted = T, decreasing = "abund"))
WNODA.null <- unlist(null[3,])
WNODAsm.null <- unlist(null[8,])
WNODAdm.null <- unlist(null[9,])

#Plot the observed nestedness value against the distribution of randomized values
par(mfrow = c(1,3))
plot(density(WNODA.null), xlim=c(min(obs[3], min(WNODA.null)), max(obs[3], max(WNODA.null))), 
     main="Observed vs. randomized", xlab = "WNODA matrix")
abline(v=obs[3], col="red", lwd=2)    
plot(density(WNODAsm.null), xlim=c(min(obs[8], min(WNODAsm.null)), max(obs[8], max(WNODAsm.null))), 
     main="Observed vs. randomized", xlab = "WNODAsm matrix")
abline(v=obs[8], col="red", lwd=2)    
plot(density(WNODAdm.null), xlim=c(min(obs[9], min(WNODAdm.null)), max(obs[9], max(WNODAdm.null))), 
     main="Observed vs. randomized", xlab = "WNODAdm matrix")
abline(v=obs[9], col="red", lwd=2)    

#Estimate the P-values

#Nestedness in th entire network
praw.WNODA <- sum(WNODA.null>obs[3]) / length(WNODA.null)
p.WNODA <- ifelse(praw.WNODA > 0.5, 1- praw.WNODA, praw.WNODA)    # P-value
p.WNODA

#Nestedness within the modules
praw.WNODAsm <- sum(WNODAsm.null>obs[8]) / length(WNODAsm.null)
p.WNODAsm <- ifelse(praw.WNODAsm > 0.5, 1- praw.WNODAsm, praw.WNODAsm)    # P-value
p.WNODAsm

#Nestedness between the modules
praw.WNODAdm <- sum(WNODAdm.null>obs[9]) / length(WNODAdm.null)
p.WNODAdm <- ifelse(praw.WNODAdm > 0.5, 1- praw.WNODAdm, praw.WNODAdm)    # P-value
p.WNODAdm


############### 5. PLOTTING THE NETWORK ############### 

par(mfrow = c(1,1))

#Sort the matrix in a way that facilitates visualizing the compound topology
Sistema1.comp <- bipartite::sortmatrix(matrix = Sistema1, topology = "compound", sort_by = "weights", row_partitions = row.Part, col_partitions = col.Part)

#Assign colors for the modules
modcol <- rainbow((length(unique(Part))), alpha=1)

#Plot the matrix
plotmatrix(Sistema1.comp$matrix, 
           row_partitions = Sistema1.comp$row_partitions, 
           col_partitions = Sistema1.comp$col_partitions, 
           border = T,
           binary = F,
           modules_colors = modcol,
           within_color = modcol, 
           between_color = "lightgrey")

#############################################################################################################

##########################################################################################
#Anidamiento Sistema2
##########################################################################################
##########################################################################################
##########################################################################################

Mod <- bipartite::computeModules(Sistema2)

#Recover the partitions
Part <- bipartite::module2constraints(Mod)
row.Part <- Part[1:nrow(Sistema2)]
col.Part <- Part[(nrow(Sistema2)+1):(nrow(Sistema2)+ncol(Sistema2))]

#Test for the significance of modularity with a Monte Carlo procedure

#Generate randomized matrices
nulls <- nullmodel(Sistema2, N=9, method="r2d")

#Calculate the modularity of the randomized matrices
mod.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(mod.nulls, function(x) x@likelihood)

#Calculate the z-score of the randomized distribution
(z <- (Mod@likelihood - mean(like.nulls))/sd(like.nulls))

#Plot the observed modularity value against the distribution of randomized values
plot(density(like.nulls), xlim=c(min((Mod@likelihood), min(like.nulls)), max((Mod@likelihood), max(like.nulls))), 
     main="Observed vs. randomized")
abline(v=(Mod@likelihood), col="red", lwd=2)    

#Estimate the P-value
mean(like.nulls)
sd(like.nulls)
Mod@likelihood
praw <- sum(like.nulls>(Mod@likelihood)) / length(like.nulls)
ifelse(praw > 0.5, 1-praw, praw)

############### 3. NESTEDNESS ANALYSIS ############### 

#Calculate the desired nestedness metric (here WNODA) for the original network.
obs <- unlist(bipartite::nest.smdm(x = Sistema2, 
                                   constraints = Part, #Input the modular structured recovered from step 2
                                   weighted = T, #By considering the edge weights, you are choosing WNODA
                                   decreasing = "abund"))

#Check the scores
obs

############### 4. RESTRICTED NULL MODEL ANALYSIS ############### 

#Calculate the same nestedness metric for all randomized networks
null <- sapply(nulls, function(x) bipartite::nest.smdm(x = x, constraints = Part, weighted = T, decreasing = "abund"))
WNODA.null <- unlist(null[3,])
WNODAsm.null <- unlist(null[8,])
WNODAdm.null <- unlist(null[9,])

#Plot the observed nestedness value against the distribution of randomized values
par(mfrow = c(1,3))
plot(density(WNODA.null), xlim=c(min(obs[3], min(WNODA.null)), max(obs[3], max(WNODA.null))), 
     main="Observed vs. randomized", xlab = "WNODA matrix")
abline(v=obs[3], col="red", lwd=2)    
plot(density(WNODAsm.null), xlim=c(min(obs[8], min(WNODAsm.null)), max(obs[8], max(WNODAsm.null))), 
     main="Observed vs. randomized", xlab = "WNODAsm matrix")
abline(v=obs[8], col="red", lwd=2)    
plot(density(WNODAdm.null), xlim=c(min(obs[9], min(WNODAdm.null)), max(obs[9], max(WNODAdm.null))), 
     main="Observed vs. randomized", xlab = "WNODAdm matrix")
abline(v=obs[9], col="red", lwd=2)    

#Estimate the P-values

#Nestedness in th entire network
praw.WNODA <- sum(WNODA.null>obs[3]) / length(WNODA.null)
p.WNODA <- ifelse(praw.WNODA > 0.5, 1- praw.WNODA, praw.WNODA)    # P-value
p.WNODA

#Nestedness within the modules
praw.WNODAsm <- sum(WNODAsm.null>obs[8]) / length(WNODAsm.null)
p.WNODAsm <- ifelse(praw.WNODAsm > 0.5, 1- praw.WNODAsm, praw.WNODAsm)    # P-value
p.WNODAsm

#Nestedness between the modules
praw.WNODAdm <- sum(WNODAdm.null>obs[9]) / length(WNODAdm.null)
p.WNODAdm <- ifelse(praw.WNODAdm > 0.5, 1- praw.WNODAdm, praw.WNODAdm)    # P-value
p.WNODAdm


############### 5. PLOTTING THE NETWORK ############### 

par(mfrow = c(1,1))

#Sort the matrix in a way that facilitates visualizing the compound topology
Sistema2.comp <- bipartite::sortmatrix(matrix = Sistema2, topology = "compound", sort_by = "weights", row_partitions = row.Part, col_partitions = col.Part)

#Assign colors for the modules
modcol <- rainbow((length(unique(Part))), alpha=1)

#Plot the matrix
plotmatrix(Sistema2.comp$matrix, 
           row_partitions = Sistema2.comp$row_partitions, 
           col_partitions = Sistema2.comp$col_partitions, 
           border = T,
           binary = F,
           modules_colors = modcol,
           within_color = modcol, 
           between_color = "lightgrey")

#############################################################################################################


##########################################################################################
#Anidamiento Sistema3
##########################################################################################
##########################################################################################
##########################################################################################

Mod <- bipartite::computeModules(Sistema3)

#Recover the partitions
Part <- bipartite::module2constraints(Mod)
row.Part <- Part[1:nrow(Sistema3)]
col.Part <- Part[(nrow(Sistema3)+1):(nrow(Sistema3)+ncol(Sistema3))]

#Test for the significance of modularity with a Monte Carlo procedure

#Generate randomized matrices
nulls <- nullmodel(Sistema3, N=9, method="r2d")

#Calculate the modularity of the randomized matrices
mod.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(mod.nulls, function(x) x@likelihood)

#Calculate the z-score of the randomized distribution
(z <- (Mod@likelihood - mean(like.nulls))/sd(like.nulls))

#Plot the observed modularity value against the distribution of randomized values
plot(density(like.nulls), xlim=c(min((Mod@likelihood), min(like.nulls)), max((Mod@likelihood), max(like.nulls))), 
     main="Observed vs. randomized")
abline(v=(Mod@likelihood), col="red", lwd=2)    

#Estimate the P-value
mean(like.nulls)
sd(like.nulls)
Mod@likelihood
praw <- sum(like.nulls>(Mod@likelihood)) / length(like.nulls)
ifelse(praw > 0.5, 1-praw, praw)

############### 3. NESTEDNESS ANALYSIS ############### 

#Calculate the desired nestedness metric (here WNODA) for the original network.
obs <- unlist(bipartite::nest.smdm(x = Sistema3, 
                                   constraints = Part, #Input the modular structured recovered from step 2
                                   weighted = T, #By considering the edge weights, you are choosing WNODA
                                   decreasing = "abund"))

#Check the scores
obs

############### 4. RESTRICTED NULL MODEL ANALYSIS ############### 

#Calculate the same nestedness metric for all randomized networks
null <- sapply(nulls, function(x) bipartite::nest.smdm(x = x, constraints = Part, weighted = T, decreasing = "abund"))
WNODA.null <- unlist(null[3,])
WNODAsm.null <- unlist(null[8,])
WNODAdm.null <- unlist(null[9,])

#Plot the observed nestedness value against the distribution of randomized values
par(mfrow = c(1,3))
plot(density(WNODA.null), xlim=c(min(obs[3], min(WNODA.null)), max(obs[3], max(WNODA.null))), 
     main="Observed vs. randomized", xlab = "WNODA matrix")
abline(v=obs[3], col="red", lwd=2)    
plot(density(WNODAsm.null), xlim=c(min(obs[8], min(WNODAsm.null)), max(obs[8], max(WNODAsm.null))), 
     main="Observed vs. randomized", xlab = "WNODAsm matrix")
abline(v=obs[8], col="red", lwd=2)    
plot(density(WNODAdm.null), xlim=c(min(obs[9], min(WNODAdm.null)), max(obs[9], max(WNODAdm.null))), 
     main="Observed vs. randomized", xlab = "WNODAdm matrix")
abline(v=obs[9], col="red", lwd=2)    

#Estimate the P-values

#Nestedness in th entire network
praw.WNODA <- sum(WNODA.null>obs[3]) / length(WNODA.null)
p.WNODA <- ifelse(praw.WNODA > 0.5, 1- praw.WNODA, praw.WNODA)    # P-value
p.WNODA

#Nestedness within the modules
praw.WNODAsm <- sum(WNODAsm.null>obs[8]) / length(WNODAsm.null)
p.WNODAsm <- ifelse(praw.WNODAsm > 0.5, 1- praw.WNODAsm, praw.WNODAsm)    # P-value
p.WNODAsm

#Nestedness between the modules
praw.WNODAdm <- sum(WNODAdm.null>obs[9]) / length(WNODAdm.null)
p.WNODAdm <- ifelse(praw.WNODAdm > 0.5, 1- praw.WNODAdm, praw.WNODAdm)    # P-value
p.WNODAdm


############### 5. PLOTTING THE NETWORK ############### 

par(mfrow = c(1,1))

#Sort the matrix in a way that facilitates visualizing the compound topology
Sistema3.comp <- bipartite::sortmatrix(matrix = Sistema3, topology = "compound", sort_by = "weights", row_partitions = row.Part, col_partitions = col.Part)

#Assign colors for the modules
modcol <- rainbow((length(unique(Part))), alpha=1)

#Plot the matrix
plotmatrix(Sistema3.comp$matrix, 
           row_partitions = Sistema3.comp$row_partitions, 
           col_partitions = Sistema3.comp$col_partitions, 
           border = T,
           binary = F,
           modules_colors = modcol,
           within_color = modcol, 
           between_color = "lightgrey")

#############################################################################################################

##########################################################################################

Material Suplementario 6. SIMULACION DE EXTINCIONES

#participant="higher" Nivel trofico superior representado por PLANTAS en columnas
#participant="lower" Nivel trofico inferior representado por AVES en filas 
#method="degree" elimina las especies mayormente conectadas
#method="random" elimina las especies de manera aleatoria
#Detalles
#La función escala las secuencias de extinción a valores entre 0 y 1 para cada 
# participante. El eje x del gráfico presenta la proporción de participantes 
#exterminados, mientras que el eje y representa la proporción de extinciones secundarias. 
#Dado que estas curvas suelen seguir una función hiperbólica (ver ejemplos en Memmott et al. 2004), 
#esto se ajusta a los datos.
#En la actualidad, sólo una función de tipoestá ajustado (usando nls), es decir, sin compensación.
#Si bien, por lo general, esta función proporciona muy buenos ajustes, verifique el gráfico y 
#juzgue usted mismo. 
#El ajuste de esta función simple hace que su parámetro 'a' sea una medida de la vulnerabilidad 
#a la extinción. Cuanto más graduales sean las extinciones secundarias, 
#menor será el valor absoluto de 'a'. O, dicho de otro modo, los valores absolutos grandes 
#de 'a' indican una mortandad muy abrupta, indicativa de una alta redundancia inicial en la red.

#Referencias
#Memmott, J., Waser, NM y Price, MV (2004) Tolerancia de las redes de 
#polinización a la extinción de especies. Actas de la Royal Society B 271 , 2605--2611

##########################################################################################

############################################################################
#Extinciones Sistema2
############################################################################
######################################################

Sistema1 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Archivo completo de redes guardado/Sistema1.txt", row.names=1)

######################################################
#Extinciones primarias y aleatorias de aves
######################################################

(exSistema1AvesAleatorio <- second.extinct(Sistema1, participant="lower", method="random", nrep=100, 
                      details=TRUE))
(exSistema1AvesAleatorio <- second.extinct(Sistema1, participant="lower", method="random", nrep=100, 
                      details=FALSE))
slope.bipartite(exSistema1AvesAleatorio)

######################################################
#Extinciones primarias y aleatorias de plantas
######################################################

(exSistema1plantasAleatorio <- second.extinct(Sistema1, participant="higher", method="random", nrep=100, 
                      details=TRUE))
(exSistema1plantasAleatorio <- second.extinct(Sistema1, participant="higher", method="random", nrep=100, 
                      details=FALSE))
slope.bipartite(exSistema1plantasAleatorio)

######################################################
#Extinciones primarias de aves mayormente conectadas
######################################################

(exSistema1AvesMasInteracion <- second.extinct(Sistema1, participant="lower", method="degree", nrep=100, 
                                           details=TRUE))
(exSistema1AvesMasInteracion <- second.extinct(Sistema1, participant="lower", method="degree", nrep=100, 
                                           details=FALSE))
slope.bipartite(exSistema1AvesMasInteracion)

######################################################
#Extinciones primarias de plantas mayormente conectadas
######################################################

(exSistema1plantasMasInteracion <- second.extinct(Sistema1, participant="higher", method="degree", nrep=100, 
                                              details=TRUE))
(exSistema1plantasMasInteracion <- second.extinct(Sistema1, participant="higher", method="degree", nrep=100, 
                                              details=FALSE))
slope.bipartite(exSistema1plantasMasInteracion)

############################################################################
#Extinciones Sistema2
############################################################################
######################################################

Sistema2 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Archivo completo de redes guardado/Sistema2.txt", row.names=1)

######################################################
#Extinciones primarias y aleatorias de aves
######################################################

(exSistema2AvesAleatorio <- second.extinct(Sistema2, participant="lower", method="random", nrep=100, 
                                           details=TRUE))
(exSistema2AvesAleatorio <- second.extinct(Sistema2, participant="lower", method="random", nrep=100, 
                                           details=FALSE))
slope.bipartite(exSistema2AvesAleatorio)

######################################################
#Extinciones primarias y aleatorias de plantas
######################################################

(exSistema2plantasAleatorio <- second.extinct(Sistema2, participant="higher", method="random", nrep=100, 
                                              details=TRUE))
(exSistema2plantasAleatorio <- second.extinct(Sistema2, participant="higher", method="random", nrep=100, 
                                              details=FALSE))
slope.bipartite(exSistema2plantasAleatorio)

######################################################
#Extinciones primarias de aves mayormente conectadas
######################################################

(exSistema2AvesMasInteracion <- second.extinct(Sistema2, participant="lower", method="degree", nrep=100, 
                                               details=TRUE))
(exSistema2AvesMasInteracion <- second.extinct(Sistema2, participant="lower", method="degree", nrep=100, 
                                               details=FALSE))
slope.bipartite(exSistema2AvesMasInteracion)

######################################################
#Extinciones primarias de plantas mayormente conectadas
######################################################

(exSistema2plantasMasInteracion <- second.extinct(Sistema2, participant="higher", method="degree", nrep=100, 
                                                  details=TRUE))
(exSistema2plantasMasInteracion <- second.extinct(Sistema2, participant="higher", method="degree", nrep=100, 
                                                  details=FALSE))
slope.bipartite(exSistema2plantasMasInteracion)


############################################################################
#Extinciones Sistema3
############################################################################
######################################################

Sistema3 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Archivo completo de redes guardado/Sistema3.txt", row.names=1)

######################################################
#Extinciones primarias y aleatorias de aves
######################################################

(exSistema3AvesAleatorio <- second.extinct(Sistema3, participant="lower", method="random", nrep=100, 
                                           details=TRUE))
(exSistema3AvesAleatorio <- second.extinct(Sistema3, participant="lower", method="random", nrep=100, 
                                           details=FALSE))
slope.bipartite(exSistema3AvesAleatorio)

######################################################
#Extinciones primarias y aleatorias de plantas
######################################################

(exSistema3plantasAleatorio <- second.extinct(Sistema3, participant="higher", method="random", nrep=100, 
                                              details=TRUE))
(exSistema3plantasAleatorio <- second.extinct(Sistema3, participant="higher", method="random", nrep=100, 
                                              details=FALSE))
slope.bipartite(exSistema3plantasAleatorio)

######################################################
#Extinciones primarias de aves mayormente conectadas
######################################################

(exSistema3AvesMasInteracion <- second.extinct(Sistema3, participant="lower", method="degree", nrep=100, 
                                               details=TRUE))
(exSistema3AvesMasInteracion <- second.extinct(Sistema3, participant="lower", method="degree", nrep=100, 
                                               details=FALSE))
slope.bipartite(exSistema3AvesMasInteracion)

######################################################
#Extinciones primarias de plantas mayormente conectadas
######################################################

(exSistema3plantasMasInteracion <- second.extinct(Sistema3, participant="higher", method="degree", nrep=100, 
                                                  details=TRUE))
(exSistema3plantasMasInteracion <- second.extinct(Sistema3, participant="higher", method="degree", nrep=100, 
                                                  details=FALSE))
slope.bipartite(exSistema3plantasMasInteracion)

######################################################
######################################################
######################################################

Material Suplementario 8. CORRESPONDENCIAS

library(factoextra)
library(FactoMineR)
library(igraph)
library(gplots)
library(ape)


###############################################
###############################################
###############################################
#correspondencias Sistema1
###############################################

fisher.test(Zona1,simulate.p.value=TRUE)
Zona1.ca <- CA(Zona1, graph = FALSE)
fviz_ca_biplot(Zona1.ca, col.row = "cos2",
               gradient.cols = c("#FF3E96", "#008B00", "#6A5ACD"), 
               repel = TRUE)

###############################################
###############################################
###############################################
#correspondencias Sistema2
###############################################

fisher.test(Zona2,simulate.p.value=TRUE)
Zona2.ca <- CA(Zona2, graph = FALSE)
fviz_ca_biplot(Zona2.ca, col.row = "cos2",
               gradient.cols = c("#FF3E96", "#008B00", "#6A5ACD"), 
               repel = TRUE)

###############################################
###############################################
###############################################
#correspondencias Sistema3
###############################################

fisher.test(Zona3,simulate.p.value=TRUE)
Zona3.ca <- CA(Zona3, graph = FALSE)
fviz_ca_biplot(Zona3.ca, col.row = "cos2",
               gradient.cols = c("#FF3E96", "#008B00", "#6A5ACD"), 
               repel = TRUE)

##########################################################################################
##########################################################################################


Material Suplementario 9. Agrupacion utilizando las correspondencias

##Cargar base de datos "Son los datos del archivo ZONA.CA"

################################################################################
##### primer sistema; 95% de cobertura forestal con un 5% de pastizales    #####
################################################################################

baseagrupacionZona1 = rbind(Zona1.ca$row$coord,Zona1.ca$col$coord)

dZona1 = dist(baseagrupacionZona1)

cZona1=hclust(dZona1,method="ward.D2")

fviz_nbclust(baseagrupacionZona1, kmeans, method = "silhouette",k.max = 15)

resZona1.hk <-hkmeans(baseagrupacionZona1, 6)

grafagrupacionzona1 = fviz_cluster(resZona1.hk, ellipse.type = "convex", repel = 
                                     TRUE,ggtheme = theme_minimal())

grafagrupacionzona1$layers[[2]]$aes_params$fontface <- "italic"
grafagrupacionzona1

################################################################################
################################################################################
##### segundo sistema; 50% de cobertura forestal con un 50% de área agrosilvopastoril #####
################################################################################

baseagrupacionZona2 = rbind(Zona2.ca$row$coord,Zona2.ca$col$coord)

dZona2 = dist(baseagrupacionZona2)

cZona2=hclust(dZona2,method="ward.D2")

fviz_nbclust(baseagrupacionZona2, kmeans, method = "silhouette",k.max = 15)

resZona2.hk <-hkmeans(baseagrupacionZona2, 6)

grafagrupacionzona2 = fviz_cluster(resZona2.hk, ellipse.type = "convex", repel = 
                                     TRUE,ggtheme = theme_minimal())

grafagrupacionzona2$layers[[2]]$aes_params$fontface <- "italic"
grafagrupacionzona2

################################################################################
################################################################################
##### tercer sistema; 15% de cobertura forestal con un 85% de área agrosilvopastoril #####
################################################################################

baseagrupacionZona3 = rbind(Zona3.ca$row$coord,Zona3.ca$col$coord)

dZona3 = dist(baseagrupacionZona3)

cZona3=hclust(dZona3,method="ward.D2")

fviz_nbclust(baseagrupacionZona3, kmeans, method = "silhouette",k.max = 15)

resZona3.hk <-hkmeans(baseagrupacionZona3, 2)

grafagrupacionzona3 = fviz_cluster(resZona3.hk, ellipse.type = "convex", repel = 
                                     TRUE,ggtheme = theme_minimal())

grafagrupacionzona3$layers[[2]]$aes_params$fontface <- "italic"
grafagrupacionzona3

################################################################################
################################################################################

Material Suplementario 10. Modelo polinomial grado dos y comparacion entre modelos

#Zona1 = primer sistema; 95% de cobertura forestal con un 5% de pastizales 
#Zona2 = segundo sistema; 50% de cobertura forestal con un 50% de área agrosilvopastoril
#Zona3 = tercer sistema; 15% de cobertura forestal con un 85% de área agrosilvopastoril

#
#Archivo para modelos

#variable respuesta es la metrica especializacion d 

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


################################################################################
# Renombrar variables "el archivo no lee variables con numeros Dim 1 a Dimuno"
################################################################################
names(VariablesRedes)
names(VariablesRedes) = c("Especialización", "Interacciones", "Socios",
                          "Abundancias", "CVB", "Grupo", 
                          "Dimuno", "Dimdos", "Dimtres", "Dimcuatro", "Dimcinco")
names(VariablesRedes)

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
           + factor(Grupo), Sistema1=VariablesRedes)

anova(modelo6)
step(modelo6)

################################################################################
############################# Modelo 6 regresion lineal multiple ###############
############################# con una variable categorica (grupo) ##############
################################################################################

modelo6step=lm(Especialización~ Socios+ Abundancias + CVB 
               + Dimuno +Dimdos +Dimtres + Dimcinco 
               + factor(Grupo), Sistema1=VariablesRedes)

anova(modelo6step)
################################################################################
############################# Modelo polinomial grado dos        ###############
############################# con una variable categorica (grupo) ##############
################################################################################

mod2 <- lm(Especialización ~ Interacciones+ I(Interacciones^2) + Socios +I(Socios^2)
           + Abundancias + I(Abundancias^2) + CVB + I(CVB^2)
           + Dimuno + I(Dimuno^2)+ Dimdos + I(Dimdos^2) + Dimtres + I(Dimtres^2) 
           + Dimcuatro + I(Dimcuatro^2)+ Dimcinco + I(Dimcinco^2)+ factor(Grupo), Sistema1=VariablesRedes)

anova(mod2)

################################################################################
# Distribucion de datos entre las diferentes variables y modelos
################################################################################

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

################################################################################
# CRITERIOS DE COMPARACION DE MODELOS
################################################################################

summary(modelo6)$r.squared
summary(mod2)$r.squared

summary(modelo6)$adj.r.squared
summary(mod2)$adj.r.squared


AIC(modelo6, mod2)
BIC(modelo6, mod2)

par(mfrow=c(1, 2))

plot(modelo6, which=1, caption='Modelo lineal') 
plot(mod2, which=1, caption='Modelo polinomio grado dos')


################################################################################
# TABLA DE ANALISIS DE VARIANZA
################################################################################

library(car)

anova(modelo6, modelo6step,  mod2) 

################################################################################
# VERIFICACIÓN DE SUPUESTOS EN LOS RESIDUALES
################################################################################

summary(mod2)
summary(modelo6)

################################################################################
# NORMALIDAD
################################################################################

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

################################################################################
#***p-value debe ser menor o igual al porcentaje del error dispuesto a asumir
#*valor !! e error analizar la normalidad 
#* hipotesis nula asume que los datos tienen distribucion normal y la H1 
#* asume que los datos no tienen distribucion normal
#* 
#* 0.01 p valor rechaza la hipotesis es decir que los datos no son normales****
################################################################################
```

Material Suplementario 11. Anexos matrices 

#Zona1 = primer sistema; 95% de cobertura forestal con un 5% de pastizales 
#Zona2 = segundo sistema; 50% de cobertura forestal con un 50% de área agrosilvopastoril
#Zona3 = tercer sistema; 15% de cobertura forestal con un 85% de área agrosilvopastoril

DatoscurvaZona1 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/DatoscurvaZona1.txt", 
                              header=FALSE)
DatoscurvaZona2 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/DatoscurvaZona2.txt", 
                              header=FALSE)
DatoscurvaZona3 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/DatoscurvaZona3.txt", 
                              header=FALSE)

links1 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/links1.txt")

nodes1 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/nodes1.txt")

links2 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/links2.txt")

nodes2 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/nodes2.txt")

links3 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/links3.txt")

nodes3 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/nodes3.txt")

Zona1 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Zona1.txt", row.names=1)
Zona1
Zona2 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Zona2.txt", row.names=1)
Zona2
Zona3 <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Zona3.txt", row.names=1)
Zona3

Zona1b <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Zona1.txt", row.names=1)
Zona1b
Zona2b <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Zona2.txt", row.names=1)
Zona2b
Zona3b <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/Zona3.txt", row.names=1)
Zona3b

VariablesModelo <- read.delim("C:/Users/User/Desktop/Archivos R ago 29/VariablesModelo.txt", 
                              row.names=1)

Nuevo COdigo 



