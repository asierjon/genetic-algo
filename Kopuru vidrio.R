# Load data and basic housekeeping
library("data.table")
# data <- fread("warehouse/Contenedores_vidrio_con_publicidad.csv", dec=".") # especificar tipo decimal
link <- "https://datos.madrid.es/egob/catalogo/300232-1-Contenedores-vidrio-publicidad.csv"
data <- fread(link, dec=".") # especificar tipo decimal
summary(data)

link2 <- "https://datos.madrid.es/egobfiles/MANUAL/300276/Contenedores_varios.csv"
data2 <- fread(link2, header = T, sep = ";", dec = ",", blank.lines.skip = T, encoding = "Latin-1")
summary(data2)
data2 <- data2[!is.na(data2$`COORDENADA X`),]
summary(data2)

#install.packages("proj4")
library(proj4)

xytolatlonConvert <- function(x,y) {
  proj4string <- "+proj=utm +zone=30T +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
  pj <- project(data.frame(x=x, y=y), proj4string, inverse=TRUE)
  latlon <- data.frame(lat=pj$y, lon=pj$x)
  return(latlon)
}

data2 <- cbind(xytolatlonConvert(data2$`COORDENADA X`,data2$`COORDENADA Y`),data2) #convertir UTM xy a lat lon
#names(data2) <- c("lat", "lon", "Distrito", "Tipo Contenedor")
t(t(table(data2$Distrito))) #hay que limpiar algunos códigos de distrito
t(t(table(gsub("[[:punct:]].*","",data2$Distrito)))) #comprobamos que nos sirve esta regexp
data2$Distrito <- gsub("[[:punct:]].*","",data2$Distrito) #limpiamos

# Unificar datasets. Añadimos info de código y descripción de distrito a ambos dataset
t(t(table(data$DISTRITO)))
distritos <- data.frame(Distrito = c(2, 21, 11, 1, 5, 7, 15, 8, 16, 10, 9, 13, 3, 4, 20, 6, 12, 19, 18), DistritoDesc = c("ARGANZUELA", "BARAJAS", "CARABANCHEL", "CENTRO", "CHAMARTIN", "CHAMBERI", "CIUDAD LINEAL", "FUENCARRAL - EL PARDO", "HORTALEZA", "LATINA", "MONCLOA - ARAVACA", "PUENTE DE VALLECAS", "RETIRO", "SALAMANCA", "SAN BLAS", "TETUAN", "USERA", "VICALVARO", "VILLA DE VALLECAS"))
distritos <- distritos[order(distritos$Distrito),]

data <- merge(data, distritos, by.x = "DISTRITO", by.y = "DistritoDesc")
names(data)[1] <- "DistritoDesc"
data <- data[,c(11,10,13,1:9,12)]
data <- data[order(Distrito),]
names(data)[1:2] <- c("lat","lon") # both tables must share same names to merge

data2 <- merge(data2, distritos, by.x = "Distrito", by.y = "Distrito")
data2 <- as.data.table(data2[,c(2,3,1,17,4:16)])

dataAll <- rbind(data[,c(1:4)], data2[,c(1:4)])

# Show markers on map
#install.packages("leaflet")
library(leaflet)
library(dplyr)
#library(leaflet.extras)
# LEAFLET Funciones accesorias
#   addMarkers(clusterOptions = markerClusterOptions())

dataFiltrado <- dataAll[sample(1:nrow(dataAll),500),]

getColor <- function(data) {
  sapply(data$DistritoDesc, function(x) {
    if(x == "CENTRO") {
      "green"
    } else {
      "lightgray"
    }
  })
}
colores <- as.vector(getColor(dataAll))
icons <- awesomeIcons(icon = 'wine-bottle', iconColor = 'black', library = 'fa', markerColor = colores)

# Ruta, distancia y mapa
map <- leaflet(dataAll[DistritoDesc == "CENTRO",]) %>% addTiles() %>%
  setView(lng = -3.703544, lat = 40.417030, zoom = 12)  %>%
  addAwesomeMarkers(~lon, ~lat, icon=~icons, label=~as.character(DistritoDesc),clusterOptions = markerClusterOptions())
map

# Reclasificamos los distritos de algunos datos erróneos del distrito 1, 2, 7 y 8
map <- leaflet(dataAll[Distrito == 10,]) %>% addTiles() %>%
  setView(lng = -3.703544, lat = 40.417030, zoom = 12)  %>%
  addAwesomeMarkers(~lon, ~lat, icon=~icons, label=~as.character(lat))#,clusterOptions = markerClusterOptions())
#map
# Reclasificamos los distritos de algunos datos erróneos del distrito 1, 2, 7 y 8
distritos
dataAll[lat <= 40.40 & Distrito == 1,4] <- "LATINA"
dataAll[lat <= 40.40 & Distrito == 1,3] <- "10"
dataAll[lat >= 40.43 & Distrito == 1,4] <- "TETUAN"
dataAll[lat >= 40.43 & Distrito == 1,3] <- "6"
dataAll[lat <= 40.38 & Distrito == 2,4] <- "VILLA DE VALLECAS"
dataAll[lat <= 40.38 & Distrito == 2,3] <- "18"
dataAll[lat <= 40.40 & Distrito == 7,4] <- "LATINA"
dataAll[lat <= 40.40 & Distrito == 7,3] <- "10"
dataAll[lat <= 40.41 & Distrito == 8,4] <- "LATINA"
dataAll[lat <= 40.41 & Distrito == 8,3] <- "10"

# Funciones calculadoras de rutas
# Ruta de punto A a punto B, con gestión de fallo del servidor, librería OSRM
#install.packages("osrm")
library(osrm)
routeRetry <- function(lat1, lon1, lat2, lon2) {
  r <- NULL
  attempt <- 0
  while( is.null(r) && attempt < 1000 ) {
    tryCatch(
      r <- osrmRoute(src = c("A", lon1, lat1), dst = c("B", lon2, lat2), overview = "full", sp = T),
      error = function(e){},
      warning = function(w){}
    )
    attempt <- attempt + 1
  } 
  if(is.null(r)) {paste0("Error connecting to OSRM, A(", lat1, ",", lon1, ") to B(", lat2, ",", lon2,")")}
  else r
}

# Calcular 1 recorrido, guardar distancia y representar en mapa con un color aleatorio, para usar con rutaMulti
#install.packages("randomcoloR")
library(randomcoloR)
rutaAB <- function(LatLon1, LatLon2){
  route <- routeRetry(lat1 = LatLon1[,1], lon1 = LatLon1[,2], lat2 = LatLon2[,1], lon2 = LatLon2[,2])
  nodes <- route@lines[[1]]@Lines[[1]]@coords[,c(2,1)]
  distancia <<- distancia + route$distance
  nodes <- nodes[,c(2,1)]
  # names(nodes) <- c("lat", "lon")
  map <<- map %>%
    # addPolylines(
    #   data = nodes,
    #   opacity = 0.4,
    #   weight = 6,
    #   group = "route1",
    #   color = "black") %>%
    addPolylines(
      data = nodes,
      opacity = 0.7,
      weight = 6,
      group = "route1",
      color = randomColor())
  return(map)
}


# Calcular rutas entre todos los puntos de una lista de coordenadas y devolver distancia, utiliza función rutaAB con un bucle
# inicializar mapa en Madrid Centro
mapMadridInit <- function(lat = 40.417030, lon = -3.703544, zoom = 14) { 
  map <<- leaflet(data) %>% addTiles() %>%
    setView(lng = lon, lat = lat, zoom)
  map
}

# calcular rutas
rutaMulti <- function(paradas){
  markerInicio <- makeAwesomeIcon(icon= 'flag', markerColor = 'green', iconColor = 'white')
  markerFin <- makeAwesomeIcon(icon= 'flag', markerColor = 'red', iconColor = 'white')
  markerDefault <- makeAwesomeIcon(icon= 'trash', markerColor = 'blue', iconColor = 'white')
  puntoInicio <- paradas[1,]
  puntoFin <- paradas[nrow(paradas),]
  mapMadridInit()
  distancia <<- 0
  for (i in 1:(nrow(paradas)-1)) {
    posicionA <- paradas[i]
    posicionB <- paradas[i+1]
    rutaAB(posicionA, posicionB)
    map <<- map %>%
      addAwesomeMarkers(
        lng= ~as.numeric(paradas[i+1][,2]), lat = ~as.numeric(paradas[i+1][,1]),
        label=paste0('Contenedor ',(i+1)),
        icon = markerDefault)
  }
  map <<- map %>%
    addAwesomeMarkers(
      lng= ~as.numeric(puntoInicio[,2]), lat = ~as.numeric(puntoInicio[,1]),
      label='Inicio',
      icon = markerInicio) %>%
    addAwesomeMarkers(
      lng= ~as.numeric(puntoFin[,2]), lat = ~as.numeric(puntoFin[,1]),
      label='Fin',
      icon = markerFin)
  return(list(map = map,distancia = distancia))
}
rutaMulti(paradas[1:100])



# Cálculo de prueba de las rutas
paradas <- dataAll[dataAll$DistritoDesc == "CENTRO",c("lat", "lon")] # filtrar dataset de paradas
paradas <- dataAll[,c("lat", "lon")] # no filtrar dataset de paradas
paradas <- paradas[sample(nrow(paradas)),] #Aleatorización
solucionAleatoria <- rutaMulti(paradas = paradas)

# ALGORITMO GREEDY HEURISTIC
## Calcular matriz de distancias (lat-lon) y fijando el 1er punto ordenar los siguientes 1 a 1 por cercanía
#install.packages("geosphere")
## 0 inicializar
library(geosphere)
soluciones <- NULL
paradas2 <- paradas[sample(nrow(paradas)),]
paradasListaA <- paradas2[,c(2,1)]
## 1 retirar la primera parada de la lista inicial y pasarla a la lista final
paradasListaB <- paradasListaA[1]
paradasListaA <- paradasListaA[2:nrow(paradasListaA)]
while (nrow(paradasListaA) >0) {
  ## 2 elegir la parada más cercana desde la última parada de la lista final hasta las paradas que quedan en la lista inicial
  distancias <- distm(paradasListaB[nrow(paradasListaB)],paradasListaA)
  paradaSiguiente <- which(distancias == min(distancias))
  ## 3 colocar la siguiente parada en la última posición de la lista final y retirarla de la inicial
  paradasListaB <- rbind(paradasListaB,paradasListaA[paradaSiguiente])
  paradasListaA <- paradasListaA[-paradaSiguiente]
  ## 4 repetir 2-3
}
#soluciones <- c(soluciones, list(rutaMulti(paradasListaB[,c(2,1)])))
solucionGreedy <- rutaMulti(paradasListaB[,c(2,1)]) #algoritmo greedy
solucionGreedy
solucionOSRM <- osrmTrip(data.frame(id = c(1:nrow(paradas2)),paradas2[,c(2,1)])) #algoritmo OSRM
solucionOSRM[[1]]$summary$distance
solucionGreedy$distancia
solucionOSRM[[1]]$summary$distance-solucionGreedy$distancia #comparación
## END ALGORITMO NAIVE

# ALGORITMO CLÚSTER
plot(paradas)
# Ward Hierarchical Clustering
paradas2 <- paradas[sample(nrow(paradas)),]
d <- dist(paradas2, method = "manhattan") # distance matrix
fit <- hclust(d, method="ward.D") 
solucionCluster1 <- rutaMulti(paradas2[fit$order,])
solucionCluster1
plot(fit) # display dendogram

# ALGORITMO CLÚSTER BIS: calculate clusters, order clusters by proximity, go through each cluster and then to the next one
library(cluster)
paradas2 <- paradas
paradas2 <- paradas[sample(nrow(paradas)),]
fit <- kmeans(paradas2, 7)
# reorder clusters by proximity
centers <- fit$centers
d <- dist(centers, method = "manhattan") # distance matrix
fitCenters <- hclust(d, method="ward.D")
centersOrder <- fitCenters$order
orden <- NULL
paradasClust <- cbind(paradas2,fit$cluster)
for (cluster in centersOrder) {
  stops <- paradasClust[V2 == cluster,c(1,2)]
  if(nrow(stops) > 1) {
    d <- dist(stops, method = "manhattan") # distance matrix
    fit2 <- hclust(d, method="ward.D")
    orden <- rbind(orden,stops[fit2$order,])
  } else {
    orden <- rbind(orden,stops)
  }
}
solucionCluster2 <- rutaMulti(orden)

clusplot(paradas2, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
# ALGORITMO GENÉTICO
# Funciones
dropLast <- function(listaDeListas){
  lista <- lista[c(1:(length(lista))-1)]  
}

# Comparar una nueva lista con una lista de listas. Devuelve FALSO si la nueva no existe ya en el listado grande
sampleFind <- function(newSample, savedSamples) {
  repeated <- F
  i <- 1
  while ((i <= length(savedSamples) && (repeated == F))) {
    repeated <- identical(newSample,savedSamples[i])
    i <- i + 1
  }
  repeated
}

# Valores de una lista entre 0 y 1
normalizarValoresInv <- function(valores) {
  (max(valores)-valores)/(max(valores)-min(valores))
}

# Convertir distancia máxima de una matriz de distancias en línea recta en distancia en ciudad, en el peor de los casos: h^2 = 2*c^2. Además suponer que todas las rutas son igual que la más larga para calcular la máxima distancia posible.
distanciaCiudadMax <- function(puntos){
  2*max(distm(puntos[,c(2,1)]))/2^0.5*(nrow(puntos)-1)
}

# 1 Crear población de partida
# inicializar
now1 <- Sys.time()
nPermutaciones <- 100
permutaciones <- NULL
i = 1
# calcular samples 1 a 1 observando que no exista ya. Con reemplazo.
while (i <= nPermutaciones) {
  permutacionNueva <- list(paradas[sample(nrow(paradas)),])
  if (!sampleFind(permutacionNueva,permutaciones)) {
    permutaciones <- c(permutaciones,permutacionNueva)
    # Programáticamente: (más abajo lo hago con sapply)
    # nueva <- rutaMulti(permutaciones[[i]])
    # distanciasPermutaciones <- rbind(distanciasPermutaciones, nueva$distancia)
    # mapasPermutaciones <- c(mapasPermutaciones, list(nueva$map))
    i <- i + 1
  }
}

# Calcular y almacenar rutas, mapas, distancias
permutacionesRutas <- NULL
distanciasPermutaciones <- NULL
mapasPermutaciones <- NULL

permutacionesRutas <- t(sapply((1:nPermutaciones), function(x){rutaMulti(permutaciones[[x]])})) # rutas
permutacionesDistancias <- sapply((1:nPermutaciones),function(x){permutacionesRutas[[x,2]]}) # distancias
permutacionesMapas <- t(t(sapply((1:nPermutaciones),function(x){permutacionesRutas[x,1]}))) # mapas
now2 <- Sys.time()
timeDifference <- now2 - now1
timeDifference
timeDifference / nPermutaciones

# Cálculo de puntuación
# distanciaMax <- distanciaCiudadMax(paradas)
# fitnessPermutaciones <- distanciaMax - distanciasPermutaciones # dar puntuación

# 2 Torneo & mating pool
nContendientes <- 20 # siempre par!!!!
contendientes <- order(permutacionesDistancias, decreasing=FALSE)[1:nContendientes]
padres <- permutaciones[contendientes]
padresDistancias <- permutacionesDistancias[contendientes]
padresMapas <- permutacionesMapas[contendientes]
# padres <- sample(padres) # shuffle

# 3 Crossover
## Inicialización
herencia <- 0.5
cromosomaSize <- nrow(paradas)
herenciaSize <- as.integer(herencia * cromosomaSize)

## Elegir posición de corte
posicionCorte <- sample(cromosomaSize,1) - 1

## Construir plantillas de herencia binaria
plantillaHerencia <- if((posicionCorte + herenciaSize) <= cromosomaSize) {
  c(rep(0,posicionCorte), 
    rep(1,herenciaSize), 
    rep(0,cromosomaSize - herenciaSize - posicionCorte))
} else {
  c(rep(1,posicionCorte - herenciaSize - 1), 
    rep(0,cromosomaSize - herenciaSize), 
    rep(1, 2 * herenciaSize - posicionCorte + 1))
}
plantillaHerenciaInv <- (plantillaHerencia - 1) * (-1)

## Repetir
g <- 1 # generaciones
nGeneraciones <- 10
distanciaAcumulada <- NULL

while (g <= nGeneraciones) {
  generacionNueva <- c()
  
  i <- 1 # núm de padre de esta generación
  while (i <= nContendientes) {
    
    ## Cruce
    ### Hijo 1
    padre1 <- padres[[i]]
    padre2 <- padres[[i+1]]
    
    padre1herencia <- padre1 * plantillaHerencia
    
    a <- data.frame(t(padre1 * plantillaHerenciaInv))
    b <- data.frame(t(padre2))
    padre2herencia <- data.table(t(a[order(match(a,b))]))
    nullCoord <- data.frame(0,0)
    names(nullCoord) <- names(paradas)
    padre2herencia <- if((posicionCorte + herenciaSize) < cromosomaSize) {
      rbind(
        if(posicionCorte > 0) {padre2herencia[1:posicionCorte]},
        nullCoord[rep(1,(cromosomaSize - herenciaSize - 1)),],
        padre2herencia[(posicionCorte + 1):(cromosomaSize - herenciaSize)]
      )    
    } else {
      rbind(
        nullCoord[rep(1,herenciaSize - cromosomaSize + posicionCorte),],
        padre2herencia[1:(cromosomaSize - herenciaSize)],
        nullCoord[rep(1,(cromosomaSize - posicionCorte)),]
      ) 
    }
    
    hijo1 <- padre1herencia + padre2herencia
    
    ### Hijo 2
    padre1 <- padres[[i+1]]
    padre2 <- padres[[i]]
    
    padre1herencia <- padre1 * plantillaHerencia
    
    a <- data.frame(t(padre1 * plantillaHerenciaInv))
    b <- data.frame(t(padre2))
    padre2herencia <- data.table(t(a[order(match(a,b))]))
    nullCoord <- data.frame(0,0)
    names(nullCoord) <- names(paradas)
    padre2herencia <- if((posicionCorte + herenciaSize) < cromosomaSize) {
      rbind(
        if(posicionCorte > 0) {padre2herencia[1:posicionCorte]},
        nullCoord[rep(1,(cromosomaSize - herenciaSize - 1)),],
        padre2herencia[(posicionCorte + 1):(cromosomaSize - herenciaSize)]
      )    
    } else {
      rbind(
        nullCoord[rep(1,herenciaSize - cromosomaSize + posicionCorte),],
        padre2herencia[1:(cromosomaSize - herenciaSize)],
        nullCoord[rep(1,(cromosomaSize - posicionCorte)),]
      ) 
    }
    
    hijo2 <- padre1herencia + padre2herencia
    
    ### Añadir a listado de nueva generación
    generacionNueva <- c(generacionNueva,list(hijo1))
    generacionNueva <- c(generacionNueva,list(hijo2))
    
    # ### Generar y almacenar rutas, distancias y mapas de la generación
    # nuevoHijo <- rutaMulti(hijo1)
    # distanciasHijos <- rbind(distanciasHijos, nuevoHijo$distancia)
    # mapasHijos <- c(mapasHijos, list(nuevoHijo$map))
    # nuevoHijo <- rutaMulti(hijo2)
    # distanciasHijos <- rbind(distanciasHijos, nuevoHijo$distancia)
    # mapasHijos <- c(mapasHijos, list(nuevoHijo$map))
    
    ### repetir
    i <- i + 2
  }
  
  # Cálculo de resultados de la nueva generación
  generacionNuevaRutas <- c()
  generacionNuevaDistancias <- c()
  generacionNuevaMapas <- c()
  
  generacionNuevaRutas <- t(sapply((1:nContendientes), function(x){rutaMulti(generacionNueva[[x]])})) # rutas
  generacionNuevaDistancias <- sapply((1:nContendientes),function(x){generacionNuevaRutas[[x,2]]}) # distancias
  generacionNuevaMapas <- t(t(sapply((1:nContendientes),function(x){generacionNuevaRutas[x,1]}))) # mapas
  
  # 4 Elite
  # Selección de la siguiente generación de PADRES (élite)
  a <- sort(rbind(permutacionesDistancias,generacionNuevaDistancias),decreasing = F)[1:nContendientes]
  b <- rbind(permutacionesDistancias,generacionNuevaDistancias)
  seleccionados <- match(a,b)
  padres <- append(padres,generacionNueva)[seleccionados]
  padresDistancias <- rbind(permutacionesDistancias,generacionNuevaDistancias)[seleccionados]
  padresMapas <- append(permutacionesMapas,generacionNuevaMapas)[seleccionados]
  
  # Guardar puntuación y pasar a siguiente generación
  distanciaAcumulada <- c(distanciaAcumulada,sum(padresDistancias))
  g <- g + 1
}


install.packages("osrm")
library(osrm)
route <- osrmRoute(src = c("A", -3.713787, 40.40968), dst = c("B", -3.707193, 40.40698), overview = "full", sp = T)
route$distance
routeParadas <- data.table(route@lines[[1]]@Lines[[1]]@coords[,c(2,1)])
route2 <- rutaMulti(routeParadas)
rutaAB()
padres[[20]][1:2]
route3 <- viarouteRetry(lat1 = padres[[20]][1,1], lon1 = padres[[20]][1,2], lat2 = padres[[20]][2,1], lon2 = padres[[20]][2,2], T, 5, F)
route3$routes[[1]]$legs[[1]]$distance



nodes <- route@lines[[1]]@Lines[[1]]@coords[,c(2,1)]
nodes <- as.matrix(decode_geom(route3$routes[[1]]$geometry, 5))

map5 <- leaflet(data) %>%
  addTiles() %>%
  setView(lng = -3.703544, lat = 40.417030, zoom = 14)
map5 <- map5 %>%
  addPolylines(
    data = nodes[,c(2,1)],
    opacity = 1,
    weight = 6,
    group = "route1",
    color = "black") %>%
  addPolylines(
    data = nodes[,c(2,1)],
    opacity = 0.8,
    weight = 6,
    group = "route1",
    color = "blue")
map2
map3
map4
map5
# 5 Mutation