library("sf")
library("tidyverse")
library('revgeo')
library('ggplot2')
library('dplyr')
library('stringi')

setwd("C:\\Users\\alexa\\Documents\\Proyectos\\modelos-regresion\\Datos\\Transporte")
## Leer archivos ----------------------------------------------------------------------

 #Identificar los layer de red ferrocarril 
st_layers('Red_ferrocarril.KML')

 #Leer cada layer
Red_ferro = st_read(dsn = 'Red_ferrocarril.KML', layer = 'Infraestructura')
Red_ferro_B = st_read(dsn = 'Red_ferrocarril.KML', layer = 'L?nea B')
Red_ferro_BF = st_read(dsn = 'Red_ferrocarril.KML', layer = 'L?nea BF')
Red_ferro_BM = st_read(dsn = 'Red_ferrocarril.KML', layer = 'L?nea BM')
Red_ferro_F = st_read(dsn = 'Red_ferrocarril.KML', layer = 'L?nea F')
Red_ferro_M_FMX = st_read(dsn = 'Red_ferrocarril.KML', layer = 'L?nea M_FMX')
Red_ferro_KC = st_read(dsn = 'Red_ferrocarril.KML', layer = 'L?nea M_KC')
Red_ferro_MF = st_read(dsn = 'Red_ferrocarril.KML', layer = 'L?nea MF')
Red_ferro_BMA = st_read(dsn = 'Red_ferrocarril.KML', layer = 'BMA')

#Mostar cada layer
plot(st_geometry(Red_ferro))
plot(st_geometry(Red_ferro_B),add = TRUE)
plot(st_geometry(Red_ferro_BF), add=TRUE)
plot(st_geometry(Red_ferro_BM), add=TRUE)
plot(st_geometry(Red_ferro_F), add=TRUE)
plot(st_geometry(Red_ferro_M_FMX), add=TRUE)
plot(st_geometry(Red_ferro_KC), add=TRUE)
plot(st_geometry(Red_ferro_MF), add=TRUE)
plot(st_geometry(Red_ferro_BMA), add=TRUE)

# Leer capas de Rutas de trasporte 
st_layers('Rutas_PIMUS.KML') #Solo cuenta con una capa 
# Guardadar en Rutas la informacion de las rutas de transporte
Rutas = st_read(dsn = 'Rutas_PIMUS.KML', layer = 'SRZMM')
plot(st_geometry(Rutas)) #Ver lineas de transporte 


#Leer capas de la linea del transmetro 
st_layers('Transmetro_linea_3.KML')
#Guardar infoamcion en Trans_l_3
Trans_l_3 = st_read(dsn = 'Transmetro_linea_3.KML', layer = 'TRANSMETROS_ACTUALES') #Solo cuenta con una capa
plot(st_geometry(Trans_l_3))

#Leer capas de transmetro 
st_layers('Transmetros_actuales.KML')
#Guardad informacion de Trans
Trans = st_read(dsn = 'Transmetros_actuales.KML', layer = 'TRANSMETROS_ACTUALES')# Solo es una capa 
plot(st_geometry(Trans))

# Leer archivo del tren garcia y guardarlo en 
Tren_garcia = st_read(dsn = '.', layer = 'Tren_Gracia_Aeropuerto')

plot(st_geometry(Tren_garcia)) #Ver 

#Leer y guardar en metro la informacion de la estaciones del metro 
metro = st_read(dsn = "C:\\Users\\alexa\\Documents\\Proyectos\\modelos-regresion\\Datos\\19nuevoleon", layer ='LINEA_METRO_P')
plot(st_geometry(metro))

#Leer archivo de los centroides de infonavit 
infonavit_puntos <- st_read(dsn="C:\\Users\\alexa\\Documents\\Proyectos\\modelos-regresion\\Datos\\POLYDOCS", layer="centroides_ID_DS_a?o")
st_crs(infonavit_puntos)

##------------------------------------------------------------------------------

# Transform coordinates to UTM 14 as the input for INFONAVIT shapefile----------
Red_ferro<- st_transform(Red_ferro, 32614)
Red_ferro_B<- st_transform(Red_ferro_B, 32614) 
Red_ferro_BF<- st_transform(Red_ferro_BF, 32614)
Red_ferro_BM<- st_transform(Red_ferro_BM, 32614) 
Red_ferro_F<- st_transform(Red_ferro_F, 32614)
Red_ferro_M_FMX<- st_transform(Red_ferro_M_FMX, 32614) 
Red_ferro_KC<- st_transform(Red_ferro_KC, 32614)
Red_ferro_MF<- st_transform(Red_ferro_MF, 32614) 
Red_ferro_BMA<- st_transform(Red_ferro_BMA, 32614) 

Rutas<- st_transform(Rutas, 32614)

Trans_l_3<- st_transform(Trans_l_3, 32614)
Trans<- st_transform(Trans, 32614)

Tren_garcia<- st_transform(Tren_garcia, 32614)

metro = st_transform(metro, 32614)
###----------------------------------------------------------------------------------


## CALCUALR DISTANCIAS --------------------------------------------------------------
# Obtener distancia mas corta a alguna red ferroviaria. 
#Los layer que tengan m?s de una string se debe buscar la distancia mas corta 

  # Calcular distancia 
a = as.data.frame(st_distance(infonavit_puntos, Red_ferro))
  #Por filas, elegir la mas corta 
a = apply(a, 1, FUN = min)

  #Calcular distancias 
b = as.data.frame(st_distance(infonavit_puntos, Red_ferro_B), by_element = TRUE)
c = as.data.frame(st_distance(infonavit_puntos, Red_ferro_BF), by_element = TRUE)
d = as.data.frame(st_distance(infonavit_puntos, Red_ferro_BM), by_element = TRUE)

  #Calcular distancias 
e = as.data.frame(st_distance(infonavit_puntos, Red_ferro_F))
  #Distancia mas corta 
e = apply(e, 1, FUN = min)
  #Distancia 
f = as.data.frame(st_distance(infonavit_puntos, Red_ferro_M_FMX))
f = apply(f, 1, FUN = min) #Mas corta
  #Distancia 
g = as.data.frame(st_distance(infonavit_puntos, Red_ferro_KC))
g = apply(g, 1, FUN = min) #Mas corta
  # Distancia 
h = as.data.frame(st_distance(infonavit_puntos, Red_ferro_MF))
i = as.data.frame(st_distance(infonavit_puntos, Red_ferro_BMA))
i = apply(i, 1, FUN = min) #Mas corta

#Juntar todas la redes 
ferro = cbind(a,b,c,d,e,f,g,h,i)
ferro = apply(ferro, 1, FUN = min) # Encontrar la distacia m?s corta de todos los layers creados antes

# Agregar ID_DS a las distancias 
ferro = as.data.frame(cbind(infonavit_puntos$ID_DS, ferro))

# Cambiar nombre de columnas 
ferro = ferro%>%
  rename(ID_DS = V1,
         distancia_ferro = ferro)%>%
  as.vector()


Rutas <- as.data.frame(st_distance(infonavit_puntos, Rutas))

Rutas = Rutas %>%
  apply( 1, FUN = min)%>%
  as.data.frame()%>%
  cbind(infonavit_puntos$ID_DS)%>%
  rename(distancia_rutas = ".",
         ID_DS = "infonavit_puntos$ID_DS")%>%
  as.vector()


Trans_1_3 <- as.data.frame(st_distance(infonavit_puntos, Trans_l_3))

Trans_1_3 = Trans_1_3%>%
  apply( 1, FUN = min)%>%
  as.data.frame()%>%
  cbind(infonavit_puntos$ID_DS)%>%
  rename(distancia_trans1_3 = ".",
         ID_DS = "infonavit_puntos$ID_DS")%>%
  as.vector()
  


Trans <- as.data.frame(st_distance(infonavit_puntos, Trans))
Trans = Trans%>%
  apply( 1, FUN = min)%>%
  as.data.frame()%>%
  cbind(infonavit_puntos$ID_DS)%>%
  rename(distancia_trans = ".",
         ID_DS = "infonavit_puntos$ID_DS")%>%
  as.vector()
  

Tren_garcia <- as.data.frame(st_distance(infonavit_puntos, Tren_garcia))

Tren_garcia = Tren_garcia%>%
  apply( 1, FUN = min)%>%
  as.data.frame()%>%
  cbind(infonavit_puntos$ID_DS)%>%
  rename(distancia_trengarcia = ".",
         ID_DS = "infonavit_puntos$ID_DS")%>%
  as.vector()



Metro = as.data.frame(st_distance(infonavit_puntos, metro))

Metro = Metro %>%
  apply( 1, FUN = min)%>%
  as.data.frame()%>%
  cbind(infonavit_puntos$ID_DS)%>%
  rename(distancia_metro = ".",
         ID_DS = "infonavit_puntos$ID_DS")%>%
  as.vector()


##------------------------------------------------------------------------------------------------ 

##Agrupar distancias en una sola tabla por ID_DS--------------------------------------------------
distancias_comp = merge(Metro, Tren_garcia)
distancias_comp = merge(distancias_comp, Trans)
distancias_comp = merge(distancias_comp, Trans_1_3)
distancias_comp = merge(distancias_comp, Rutas)
distancias_comp = merge(distancias_comp, ferro)


## Guardar tabla ---------------------------------------------------------------------------------

setwd('C:\\Users\\alexa\\Documents\\Proyectos\\modelos-regresion\\Datos\\Datos-importantes')
write.csv(distancias_comp, 'Trans_idds.csv')
