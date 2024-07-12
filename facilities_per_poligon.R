# Clean the environment
#rm(list=ls())

library("sf")
library("tidyverse")
library('revgeo')
library('ggplot2')
library('dplyr')
library('stringi')
library('geojsonR')
library('here')
library('viridis')
library('ggrepel')
#setwd('~Proyectos_trabajo/modelos-regresion/Datos/19nuevoleon')

# Read layers ---------------------
Escuelas = st_read(dsn = '.', layer = 'ESCUELA') # school
Hospital = st_read(dsn = '.', layer = 'HOSPITAL') # hospitals
Mercado = st_read(dsn = '.', layer = 'MERCADO') # markets
Metro = st_read(dsn = '.', layer = 'LINEA_METRO_P') #subway
Cultural = st_read(dsn = '.', layer = 'CENTRO_CULTURAL') #cultural center
Recreativo = st_read(dsn = '.', layer = 'CENTRO_RECREATIVO') # park
#Leer centroides de los poligonos de infonavit 
infonavit_puntos <- st_read(layer="centroides_ID_DS_año")

#-----------------------------------------------------------------------------------------------------

# Filter by education levels ---------------------------------------------
kinder = Escuelas %>%
  filter(TIPO == 1)

primaria = Escuelas %>%
  filter(TIPO == 2)

secundaria = Escuelas %>%
  filter(TIPO == 3)

prepa = Escuelas %>%
  filter(TIPO == 4)

# Create buffer ------------------------------------
#buffer 2.5 km 
buff = st_buffer(infonavit_puntos, dist = 2500)

# check it works
plot(st_geometry(infonavit_puntos))
plot(st_geometry(buff), add = TRUE)
# See overlap  
plot(buff$geometry, pch=21, cex=0.7, col="blue", bg="gray80")
plot(prepa$geometry, border="gray20", col='purple', add=T)


## Add en que ID_DS's per each facility -------------------------------
kinder_join = st_join(kinder, left = FALSE, buff['ID_DS'])
primaria_join = st_join(primaria, left = FALSE, buff['ID_DS'])
secundaria_join = st_join(secundaria, left = FALSE, buff['ID_DS'])
prepa_join = st_join(prepa, left = FALSE, buff['ID_DS'])
hosp_join = st_join(Hospital, left = FALSE, buff['ID_DS'])
merca_join = st_join(Mercado, left = FALSE, buff['ID_DS'])
cultural_join = st_join(Cultural, left = FALSE, buff['ID_DS'])
recreativo_join = st_join(Recreativo, left = FALSE, buff['ID_DS'])

# Distance to the hospitals ----------------------------------------------------------------
Hospital_dis<- st_transform(Hospital, 32614)

dist_hosp = as.data.frame(st_distance(infonavit_puntos, Hospital_dis))

# Per row choose the shortest 
dist_hosp = apply(dist_hosp, 1, FUN = min)

# Add ID's columns
dist_hosp = as.data.frame(cbind(infonavit_puntos$ID_DS, dist_hosp))%>%
  rename(ID_DS = V1)
  

# Group by ID_DS (poligon) and count number of facilities per poligon -------------------------------
kinder_n = kinder_join%>%
  group_by(ID_DS)%>%
  summarise(n=n())

primaria_n = primaria_join%>%
  group_by(ID_DS)%>%
  summarise(n=n())

secundaria_n = secundaria_join%>%
  group_by(ID_DS)%>%
  summarise(n=n())

prepas_n = prepa_join%>%
  group_by(ID_DS)%>%
  summarise(n=n())


hosp_n = hosp_join%>%
  group_by(ID_DS)%>%
  summarise(n=n())
 

merca_n = merca_join%>%
  group_by(ID_DS)%>%
  summarise(n=n())


cultural_n = cultural_join%>%
  group_by(ID_DS)%>%
  summarise(n=n())

recreativo_n = recreativo_join%>%
  group_by(ID_DS)%>%
  summarise(n=n())

# El tamaño de filas de cada uno de los archivos anteriores corresponde a la cantidad de poligonos que cuentan 
# Con al menos unos de esos equipamientos


# Merge in a whole table ------------------------------------------------------------------------

kinder_n = as.data.frame(kinder_n)
kinder_n$geometry =NULL
kinder_n = rename(kinder_n, Kinder = n)
names(kinder_n)

#kinder-primaria
tabla_primaria = full_join(as.data.frame(kinder_n), as.data.frame(primaria_n), by = 'ID_DS')
names(tabla_primaria)
tabla_primaria = rename(tabla_primaria, Primaria = n)
tabla_primaria$geometry = NULL
names(tabla_primaria)

#secundaria
tabla_secundaria = left_join(as.data.frame(tabla_primaria), as.data.frame(secundaria_n), by = 'ID_DS')
names(tabla_secundaria)
tabla_secundaria = rename(tabla_secundaria, Secundaria = n)
tabla_secundaria$geometry = NULL
names(tabla_secundaria)

#preparatorias
tabla_prepas = left_join(as.data.frame(tabla_secundaria), as.data.frame(prepas_n), by = 'ID_DS')
names(tabla_prepas)
tabla_prepas = rename(tabla_prepas, Media_superior = n)
tabla_prepas$geometry = NULL
names(tabla_prepas)

#Hospitales
tabla_hosp = left_join(as.data.frame(tabla_prepas), as.data.frame(hosp_n), by ='ID_DS')
tabla_hosp = rename(tabla_hosp, Hospitales = n)
tabla_hosp$geometry = NULL
names(tabla_hosp)

#Mercados
tabla_merca = left_join(as.data.frame(tabla_hosp), as.data.frame(merca_n), by ='ID_DS')
names(tabla_merca)
tabla_merca = rename(tabla_merca, Mercados = n)
tabla_merca$geometry = NULL
names(tabla_merca)

#Cultural
tabla_cultural = left_join(as.data.frame(tabla_merca), as.data.frame(cultural_n), by ='ID_DS')
names(tabla_cultural)
tabla_cultural = rename(tabla_cultural, Cultural = n)
tabla_cultural$geometry = NULL
names(tabla_cultural)

#Recreativo
tabla_recreativo = left_join(as.data.frame(tabla_cultural), as.data.frame(recreativo_n), by ='ID_DS')
names(tabla_recreativo)
tabla_recreativo = rename(tabla_recreativo, Recreativo = n)
tabla_recreativo$geometry = NULL
names(tabla_recreativo)

tabla_recreativo[is.na(tabla_recreativo)] <- 0

# Cambiar los espacios en blanco a cero porque son aquellos poligonos que no cuentan con esos tipos de equipamiento 
# El numero de filas corresponde al numero de filas que tiene el df del equipamiento con mas presencia.

# Add distance to hospitals
tabla_recreativo = left_join(as.data.frame(tabla_recreativo), as.data.frame(dist_hosp), by= "ID_DS")


## Save --------------------------------------------
#getwd()
#setwd('C:\\Users\\alexa\\Documents\\Proyectos\\modelos-regresion\\Datos\\Documentos-importantes')
#write.csv(tabla_recreativo, 'Tabla_distancias_ine_idds_0.csv')
