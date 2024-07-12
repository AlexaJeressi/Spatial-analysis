setwd("C:\\Users\\alexa\\Documents\\Proyectos\\modelos-regresion\\Datos\\NUCLEOS")

library("sf")
library("tidyverse")


#######################################################################################################
################  PARTE 1: DISTANCIA DE INFONAVIT A NUCLEOS DE MANCHA URBANA
infonavit_puntos <- st_read(dsn="C:\\Users\\alexa\\Documents\\Proyectos\\modelos-regresion\\Datos\\POLYDOCS", layer="centroides_ID_DS_a?o")
#municipios = st_read(dsn='C:\\Users\\alexa\\OneDrive - Instituto Tecnologico y de Estudios Superiores de Monterrey\\Modelo Infonavit\\18_mun', layer = 'mun_18' )
n_1990 <- st_read(dsn=".", layer="nucleo_1990")
n_1995 <- st_read(dsn= ".", layer="nucleo_1995")
n_2000 <- st_read(dsn = ".", layer = "nucleo_2000")
n_2005 <- st_read(dsn = ".", layer = "nucleo_2005")
n_2010 <- st_read(dsn = ".", layer = "nucleo_2010")
n_2015 <- st_read(ds = ".", layer = "nucleo_2015")
n_2020 <- st_read(ds = ".", layer = "nucleo_2020")
# Revisar proyeccion geografica. Ambos deben de tener la misma proyeccion. 
# en caso de que no la tengan, reproyectar con st_transform.
st_crs(infonavit_puntos)
st_crs(n_1995)
all.equal(st_crs(infonavit_puntos),st_crs(n_1990))  # debe ser TRUE

plot(st_geometry(n_1990))
plot(st_geometry(n_2005), add=TRUE)
plot(st_geometry(n_2020), add=TRUE)
# Medir distancia de cada centroide de colonias al nucleo de 1990.
infonavit_puntos$dist_n1990 <- st_distance(infonavit_puntos, n_1990, by_element = TRUE)
infonavit_puntos$dist_n1995 <- st_distance(infonavit_puntos, n_1995, by_element = TRUE)
infonavit_puntos$dist_n2000 <- st_distance(infonavit_puntos, n_2000, by_element = TRUE)
infonavit_puntos$dist_n2005 <- st_distance(infonavit_puntos, n_2005, by_element = TRUE)
infonavit_puntos$dist_n2010 <- st_distance(infonavit_puntos, n_2010, by_element = TRUE)
infonavit_puntos$dist_n2015 <- st_distance(infonavit_puntos, n_2015, by_element = TRUE)
infonavit_puntos$dist_n2020 <- st_distance(infonavit_puntos, n_2020, by_element = TRUE)


# Guardar como un csv, convertir de espacial a puntos
df <- dplyr::select(as.data.frame(infonavit_puntos), c(ID_DS, dist_n1990,dist_n1995,dist_n2000,
                                                       dist_n2005,dist_n2010,dist_n2015,dist_n2020))
setwd("C:\\Users\\alexa\\Documents\\Proyectos\\modelos-regresion\\Datos\\Datos-importantes")
write.csv(df, "distancia_mancha.csv", row.names=FALSE)



##############################################################################################
################  PARTE 2: DISTANCIA DE INFONAVIT A SUBCENTROS URBANOS
subcenters <- st_read(dsn="C:\\Users\\alexa\\Documents\\Proyectos\\modelos-regresion\\Datos\\POLYDOCS", layer="polydocs")

plot(st_geometry(subcenters))

# Set projection
st_crs(subcenters) <- 4326  # It is geographic coordinates
st_crs(subcenters)

# Transform coordinates to UTM 14 as the input for INFONAVIT shapefile. 
subcenters <- st_transform(subcenters, 32614) 
head(subcenters)

# Split subcenters by topic, make a list. 
list_subc <- split(subcenters, subcenters$LDA)
head(list_subc)
topic_names <- names(list_subc)

# Compute distance from each INFONAVIT centroid to the closest subcenter of each type. 
# Iterate with a nested loop through each infonavit centroid and each type of subcenter:
res_points <- c() # empty vector to save the results
for (j in 1:length(topic_names)){
  for (i in 1:nrow(infonavit_puntos)){
    temp <- st_combine(list_subc[[j]]) %>% st_cast('MULTILINESTRING') %>% st_distance(infonavit_puntos[i,])
    res_points <- c(res_points, temp)
  }
}


plot(st_geometry(list_subc[[1]]))
plot(st_geometry(infonavit_puntos), add=TRUE)


resFinal <- data.frame(matrix(res_points,nrow=nrow(infonavit_puntos)))
head(resFinal)
colnames(resFinal) <- topic_names
final <- cbind(df, resFinal)
head(final)
write.csv(final, "distance_subcenters.csv", row.names=FALSE)



### NUEVAS DISTANCIAS ###-------------------------------------------------------------------------------------------------------------------------
infonavit_puntos <- st_read(dsn="C:\\Users\\alexa\\Documents\\Proyectos\\modelos-regresion\\Datos\\POLYDOCS", layer="centroides_ID_DS_a?o")

setwd("C:\\Users\\alexa\\Documents\\Proyectos\\modelos-regresion\\Datos\\Distancias_geo")

# Leer primer archivo ----
coarse <- st_read("subcentros_coarse.geojson")
coarse = st_transform(coarse, 32614)


## Proceso de para obtener distancias 1 

# Split subcenters by topic, make a list. 
list_subc <- split(coarse, coarse$cls_lbls)
head(list_subc)
topic_names <- names(list_subc)

# Compute distance from each ageb centroid to the closest subcenter of each type. 
# Iterate with a nested loop through each ageb centroid and each type of subcenter:
res_points <- c() # empty vector to save the results

for (j in 1:length(topic_names)){
  for (i in 1:nrow(infonavit_puntos)){
    temp <- st_combine(list_subc[[j]]) %>% 
      st_cast('MULTILINESTRING') %>% 
      st_distance(infonavit_puntos[i,])
    
    res_points <- c(res_points, temp)
  }
}

Final1 <- data.frame(matrix(res_points,nrow=nrow(infonavit_puntos)))
head(Final1)
colnames(Final1) <- topic_names
head(Final1)
write.csv(Final1, "distanciascoarse_infonavit.csv")


# Leer segundo archivo ----
fine <- st_read("subcentros_fine.geojson") 
fine = st_transform(fine, 32614)

list_subc.2 <- split(fine, fine$cls_lbls)
head(list_subc.2)
topic_names.2 <- names(list_subc.2)

# Compute distance from each ageb centroid to the closest subcenter of each type. 
# Iterate with a nested loop through each ageb centroid and each type of subcenter:
res_points.2 <- c() # empty vector to save the results

for (j in 1:length(topic_names.2)){
  for (i in 1:nrow(infonavit_puntos)){
    temp.2 <- st_combine(list_subc.2[[j]]) %>% 
      st_cast('MULTILINESTRING') %>% 
      st_distance(infonavit_puntos[i,])
    
    res_points.2 <- c(res_points.2, temp.2)
  }
}

Final2 <- data.frame(matrix(res_points.2,nrow=nrow(infonavit_puntos)))
head(Final2)
colnames(Final2) <- topic_names.2
head(Final2)

write.csv(Final2, "distanciasfine_infonavit.csv")

Final1 <- cbind(Final1,infonavit_puntos$ID_DS)
names(Final1)
?rename
Final1 = Final1%>%
  rename(Topic_6 = "0",
         Topic_7 = "1",
         Topic_8 = "2",
         Topic_9 = "3",
         ID_DS = "infonavit_puntos$ID_DS")

Final2 <- cbind(Final2, infonavit_puntos$ID_DS)
names(Final2)
Final2 = Final2 %>%
  rename(Topic_0 = "0",
         Topic_1 = "1",
         Topic_2 = "2",
         Topic_3 = "3",
         Topic_4 = "4",
         Topic_5 = "5",
         ID_DS = "infonavit_puntos$ID_DS")

Final_completo <- merge(Final2, Final1, by="ID_DS")

write.csv(Final_completo, "C:\\Users\\alexa\\Documents\\Proyectos\\modelos-regresion\\Datos\\Datos-importantes\\nuevas_distancias.csv", row.names = FALSE)
