library(tidyverse)
library(lubridate)
library(janitor)
library(ggplot2)

file.choose()
#Se ponen las direcciones de los archivos dentro de variables
path11 <- "C:\\Users\\blejo\\OneDrive\\Documentos\\Proyectos R\\ProyectoBici\\Proyecto Final Bici\\202311-divvy-tripdata.csv"
path12 <- "C:\\Users\\blejo\\OneDrive\\Documentos\\Proyectos R\\ProyectoBici\\Proyecto Final Bici\\202312-divvy-tripdata.csv"
path1 <- "C:\\Users\\blejo\\OneDrive\\Documentos\\Proyectos R\\ProyectoBici\\Proyecto Final Bici\\202401-divvy-tripdata.csv"
path2 <- "C:\\Users\\blejo\\OneDrive\\Documentos\\Proyectos R\\ProyectoBici\\Proyecto Final Bici\\202402-divvy-tripdata.csv"
path3 <- "C:\\Users\\blejo\\OneDrive\\Documentos\\Proyectos R\\ProyectoBici\\Proyecto Final Bici\\202403-divvy-tripdata.csv"
path4 <- "C:\\Users\\blejo\\OneDrive\\Documentos\\Proyectos R\\ProyectoBici\\Proyecto Final Bici\\202404-divvy-tripdata.csv"
path5 <- "C:\\Users\\blejo\\OneDrive\\Documentos\\Proyectos R\\ProyectoBici\\Proyecto Final Bici\\202405-divvy-tripdata.csv"
path6 <- "C:\\Users\\blejo\\OneDrive\\Documentos\\Proyectos R\\ProyectoBici\\Proyecto Final Bici\\202406-divvy-tripdata.csv"
path7 <- "C:\\Users\\blejo\\OneDrive\\Documentos\\Proyectos R\\ProyectoBici\\Proyecto Final Bici\\202407-divvy-tripdata.csv"
path8 <- "C:\\Users\\blejo\\OneDrive\\Documentos\\Proyectos R\\ProyectoBici\\Proyecto Final Bici\\202408-divvy-tripdata.csv"
path9 <- "C:\\Users\\blejo\\OneDrive\\Documentos\\Proyectos R\\ProyectoBici\\Proyecto Final Bici\\202409-divvy-tripdata.csv"
path10 <- "C:\\Users\\blejo\\OneDrive\\Documentos\\Proyectos R\\ProyectoBici\\Proyecto Final Bici\\202410-divvy-tripdata.csv"

# se carga cada archivo en un dataframe
datacruda11 <- read_csv(path11)
datacruda12 <- read_csv(path12)
datacruda1 <- read_csv(path1)
datacruda2 <- read_csv(path2)
datacruda3 <- read_csv(path3)
datacruda4 <- read_csv(path4)
datacruda5 <- read_csv(path5)
datacruda6 <- read_csv(path6)
datacruda7 <- read_csv(path7)
datacruda8 <- read_csv(path8)
datacruda9 <- read_csv(path9)
datacruda10 <- read_csv(path10)

# Se unen todos los dataframes en uno solo (para luego limpiar los datos) porque comparten las mismas columnas
# solo que son de distintas fechas.

data_cruda_total <- rbind(datacruda12,datacruda11,datacruda10,datacruda9,datacruda8,datacruda7,datacruda6,datacruda5,datacruda4,datacruda3,datacruda2,datacruda1)
dim(data_cruda_total)
names(data_cruda_total)

#Aca empieza la limpieza de los datos

# "est_ubi_crudo" es una tabla con todos los nombres de las estaciones y sus ubicaciones pero crudo, con datos repetidos y faltantes
est_ubi_crudo <- select(data_cruda_total, start_station_name, start_lat, start_lng, end_lat, end_lng)

# aca se pone en la variable "estac_ubi" los datos no repetidos según la columna "start_station_name" y se mantiene la otra columna sin comparar porque no hace falta.
estac_ubi <- distinct(est_ubi_crudo, start_station_name, .keep_all = TRUE)


#Esta prueba se hizo con una pequeña tabla para ver si la función hacia lo que necesitabamos.
#datos_prueba <- head(estac_ubi, n=10L)
#tabla_dist <- mutate(datos_prueba, distancia = dis_travel_func(datos_prueba$start_lat,datos_prueba$start_lng,datos_prueba$end_lat,datos_prueba$end_lng))

#Se crea una función que mida la duracion de los viajes

func_dist <- function(lat1, lng1, lat2, lng2){

  d_lat <- (lat2-lat1) * pi/180   #aca estamos pasando de decimales a radianes
  d_lng <- (lng2-lng1) * pi/180
  r <- 6371
  d_km <- 2*r*asin(sqrt(sin(d_lat/2)^2+cos(lat1*pi/180)*cos(lat2*pi/180)*sin(d_lng/2)^2)) #Formula de Haversine para la dist

  return(d_km)  
}

#la función "as.duration" mide la diferencia en segundos y lo devuelve como un dato tipo "Duration".
#se trasnforma el dato tipo "duration" a dato numerico y se lo divide  en 60 para expresar el resultado en minutos.
#como en el condicional if no se puede usar un vector y dif_time_min es un vector entonces temenos que usar ifelse para evaluar el vector y si el valor es negativo lo transforma con la funcion(abs) y si no devuelve el valor como venía.

travel_time_func <- function(time1,time2)
{
  dif_time <- as.duration(time2-time1) 
  dif_time_min <- as.numeric(dif_time)/60  
  ifelse(dif_time_min < 0, abs(dif_time_min), dif_time_min)
}


#acá se hace un data frame con las columas necesarias para saber la distancia y el tiempo de cada viaje mas si es mienbro o casual.
data_flit1 <- select(data_cruda_total, rideable_type, ride_id, started_at, ended_at, start_lat, start_lng, end_lat, end_lng, member_casual)

#en esta df ya se incluyen las columnas calculadas del tiempo y la distancia con las funciones ya armadas.

data_dist_time <- mutate(data_flit1, travel_dis = func_dist(data_flit1$start_lat, data_flit1$start_lng, data_flit1$end_lat, data_flit1$end_lng), travel_time = travel_time_func(data_flit1$started_at, data_flit1$ended_at))

#acá lo que hacemos es redondear los datos y ordenarlos de manera ascendente

data_dist_time <- mutate_at(data_dist_time, vars(travel_dis, travel_time), round, digits = 1)
data_dist_time <- arrange(data_dist_time, data_dist_time$travel_dis, data_dist_time$travel_time)

ggplot(data = data_dist_time)+geom_bar(mapping = aes(x=member_casual, fill = factor(rideable_type)))

write.csv(data_dist_time, "Df_data_dis_time")


ggplot(data = data_dist_time, aes(x = member_casual, y=travel_time))+geom_boxplot()

x <- c(data_dist_time$travel_time)
mean(x)
summary(x)
  