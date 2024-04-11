

library(tidyverse)
library(dplyr) 
library(DBI)
library(RSQLite)
library(odbc)
library(ggplot2)


#se realiza la conexioncon la Base de Datos en SQLite
conexion <- dbConnect(RSQLite::SQLite(), dbname = 'F://2024//R//prueba tecnica Pedro Muñoz//BaseDatos.db')
#se consulta las tablas de la base de datos
dbListTables(conexion)
#se consulta los atributos de la base de datos 
dbListFields(conexion, "Municipios")
dbListFields(conexion, "Prestadores")

###Municipios ... Exploracion de datos.. Analisis Descriptivos

dbSendQuery(conexion, "SELECT * FROM Municipios limit 10")
mun <- dbSendQuery(conexion, "SELECT * FROM Municipios limit 10")
dbFetch(mun)

#descripcion de las columnas tipo de datos 
dbColumnInfo(mun)

#solamente escogemos 1000 filas de la tabla Municipio
mun1 <- dbGetQuery(conexion, "SELECT * FROM Municipios limit 1000")
head(mun1)
#se verifica el tipo de datos
summary(mun1)
#se cambia el tipo de datoa  a la variable superfice de caracter a numerico por que mas adelante trabajaremos con esa variable

mun1$Superficie <- as.numeric(gsub(",", ".",mun1$Superficie))

summary(mun1)

#se suma la poblacion total por departamento en un dataframe
mun11 <- mun1 %>%
  group_by(Departamento) %>%
  summarise(Pobla=sum(Poblacion))
#se visualiza el dataframe
View(mun11)
#se visualiza el dataframe
#grafico de barras poblacion por departamento
ggplot(mun11, aes(x=Pobla, y=Departamento, fill=Pobla)) +
  geom_bar(stat="identity") +  ggtitle("Poblacion Total por Departamentos")
 #  scale_fill_brewer(palette="Pastel2")
##se suma la poblacion total por region en un dataframe
mun22 <- mun1 %>%
  group_by(Region) %>%
  summarise(Pobla=sum(Poblacion))
#se visualiza el dataframe
View(mun22)
#poblacion total por region
ggplot(mun22, aes(x=Pobla, y=Region, fill=Pobla)) +
  geom_bar(stat="identity")+  ggtitle("Poblacion Total por Region")


##se suma de superficie total por departamento en un dataframe
mun33 <- mun1 %>%
  group_by(Departamento) %>%
  summarise(Superf=sum(Superficie))
#se visualiza el dataframe
View(mun33)
#superficie total por departamento
ggplot(mun33, aes(x=Superf, y=Departamento, fill=Superf)) +
  geom_bar(stat="identity")+  ggtitle("Superficie Total por Departamentos")


##se suma la superficie total por region en un dataframe
mun44 <- mun1 %>%
  group_by(Region) %>%
  summarise(Superf=sum(Superficie))
#se visualiza el dataframe
view(mun44)
#superficie total por Region
ggplot(mun44, aes(x=Superf, y=Region, fill=Superf)) +
  geom_bar(stat="identity")+  ggtitle("Superficie Total por Region")

###Prestadores ... Exploracion de datos.. Analisis Descriptivos

dbSendQuery(conexion, "SELECT * FROM Prestadores limit 1000")
prest <- dbSendQuery(conexion, "SELECT * FROM Prestadores limit 1000")
dbFetch(prest)


#solamente escogemos 1000 filas de la tabla Prestadores
prest1 <- dbGetQuery(conexion, "SELECT * FROM Prestadores limit 1000")
head(prest1)
#se verifica el tipo de datos
summary(prest1)
#consulta desde dataframe en R total de tipo de personal por departamento
prest11 <- prest1 %>%
  group_by(depa_nombre,clpr_codigo,clpr_nombre) %>%
  summarise(TotalPersonal=n())
#se visualiza el dataframe
View(prest11)
#grafico del Total de Tipo de Personal por Departamento
ggplot(prest11, aes(x=TotalPersonal, y=depa_nombre, z=clpr_nombre, fill=clpr_nombre)) +
  geom_bar(stat="identity")+  ggtitle("Total de Tipo de personal por Departamento")


#la misma consulta desde la base de datos de tipo de personal por departamento
(prest11 <- dbGetQuery(conexion, "SELECT depa_nombre,clpr_nombre,count(clpr_codigo) as clpr_codigo  FROM Prestadores WHERE clpr_codigo IN (1,2,3,4) group by depa_nombre limit 1000"))
#se visualiza el dataframe
view(prest11)


#consulta desde dataframe en R total de tipo de personal por municipio
prest22 <- prest1 %>%
  group_by(muni_nombre,clpr_codigo,clpr_nombre) %>%
  summarise(TotalPersonal=n())
#se visualiza el dataframe
View(prest22)

#Grafico del Total de Tipo de Personal por Municipio
ggplot(prest22, aes(x=TotalPersonal, y=muni_nombre, z=clpr_nombre, fill=clpr_nombre)) +
  geom_bar(stat="identity")+  ggtitle("Total de Tipo de personal por Departamento")




