# verificar que tenemos las librerías instaladas

librerias <- c("dplyr","readr","ggplot2")

if (length(setdiff(librerias, rownames(installed.packages))) >0 ) {
  install.packages(setdiff(librerias, rownames(installed.packages())))  
}

library(dplyr)
library(readr)
library(ggplot2)

getwd()

setwd("C:/Users/rguerrerop/Desktop")

list.files(getwd())

archivo <- read.csv("googleplaystore.csv")

View(head(archivo))

options(scipen = 999)

archivo <- archivo %>%
  filter(Installs != "0")

archivo$Installs <- gsub(",", "", gsub("\\.", "", archivo$Installs))
archivo$Installs <- as.character(archivo$Installs)
archivo$Installs <- substr(archivo$Installs,1,nchar(archivo$Installs)-1)
archivo$Installs <- as.numeric(archivo$Installs)


# * ¿Cuáles son las categorías de aplicaciones de Pago que más ganancias dejan?

archivo %>%
  filter(Type == "Paid") %>%
  group_by(Category) %>%
  summarise(Instalaciones = sum(Installs)) %>%
  arrange(desc(Instalaciones)) %>%
  head(10) %>%
  ggplot(aes(x = Category, y = Instalaciones)) + 
  geom_bar(stat = "identity", width = .5, fill = "forestgreen") +
  labs(title = "Top 10 Categorías de pago con más instalaciones") +
  theme(axis.title.x = element_text(angle = 65, vjust = 0.6))


# * , ¿Cuáles son los géneros que más ganancias producen?

archivo %>%
  filter(Type == "Paid" & Category == "FAMILY") %>%
  group_by(Genres) %>%
  summarise(conteo = n()) %>%
  arrange(desc(conteo)) %>%
  head(10) %>%
  ggplot(aes(x = Genres, y = conteo)) +
  geom_bar(stat = "identity", width = .5, fill = "forestgreen") +
  labs(title = "Top 10 géneros de categoria Familia con más ganancias") +
  theme(axis.title.x = element_text(angle = 65, vjust = 0.6))

# * De las aplicaciones gratuitas, ¿Cuáles son las categorías más instaladas por los usuarios?

archivo %>%
  filter(Type == "Free") %>%
  group_by(Category) %>%
  summarise(Instalaciones = sum(Installs)) %>%
  arrange(desc(Instalaciones)) %>%
  head(10) %>%
  ggplot(aes(x = Category, y = Instalaciones)) +
  geom_bar(stat = "identity", width = .5, fill ="forestgreen") +
  labs(title = "Top 10 categorías gratuitas más descargadas") +
  theme(axis.title.x = element_text(angle =  65, vjust = 0.6))


# De las categorías gratuitas, ¿Qué géneros son los más destacados?

  archivo %>%
    filter(Type  == "Free" & Category == "GAME") %>%
    group_by(Genres) %>%
    summarise(instalaciones = sum(Installs)) %>%
    arrange(desc(instalaciones)) %>%
    head(10) %>%
    ggplot(aes(x = Genres, y = instalaciones)) +
    geom_bar(stat = "identity", width = .5, fill = "forestgreen") +
    labs(title = "Top 10 de géneros de categoría JUEGOS gratuitas más descargadas") +
    theme(axis.title.x = element_text(angle = 65, vjust = 0.6))
  









   
  
  
  
  
  


