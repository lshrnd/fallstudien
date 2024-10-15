library(dplyr) # für semijoin

setwd("C:/Users/Nils Mac/Documents/GitHub/fallstudien/Projekt 1")
census = read.csv("census_2022_2002.csv")

#Aufteilen in Daten nach Jahr
census02<-subset(census, census$Year == 2002)
census22<-subset(census, census$Year == 2022)

#welche Zeilen NA
census[apply(census, 1, function(x) any(is.na(x))), ]

#Zensus ohne NA 
zen02_clean<-na.omit(census02) 
zen22_clean<- semi_join(census22, zen02_clean, by= "Country")

zen22 <- na.omit(census22)


# Boxplots Lebenserwartung nach Regionen und Subregionen

boxplot(zen22$Life_Expectancy_Overall[zen22$Region == "Europe"], zen22$Life_Expectancy_Overall[zen22$Region == "Asia"], zen22$Life_Expectancy_Overall[zen22$Region == "Americas"], zen22$Life_Expectancy_Overall[zen22$Region == "Africa"], zen22$Life_Expectancy_Overall[zen22$Region == "Oceania"], names=c("Europe", "Asia", "Americas", "Africa", "Oceania"))

boxplot(zen22$Life_Expectancy_Overall[zen22$Region == "Europe"], zen22$Life_Expectancy_Overall[zen22$Subregion == "Northern Europe"], zen22$Life_Expectancy_Overall[zen22$Subregion == "Western Europe"], zen22$Life_Expectancy_Overall[zen22$Subregion == "Eastern Europe"], zen22$Life_Expectancy_Overall[zen22$Subregion == "Southern Europe"], names=c("Europa", "Nord", "West", "Ost", "Süd"))
boxplot(zen22$Life_Expectancy_Overall[zen22$Region == "Asia"], zen22$Life_Expectancy_Overall[zen22$Subregion == "Western Asia"], zen22$Life_Expectancy_Overall[zen22$Subregion == "South-Central Asia"], zen22$Life_Expectancy_Overall[zen22$Subregion == "South-Eastern Asia"], zen22$Life_Expectancy_Overall[zen22$Subregion == "Eastern Asia"], names=c("Asien", "West", "Süd-Zentral", "Süd-Ost", "Ost"))
boxplot(zen22$Life_Expectancy_Overall[zen22$Region == "Americas"], zen22$Life_Expectancy_Overall[zen22$Subregion == "Northern America"], zen22$Life_Expectancy_Overall[zen22$Subregion == "Central America"], zen22$Life_Expectancy_Overall[zen22$Subregion == "South America"], zen22$Life_Expectancy_Overall[zen22$Subregion == "Caribbean"], names=c("Amerika", "Nord", "Zentral", "Süd", "Karibik"))
boxplot(zen22$Life_Expectancy_Overall[zen22$Region == "Africa"], zen22$Life_Expectancy_Overall[zen22$Subregion == "Northern Africa"], zen22$Life_Expectancy_Overall[zen22$Subregion == "Middle Africa"], zen22$Life_Expectancy_Overall[zen22$Subregion == "Southern Africa"], zen22$Life_Expectancy_Overall[zen22$Subregion == "Western Africa"], zen22$Life_Expectancy_Overall[zen22$Subregion == "Eastern Africa"], names=c("Afrika", "Nord", "Mittel", "Süd", "West", "Ost"))
boxplot(zen22$Life_Expectancy_Overall[zen22$Region == "Oceania"], zen22$Life_Expectancy_Overall[zen22$Subregion == "Australia/New Zealand"], zen22$Life_Expectancy_Overall[zen22$Subregion == "Micronesia"], zen22$Life_Expectancy_Overall[zen22$Subregion == "Melanesia"], zen22$Life_Expectancy_Overall[zen22$Subregion == "Polynesia"], names=c("Ozeanien", "AUS / NZL", "Mikronesien", "Melanesien", "Polynesien"))
















## Karte mit lebenserwartung 2022

library(maps)
library(ggplot2)
library(gapminder)
library(tidyverse)

map_zen22 <- zen22
head(map_zen22) # Variablennamen anzeigen
#Variable "country" in  "region" umbenennen
map_zen22 <- map_zen22 %>% 
  rename(continent = Region)
map_zen22 <- map_zen22 %>% 
  rename(region = Country)
head(map_zen22) # überprüfen ob Umbenennung funktioniert hat

#Erstellen eines Datensatzes mit Koordinaten zu den einzelnen Ländern
mapdata <-map_data("world")
view(mapdata)
# beide Datensätze in einem neuen Datensatz vereinen
alldata <- left_join(mapdata, map_zen22, by="region")
view(alldata)

map1<-ggplot(alldata[alldata$continent == "Oceania",], aes(x= long, y = lat, group=group))+
  geom_polygon(aes(fill =Life_Expectancy_Overall),color="black")
map1

#Optische Anpassung der Weltkarte
map2<-map1 + scale_fill_gradient(name="Jahre", 
                                 low ="red", 
                                 high="green", 
                                 na.value="grey50")+
  labs(title = "Lebenserwartung bei Geburt im Jahr 2022 nach Ländern", 
       subtitle = "(bei grau unterlegten Ländern sind keine Daten verfügbar)")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        rect = element_blank())
map2

beide <- intersect(mapdata$region, map_zen22$region)
print(beide)

nurmap <- setdiff(mapdata$region, map_zen22$region)
print(nurmap)

nurzen <- setdiff(map_zen22$region, mapdata$region)
print(nurzen)


#install and load writexl package
install.packages('writexl')
library(writexl)

write_xlsx(mapdata, 'C:\\Users\\Nils Mac\\Downloads\\data.xlsx')








install.packages("rnaturalearth")
# Laden der Daten und speichern als Objekt map_world
map_world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

map_world %>%
  ggplot() +
  geom_sf(fill = "white", color = "black") +
  labs(
    title = "World map", 
    subtitle = paste0("(", length(unique(map_world$admin)), " countries)"), # Die Funktion length() zählt die Länge des Vektors und damit die Anzahl der Länder
    x = "Longitude",
    y = "Latitude"
  )
