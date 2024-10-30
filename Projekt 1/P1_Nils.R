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

boxplot(data22$Life_Expectancy_Overall, data22$Life_Expectancy_Male, data22$Life_Expectancy_Female, 
        names = c("Gesamt","Männlich","Weiblich"), las = 1, cex.axis = 0.7,
        xlab = "Alter in Jahre", horizontal = T)

?boxplot

boxplot(zen22$Life_Expectancy_Overall[zen22$Region == "Europe"], zen22$Life_Expectancy_Overall[zen22$Region == "Asia"], zen22$Life_Expectancy_Overall[zen22$Region == "Americas"], zen22$Life_Expectancy_Overall[zen22$Region == "Africa"], zen22$Life_Expectancy_Overall[zen22$Region == "Oceania"], names=c("Europa", "Asien", "Amerika", "Afrika", "Ozeanien"),las = 1, cex.axis = 0.7,
        xlab = "", ylab ="", horizontal = T)
title(xlab = "Alter in Jahre", line = 2.5)
title(ylab = "Kontinente", line = 4)


# Europa
boxplot(zen22$Life_Expectancy_Overall[zen22$Region == "Europe"], zen22$Life_Expectancy_Overall[zen22$Subregion == "Northern Europe"], zen22$Life_Expectancy_Overall[zen22$Subregion == "Western Europe"], zen22$Life_Expectancy_Overall[zen22$Subregion == "Eastern Europe"], zen22$Life_Expectancy_Overall[zen22$Subregion == "Southern Europe"], names=c("Europa", "Nordeuropa", "Westeuropa", "Osteuropa", "Südeuropa"),las = 1, cex.axis = 0.7,
        xlab = "", ylab ="", horizontal = T, at = c(1,2.5,3.5,4.5,5.5))
abline(h = 1.75,lty = 3)
title(xlab = "Alter in Jahre", line = 2.5)
title(ylab = "(Sub-)Regionen", line = 4.5)



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
#view(mapdata)

#Länder nach Census benennen
mapdata$subregion[mapdata$region == "Antigua"] <- "Antigua"
mapdata$region[mapdata$region == "Antigua"] <- "Antigua and Barbuda"
mapdata$subregion[mapdata$region == "Barbuda"] <- "Barbuda"
mapdata$region[mapdata$region == "Barbuda"] <- "Antigua and Barbuda"
mapdata$region[mapdata$region == "Bahamas"] <- "Bahamas, The"
mapdata$region[mapdata$region == "Myanmar"] <- "Burma"
mapdata$region[mapdata$region == "Cape Verde"] <- "Cabo Verde"
mapdata$region[mapdata$region == "Republic of Congo"] <- "Congo (Brazzaville)"
mapdata$region[mapdata$region == "Democratic Republic of the Congo"] <- "Congo (Kinshasa)"
mapdata$region[mapdata$region == "Ivory Coast"] <- "Côte d'Ivoire"
mapdata$region[mapdata$region == "Curacao"] <- "Curaçao"
mapdata$region[mapdata$region == "Czech Republic"] <- "Czechia"
mapdata$region[mapdata$region == "Swaziland"] <- "Eswatini"
mapdata$subregion[mapdata$region == "French Guiana"] <- "French Guiana"
mapdata$region[mapdata$region == "French Guiana"] <- "France"
mapdata$region[mapdata$region == "French Southern and Antarctic Lands"] <- "France"
mapdata$region[mapdata$region == "Guadeloupe"] <- "France"
mapdata$subregion[mapdata$region == "Martinique"] <- "Martinique"
mapdata$region[mapdata$region == "Martinique"] <- "France"
mapdata$subregion[mapdata$region == "Mayotte"] <- "Mayotte"
mapdata$region[mapdata$region == "Mayotte"] <- "France"
mapdata$subregion[mapdata$region == "Reunion"] <- "Reunion"
mapdata$region[mapdata$region == "Reunion"] <- "France"
mapdata$region[mapdata$region == "Gambia"] <- "Gambia, The"
mapdata$region[mapdata$subregion == "Gaza Strip"] <- "Gaza Strip"
mapdata$subregion[mapdata$subregion == "Gaza Strip"] <- NA
mapdata$region[mapdata$subregion == "Hong Kong"] <- "Hong Kong"
mapdata$subregion[mapdata$subregion == "Hong Kong"] <- NA
mapdata$region[mapdata$region == "North Korea"] <- "Korea, North"
mapdata$region[mapdata$region == "South Korea"] <- "Korea, South"
mapdata$region[mapdata$subregion == "Macao"] <- "Macau"
mapdata$subregion[mapdata$subregion == "Macao"] <- NA
mapdata$region[mapdata$region == "Micronesia"] <- "Micronesia, Federated States of"
mapdata$subregion[mapdata$region == "Azores"] <- "Azores"
mapdata$region[mapdata$region == "Azores"] <- "Portugal"
mapdata$region[mapdata$region == "Madeira Islands"] <- "Portugal"
mapdata$subregion[mapdata$region == "Ascension Island"] <- "Ascension Island"
mapdata$region[mapdata$region == "Ascension Island"] <- "Saint Helena, Ascension, and Tristan da Cunha"
mapdata$subregion[mapdata$region == "Saint Helena"] <- "Saint Helena"
mapdata$region[mapdata$region == "Saint Helena"] <- "Saint Helena, Ascension, and Tristan da Cunha"
mapdata$subregion[mapdata$region == "Nevis"] <- "Nevis"
mapdata$region[mapdata$region == "Nevis"] <- "Saint Kitts and Nevis"
mapdata$subregion[mapdata$region == "Saint Kitts"] <- "Saint Kitts"
mapdata$region[mapdata$region == "Saint Kitts"] <- "Saint Kitts and Nevis"
mapdata$region[mapdata$region == "Grenadines"] <- "Saint Vincent and the Grenadines"
mapdata$subregion[mapdata$region == "Saint Vincent"] <- "Saint Vincent"
mapdata$region[mapdata$region == "Saint Vincent"] <- "Saint Vincent and the Grenadines"
mapdata$region[mapdata$region == "Canary Islands"] <- "Spain"
mapdata$subregion[mapdata$region == "Tobago"] <- "Tobago"
mapdata$region[mapdata$region == "Tobago"] <- "Trinidad and Tobago"
mapdata$subregion[mapdata$region == "Trinidad"] <- "Trinidad"
mapdata$region[mapdata$region == "Trinidad"] <- "Trinidad and Tobago"
mapdata$region[mapdata$region == "UK"] <- "United Kingdom"
mapdata$region[mapdata$region == "USA"] <- "United States"
mapdata$region[mapdata$region == "Virgin Islands" & mapdata$subregion == " British"] <- "Virgin Islands, British"
mapdata$subregion[mapdata$region == "Virgin Islands" & mapdata$subregion == " British"] <- NA
mapdata$region[mapdata$region == "Virgin Islands" & mapdata$subregion == " US"] <- "Virgin Islands, U.S."
mapdata$subregion[mapdata$region == "Virgin Islands" & mapdata$subregion == " US"] <- NA
mapdata$region[mapdata$region == "Palestine" & mapdata$subregion == "2"] <- "West Bank"
mapdata$subregion[mapdata$region == "West Bank"] <- NA


# beide Datensätze in einem neuen Datensatz vereinen
alldata <- left_join(mapdata, map_zen22, by="region")
#view(alldata)

# Lebenserwartung in Kategorien einteilen
alldata$kategorie <- cut(alldata$Life_Expectancy_Overall,
                         breaks = c(0, 50, 60, 70, 80, 100),
                         labels = c("x<50", "50<=x<60", "60<=x<70", "70<=x<80", "80<=x"))

map1<-ggplot(alldata, aes(x= long, y = lat, group=group))+
  #geom_polygon(aes(fill =Life_Expectancy_Overall),color="black")
  geom_polygon(aes(fill =kategorie),color="black")
map1

#Optische Anpassung der Weltkarte
#map2<-map1 + scale_fill_gradient(name="Jahre", low ="orange", high="firebrick4", na.value="grey50")+
  map2<-map1 + scale_fill_manual(values = c("x<50" = "red", "50<=x<60" = "red", "60<=x<70" = "yellow", "70<=x<80" = "green", "80<=x" = "blue"))+
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
