##### Fallstudien 1 - Projekt 4 #####
#####
##### Jorrit Kuehne #####


library(readxl)
setwd("C:/Users/jorri/Documents/GitHub/fallstudien/Projekt 4")
data = read_xlsx("Medaillen.xlsx")

laender = unique(data$Land)
sportarten = unique(data$Sportart) 

### Notize Aufgabe 1

## Untersuchen Sie die Abhängigkeit zwischen der Sportart und dem Land bezüglich
## der Gesamtanzahl an Medaillen

cTable1 = t(matrix(data$Total, nrow = 4, ncol = 5))

rownames(cTable1) = laender
colnames(cTable1) = sportarten


### Notizen Aufgabe 2

## Untersuchen Sie für jede Sportart die Abhängigkeit zwischen der Medaillenfarbe
## und dem Land

# Kampfsport

cTableKampfsport = as.matrix(subset(data, Sportart == "Kampfsport")[ , 3:5])
rownames(cTableKampfsport) = laender

# Leichtathletik

cTableLeichtathletik = as.matrix(subset(data, Sportart == "Leichtathletik")[ , 3:5])
rownames(cTableLeichtathletik) = laender

# Ballsportarten

cTableBallsportarten = as.matrix(subset(data, Sportart == "Ballsportart")[ , 3:5])
rownames(cTableBallsportarten) = laender

# Schwimmen

cTableSchwimmen = as.matrix(subset(data, Sportart == "Schwimmen")[ , 3:5])
rownames(cTableSchwimmen) = laender



### Notizen Aufgabe 3

## Untersuchen Sie für jedes Land die Abhängigkeit zwischen der Medaillenfarbe und
## der Sportart.

# USA

cTableUSA = as.matrix(subset(data, Land == "USA")[ , 3:5])
rownames(cTableUSA) = sportarten

# VR China

cTableVRChina = as.matrix(subset(data, Land == "VR China")[ , 3:5])
rownames(cTableVRChina) = sportarten

# Japan

cTableJapan = as.matrix(subset(data, Land == "Japan")[ , 3:5])
rownames(cTableJapan) = sportarten

# Australien

cTableAustralien = as.matrix(subset(data, Land == "Australien")[ , 3:5])
rownames(cTableAustralien) = sportarten

# Frankreich

cTableFrankreich = as.matrix(subset(data, Land == "Frankreich")[ , 3:5])
rownames(cTableFrankreich) = sportarten
