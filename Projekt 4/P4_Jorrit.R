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

chisq.test(cTable1)
chisq.test(cTable1, simulate.p.value = TRUE)


### Notizen Aufgabe 2

## Untersuchen Sie für jede Sportart die Abhängigkeit zwischen der Medaillenfarbe
## und dem Land

# Kampfsport

cTableKampfsport = as.matrix(subset(data, Sportart == "Kampfsport")[ , 3:5])
rownames(cTableKampfsport) = laender
sum(cTableKampfsport)
chisq.test(cTableKampfsport)
chisq.test(cTableKampfsport, simulate.p.value = TRUE)


# Leichtathletik

cTableLeichtathletik = as.matrix(subset(data, Sportart == "Leichtathletik")[ , 3:5])
rownames(cTableLeichtathletik) = laender
sum(cTableLeichtathletik)
chisq.test(cTableLeichtathletik)
chisq.test(cTableLeichtathletik, simulate.p.value = TRUE)


# Ballsportarten

cTableBallsportarten = as.matrix(subset(data, Sportart == "Ballsportart")[ , 3:5])
rownames(cTableBallsportarten) = laender
sum(cTableBallsportarten)
chisq.test(cTableBallsportarten)
chisq.test(cTableBallsportarten, simulate.p.value = TRUE)

# Schwimmen

cTableSchwimmen = as.matrix(subset(data, Sportart == "Schwimmen")[ , 3:5])
rownames(cTableSchwimmen) = laender
sum(cTableSchwimmen)
chisq.test(cTableSchwimmen)
chisq.test(cTableSchwimmen, simulate.p.value = TRUE)


# Voraussetzung der Unabhänigigen Stichprobe wahrscheinlich für diesen Aufgabenteil
# nicht gegeben, da die Zeilen (Medaillenfarben für Land) definitiv abhängig sind.


### Notizen Aufgabe 3

## Untersuchen Sie für jedes Land die Abhängigkeit zwischen der Medaillenfarbe und
## der Sportart.

# USA

cTableUSA = as.matrix(subset(data, Land == "USA")[ , 3:5])
rownames(cTableUSA) = sportarten
sum(cTableUSA)
chisq.test(cTableUSA)

# VR China

cTableVRChina = as.matrix(subset(data, Land == "VR China")[ , 3:5])
rownames(cTableVRChina) = sportarten
sum(cTableVRChina)
chisq.test(cTableVRChina)

# Japan

cTableJapan = as.matrix(subset(data, Land == "Japan")[ , 3:5])
rownames(cTableJapan) = sportarten
sum(cTableJapan)
chisq.test(cTableJapan)

# Australien

cTableAustralien = as.matrix(subset(data, Land == "Australien")[ , 3:5])
rownames(cTableAustralien) = sportarten
sum(cTableAustralien)
chisq.test(cTableAustralien)

# Frankreich

cTableFrankreich = as.matrix(subset(data, Land == "Frankreich")[ , 3:5])
rownames(cTableFrankreich) = sportarten
sum(cTableFrankreich)
chisq.test(cTableFrankreich)



permutationChisqTest = function(cTable) {
  newCols = sample(length(cTable[1, ]))
  print(newCols)
  newCTable = transform(as.data.frame(cTable), newCols)
  newCTable
}

permutationChisqTest(cTableFrankreich)


