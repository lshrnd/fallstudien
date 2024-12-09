library(readxl)
library(crayon)
setwd("C:/Users/lisah/Documents/GitHub/fallstudien/Projekt 4")
data = read_xlsx("Medaillen.xlsx")


### chi^2-Test (analog zu chisq.test(), mir war langweilig man ich weiss doch auch nicht)
# testet H0: Merkmale X und Y sind unabhängig
# Input: table = Daten in Kontingenztafelform, alpha = gewuenschtes Sign.niveau
# Output: Testentscheidung, Wert der Teststatistik, Quantil, p-Wert
myChisqTest = function(table, alpha){
  n_X = margin.table(table,1) # Zeilensummen
  n_Y = margin.table(table,2) # Spaltensummen
  n = sum(table) # Gesamtsumme
  table_expected = outer(n_X, n_Y)/n # erwartete Tafel im Falle der Unabh.
  
  stat = sum(((table - table_expected)^2)/table_expected) # Teststatitik
  
  df = (length(n_X)-1)*(length(n_Y)-1) # Freiheitsgrade
  quan = qchisq(1-alpha,df) # Quantil
  
  p = 1-pchisq(stat,df) # p-Wert
  
  if(stat > quan) result = red("H0 ablehnen") else result = green("H0 beibehalten") 
  # Testentscheidung
  
  # Output
  cat("Chi-Squared-Test for:", deparse(substitute(table)), "\n \n")
  cat("Teststatistik:", stat, "                         p-Wert:", p, "\n")
  cat("      Quantil:", quan, "               Testentscheidung:", result)
}

# Helper-Funktion
giveExpected = function(table){
  n_X = margin.table(table,1) # Zeilensummen
  n_Y = margin.table(table,2) # Spaltensummen
  n = sum(table) # Gesamtsumme
  table_expected = outer(n_X, n_Y)/n # erwartete Tafel im Falle der Unabh.
  
  slots = length(n_X) * length(n_Y)
  smallerThan5 = sum(table_expected < 5)
  perc = smallerThan5 / slots * 100
  
  smallerThan1 = sum(table_expected < 1)
  
  print(table_expected)
  cat("\n n:", n, "\n")
  
  if(perc <= 20){
    cat(green(smallerThan5, "von", slots,"(", perc, "%) Werten sind kleiner als 5 \n"))
  } else {
    cat(red(smallerThan5, "von", slots,"(", perc, "%) Werten sind kleiner als 5 \n"))
  }
  
  if(smallerThan1 == 0){
    result2 = green("kein Wert ist kleiner als 1")
  } else if(smallerThan1 == 1){
    result2 = red("ein Wert ist kleiner als 1")
  } else {
    result2 = red(smallerThan1, "Werte sind kleiner als 1")
  }
  
  cat(result2)
}


### Aufgabe 1:
medalCountTotal = t(matrix(data$Total, nrow = 4, ncol = 5))
rownames(medalCountTotal) = unique(data$Land)
colnames(medalCountTotal) = unique(data$Sportart)

giveExpected(medalCountTotal)
myChisqTest(medalCountTotal,0.05)



### Aufgabe 2:

# Medaillenspiegel fuer Kampfsport
medalCountKampfsport = as.matrix(subset(data, Sportart == "Kampfsport")[,3:5])
rownames(medalCountKampfsport) = unique(data$Land)

giveExpected(medalCountKampfsport)
myChisqTest(medalCountKampfsport, 0.05)

# Medaillenspiegel fuer Leichtathletik
medalCountLeichtathletik = as.matrix(subset(data, Sportart == "Leichtathletik")[,3:5])
rownames(medalCountLeichtathletik) = unique(data$Land)

giveExpected(medalCountLeichtathletik)
myChisqTest(medalCountLeichtathletik, 0.05)

# Medaillenspiegel fuer Ballsportart
medalCountBallsportart = as.matrix(subset(data, Sportart == "Ballsportart")[,3:5])
rownames(medalCountBallsportart) = unique(data$Land)

giveExpected(medalCountBallsportart)
myChisqTest(medalCountBallsportart, 0.05)

# Medaillenspiegel fuer Schwimmen
medalCountSchwimmen = as.matrix(subset(data, Sportart == "Schwimmen")[,3:5])
rownames(medalCountSchwimmen) = unique(data$Land)

giveExpected(medalCountSchwimmen)
myChisqTest(medalCountSchwimmen, 0.05)

### Aufgabe 3:

# Medaillenspiegel fuer USA
medalCountUSA = as.matrix(subset(data, Land == "USA")[,3:5])
rownames(medalCountUSA) = unique(data$Sportart)

giveExpected(medalCountUSA)
myChisqTest(medalCountUSA, 0.05)

# Medaillenspiegel fuer VR China
medalCountChina = as.matrix(subset(data, Land == "VR China")[,3:5])
rownames(medalCountChina) = unique(data$Sportart)

giveExpected(medalCountChina)
myChisqTest(medalCountChina, 0.05)

# Medaillenspiegel fuer Japan
medalCountJapan = as.matrix(subset(data, Land == "Japan")[,3:5])
rownames(medalCountJapan) = unique(data$Sportart)

giveExpected(medalCountJapan)
myChisqTest(medalCountJapan, 0.05)

# Medaillenspiegel fuer Australien
medalCountAustralien = as.matrix(subset(data, Land == "Australien")[,3:5])
rownames(medalCountAustralien) = unique(data$Sportart)

giveExpected(medalCountAustralien)
myChisqTest(medalCountAustralien, 0.05)

# Medaillenspiegel fuer Frankreich
medalCountFrankreich = as.matrix(subset(data, Land == "Frankreich")[,3:5])
rownames(medalCountFrankreich) = unique(data$Sportart)

giveExpected(medalCountFrankreich)
myChisqTest(medalCountFrankreich, 0.05)