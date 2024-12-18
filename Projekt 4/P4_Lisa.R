{ #-#
library(readxl)
library(crayon)
library(scales)

setwd("C:/Users/lisah/Documents/GitHub/fallstudien/Projekt 4")
data = read_xlsx("Medaillen.xlsx")

# fuer pdfs exportieren
setwd("C:/Users/lisah/OneDrive/DSStudium/Fallstudien I")

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

teststat = function(table){
  n_X = margin.table(table,1) # Zeilensummen
  n_Y = margin.table(table,2) # Spaltensummen
  n = sum(table) # Gesamtsumme
  table_expected = outer(n_X, n_Y)/n # erwartete Tafel im Falle der Unabh.
  
  stat = sum(((table - table_expected)^2)/table_expected)
}

# macht nur Schwachsinn, diese Funktion smh
permutationTest = function(table, repCount, alpha = 0.05){
  teststats = rep(NA,repCount)
  
  for(i in 1:repCount){
    order = sample(ncol(table))
    newTable = matrix(nrow = nrow(table), ncol = ncol(table), table[,order])
    teststats[i] = teststat(newTable)
  }
  
  unpermutatedTeststat = teststat(table)
  
  index = which.min(sort(teststats) >= unpermutatedTeststat)
  p = index / repCount
  
  if(p < alpha) result = red("H0 ablehnen") else result = green("H0 beibehalten")
  
  cat("Permutated Chi-Squared-Test for:", deparse(substitute(table)), "\n \n")
  cat("Teststatistik:", unpermutatedTeststat, "             p-Wert:", p, "\n")
  cat("                                 Testentscheidung:", result)
  print(teststats)
}

landNames = unique(data$Land)
landCols = alpha(c("USA" = "purple",
                   "VR China" = "red",
                   "Japan" = "white",
                   "Australien" = "green",
                   "Frankreich"= "blue"),
                 0.5)

sportNames = unique(data$Sportart)
sportCols = grey.colors(4)

medalNames = c("Gold","Silber","Bronze")
medalCols = c("#FDC961","#E5E4E4","#DCB386")

myRepCount = 10000

} #-#


### Aufgabe 1:
medalCountTotal = t(matrix(data$Total, nrow = 4, ncol = 5))
rownames(medalCountTotal) = landNames
colnames(medalCountTotal) = sportNames

giveExpected(medalCountTotal)
myChisqTest(medalCountTotal,0.05)
fisher.test(medalCountTotal)
chisq.test(medalCountTotal, simulate.p.value = T)



### Aufgabe 2:

# Medaillenspiegel fuer Kampfsport
medalCountKampfsport = as.matrix(subset(data, Sportart == "Kampfsport")[,3:5])
rownames(medalCountKampfsport) = landNames

giveExpected(medalCountKampfsport)
myChisqTest(medalCountKampfsport, 0.05)
fisher.test(medalCountKampfsport)
chisq.test(medalCountKampfsport, simulate.p.value = T)

# Medaillenspiegel fuer Leichtathletik
medalCountLeichtathletik = as.matrix(subset(data, Sportart == "Leichtathletik")[,3:5])
rownames(medalCountLeichtathletik) = landNames

giveExpected(medalCountLeichtathletik)
myChisqTest(medalCountLeichtathletik, 0.05)
fisher.test(medalCountLeichtathletik)
chisq.test(medalCountLeichtathletik, simulate.p.value = T)

# Medaillenspiegel fuer Ballsportart
medalCountBallsportart = as.matrix(subset(data, Sportart == "Ballsportart")[,3:5])
rownames(medalCountBallsportart) = landNames

giveExpected(medalCountBallsportart)
myChisqTest(medalCountBallsportart, 0.05)
fisher.test(medalCountBallsportart) # lehnt ab
# China hat mehr als erwartet
chisq.test(medalCountBallsportart, simulate.p.value = T)

# Medaillenspiegel fuer Schwimmen
medalCountSchwimmen = as.matrix(subset(data, Sportart == "Schwimmen")[,3:5])
rownames(medalCountSchwimmen) = landNames

giveExpected(medalCountSchwimmen)
myChisqTest(medalCountSchwimmen, 0.05)
fisher.test(medalCountSchwimmen)

### Aufgabe 3:

# Medaillenspiegel fuer USA
medalCountUSA = as.matrix(subset(data, Land == "USA")[,3:5])
rownames(medalCountUSA) = sportNames

giveExpected(medalCountUSA)
myChisqTest(medalCountUSA, 0.05)
fisher.test(medalCountUSA)

# Medaillenspiegel fuer VR China
medalCountChina = as.matrix(subset(data, Land == "VR China")[,3:5])
rownames(medalCountChina) = sportNames

giveExpected(medalCountChina)
myChisqTest(medalCountChina, 0.05)
fisher.test(medalCountChina) # lehnt ab
# China ist besser im Ballsport als erwartet, rest passt ca 

# Medaillenspiegel fuer Japan
medalCountJapan = as.matrix(subset(data, Land == "Japan")[,3:5])
rownames(medalCountJapan) = sportNames

giveExpected(medalCountJapan)
myChisqTest(medalCountJapan, 0.05)
fisher.test(medalCountJapan) # lehnt ab
# Japan hat etwas weniger Gold in Kampf und etwas mehr in Ball

# Medaillenspiegel fuer Australien
medalCountAustralien = as.matrix(subset(data, Land == "Australien")[,3:5])
rownames(medalCountAustralien) = sportNames

giveExpected(medalCountAustralien)
myChisqTest(medalCountAustralien, 0.05)
fisher.test(medalCountAustralien)

# Medaillenspiegel fuer Frankreich
medalCountFrankreich = as.matrix(subset(data, Land == "Frankreich")[,3:5])
rownames(medalCountFrankreich) = sportNames

giveExpected(medalCountFrankreich)
myChisqTest(medalCountFrankreich, 0.05)
fisher.test(medalCountFrankreich)

############ Deskription

# Barplot Medaillen Total nach Sportart aufgeteilt
barplot(medalCountTotal, 
        beside = T,
        ylim = c(0,40),
        col = landCols)
legend("top", ncol = 5, legend = landNames, col = landCols, 
       pch = 15, x.intersp = 0.8, y.intersp = 0.6)

# Barplot Medaillen Total nach Land aufgeteilt
barplot(t(medalCountTotal), 
        beside = T,
        ylim = c(0,40),
        col = sportCols)
legend("top", ncol = 4, legend = sportNames, pt.bg = sportCols, 
       pch = 22, x.intersp = 0.8, y.intersp = 0.6)

  # oder Gesamtsumme an Medaillen
barplot(margin.table(medalCountTotal,1),
        col = landCols)

# Barplot für jede Sportart nach Land aufgeteilt
pdf(file = "4_Barplots_medalCountSportart.pdf", width = 10, height = 6)
{
par(mfrow = c(2,2), mar = c(2,2,2.5,0))
barplot(t(medalCountKampfsport),
        beside = T,
        ylim = c(0,15),
        col = medalCols,
        main = "Kampfsport",
        cex.names = 1.2,
        cex.main = 1.35)
legend("top", ncol = 3, legend = medalNames, pt.bg = medalCols, 
       pch = 22, x.intersp = 0.8, y.intersp = 0.6, cex = 1.2)

barplot(t(medalCountLeichtathletik),
        beside = T,
        ylim = c(0,15),
        col = medalCols,
        main = "Leichtathletik",
        cex.names = 1.2,
        cex.main = 1.35)
legend("top", ncol = 3, legend = medalNames, pt.bg = medalCols, 
       pch = 22, x.intersp = 0.8, y.intersp = 0.6, cex = 1.2)

barplot(t(medalCountBallsportart),
        beside = T,
        ylim = c(0,15),
        col = medalCols,
        main = "Ballsportart",
        cex.names = 1.2,
        cex.main = 1.35)
legend("top", ncol = 3, legend = medalNames, pt.bg = medalCols, 
       pch = 22, x.intersp = 0.8, y.intersp = 0.6, cex = 1.2)

barplot(t(medalCountSchwimmen),
        beside = T,
        ylim = c(0,15),
        col = medalCols,
        main = "Schwimmen",
        cex.names = 1.2,
        cex.main = 1.35)
legend("top", ncol = 3, legend = medalNames, pt.bg = medalCols, 
       pch = 22, x.intersp = 0.8, y.intersp = 0.6, cex = 1.2)
}
dev.off()

