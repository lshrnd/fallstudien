library(readxl)
data = read_xlsx("Medaillen.xlsx")

land = unique(data$Land)
sportarten = unique(data$Sportart) 
#############

### Notize Aufgabe 1

## Untersuchen Sie die Abhängigkeit zwischen der Sportart und dem Land bezüglich
## der Gesamtanzahl an Medaillen

cTable1 = t(matrix(data$Total, nrow = 4, ncol = 5))
rownames(cTable1) = land


colnames(cTable1) = sportarten

####Deskription
par(mar = c(1, 0.5, 0.5, 0))
mosaicplot(cTable1,
           main="",cex.axis = 1.5, color=T)
#Test
chisq.test(cTable1)
#chisq.test(cTable1, simulate.p.value = TRUE) #Monte Carlo 

#######Permutations
# Funktion zur Erstellung der neuen Matrix
neu_matrix <- function(original_matrix) {
  # Spaltensummen berechnen
  column_sums <- colSums(original_matrix)
  
  # Anzahl der Zeilen und Spalten
  n_rows <- nrow(original_matrix)
  n_cols <- ncol(original_matrix)
  
  # Neue Matrix initialisieren
  new_matrix <- matrix(0, nrow = n_rows, ncol = n_cols)
  
  # Werte für jede Spalte zufällig verteilen
  for (col in seq_len(n_cols)) {
    # Spaltensumme in zufällige Werte aufteilen
    values <- sample(1:column_sums[col], size = n_rows, replace = TRUE)
    
    # Werte so skalieren, dass die Summe gleich bleibt
    scaled_values <- floor(values / sum(values) * column_sums[col])
    
    # Anpassen, falls Rundungsfehler die Summe leicht verzerrt
    diff <- column_sums[col] - sum(scaled_values)
    while (diff > 0) {
      # Verteile die Differenz zufällig, ohne negative Werte zu erzeugen
      idx <- sample(seq_len(n_rows), size = 1)
      scaled_values[idx] <- scaled_values[idx] + 1
      diff <- diff - 1
    }
    
    # Spalte der neuen Matrix zuweisen
    new_matrix[, col] <- scaled_values
  }
  
  return(new_matrix)
}
#neu_matrix(cTableUSA) #Test

permutation<-function(original_matrix, iterations = 1000) {
  results <- numeric(iterations)  # Hier speichern wir die p-Werte
  expected_values <- original_matrix  # Erwartete Werte sind die Originalmatrix
  
  for (i in seq_len(iterations)) {
    # Neue Matrix erstellen
    new_matrix <- neu_matrix(original_matrix)
    
    # Chi-Quadrat-Test durchführen
    chi_squared_test <- chisq.test(new_matrix)
    
    # Teststatistik speichern
    results[i] <- chi_squared_test$statistic
  }
  
  return(results)
}
permutationChi<-function(cTable, iterations=10000){
  sort(quantile(permutation(cTable, iterations),0.95, na.rm=T))
}


### Notizen Aufgabe 2
par(mar = c(1, 0.5, 1, 0), cex.main=1.5)
## Untersuchen Sie für jede Sportart die Abhängigkeit zwischen der Medaillenfarbe
## und dem Land

# Kampfsport

cTableKampfsport = as.matrix(subset(data, Sportart == "Kampfsport")[ , 3:5])
rownames(cTableKampfsport) = land
sum(cTableKampfsport)
mosaicplot(cTableKampfsport, main="Kampfsport",cex.axis = 1.5, color=T) 
chisq.test(cTableKampfsport)
#chisq.test(cTableKampfsport, simulate.p.value = TRUE)
fisher.test(cTableKampfsport)
permutationChi(cTableKampfsport)

# Leichtathletik

cTableLeichtathletik = as.matrix(subset(data, Sportart == "Leichtathletik")[ , 3:5])
rownames(cTableLeichtathletik) = land
sum(cTableLeichtathletik)
mosaicplot(cTableLeichtathletik,main="Leichtathletik",cex.axis = 1.5, color=T)
chisq.test(cTableLeichtathletik)
#chisq.test(cTableLeichtathletik, simulate.p.value = TRUE)
fisher.test(cTableLeichtathletik)
permutationChi(cTableLeichtathletik)

# Ballsportarten

cTableBallsportarten = as.matrix(subset(data, Sportart == "Ballsportart")[ , 3:5])
rownames(cTableBallsportarten) = land
sum(cTableBallsportarten)
mosaicplot(cTableBallsportarten,main="Ballsport",cex.axis = 1.5, color=T)
chisq.test(cTableBallsportarten)
#chisq.test(cTableBallsportarten, simulate.p.value = TRUE)
fisher.test(cTableBallsportarten) #lehnt ab
permutationChi(cTableBallsportarten)

# Schwimmen

cTableSchwimmen = as.matrix(subset(data, Sportart == "Schwimmen")[ , 3:5])
rownames(cTableSchwimmen) = land
sum(cTableSchwimmen)
mosaicplot(cTableSchwimmen,main="",cex.axis = 1.5, color=T)
chisq.test(cTableSchwimmen)
#chisq.test(cTableSchwimmen, simulate.p.value = TRUE)
fisher.test(cTableSchwimmen)
permutationChi(cTableSchwimmen)

# Voraussetzung der Unabhänigigen Stichprobe wahrscheinlich für diesen Aufgabenteil
# nicht gegeben, da die Zeilen (Medaillenfarben für Land) definitiv abhängig sind.


### Notizen Aufgabe 3

## Untersuchen Sie für jedes Land die Abhängigkeit zwischen der Medaillenfarbe und
## der Sportart.
# USA

cTableUSA = as.matrix(subset(data, Land == "USA")[ , 3:5])
rownames(cTableUSA) = sportarten
sum(cTableUSA)
mosaicplot(cTableUSA,main="USA",cex.axis = 1.5, color=T)
chisq.test(cTableUSA)
fisher.test(cTableUSA)
permutationChi(cTableUSA, iterations = 10000)

# VR China

cTableVRChina = as.matrix(subset(data, Land == "VR China")[ , 3:5])
rownames(cTableVRChina) = sportarten
sum(cTableVRChina)
mosaicplot(cTableVRChina,main="VR China",cex.axis = 1.5, color=T)
chisq.test(cTableVRChina)
fisher.test(cTableVRChina) #lehnt ab
permutationChi(cTableVRChina)

# Japan

cTableJapan = as.matrix(subset(data, Land == "Japan")[ , 3:5])
rownames(cTableJapan) = sportarten
sum(cTableJapan)
mosaicplot(cTableJapan,main="Japan",cex.axis = 1.5, color=T)
chisq.test(cTableJapan)
fisher.test(cTableJapan) #lehnt ab
permutationChi(cTableJapan)

# Australien

cTableAustralien = as.matrix(subset(data, Land == "Australien")[ , 3:5])
rownames(cTableAustralien) = sportarten
sum(cTableAustralien)
mosaicplot(cTableAustralien,main="Australien",cex.axis = 1.5, color=T)
chisq.test(cTableAustralien)
fisher.test(cTableAustralien)
permutationChi(cTableAustralien)

# Frankreich

cTableFrankreich = as.matrix(subset(data, Land == "Frankreich")[ , 3:5])
rownames(cTableFrankreich) = sportarten
sum(cTableFrankreich)
mosaicplot(cTableFrankreich,main="Frankreich",cex.axis = 1.5, color=T)
chisq.test(cTableFrankreich)
fisher.test(cTableFrankreich)
permutationChi(cTableFrankreich)



