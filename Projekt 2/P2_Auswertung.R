data = read.table("Konzentrationsdaten.txt", header = T)
#######################################################
data_gr1 = data[data$gruppe == 1,]
data_gr2 = data[data$gruppe == 2,]

data_du1 = data[data$durchgang == 1,]
data_du2 = data[data$durchgang == 2,]

#######################################################  
# dataN - Datensatz ohne den Ausreisser, fuer das Testen
dataN = data[- which(data$id == 14),]

dataN_gr1 = dataN[dataN$gruppe == 1,]
dataN_gr2 = dataN[dataN$gruppe == 2,]

dataN_du1 = dataN[dataN$durchgang == 1,]
dataN_du2 = dataN[dataN$durchgang == 2,]


### A1 - Vergleich des Konzentrationsscore zwischen Gruppe 1 und 2, fuer Durchgang 1

# F-Test auf gleiche Varianzen unter NV-Annahme
var.test(dataN_gr1$KL[dataN_gr1$durchgang == 1],
         dataN_gr2$KL[dataN_gr2$durchgang == 1],
         alternative = "two.sided")

# t-Test auf gleichen Mittelwert unter NV-Annahme und gleicher Varianz
t.test(dataN_gr1$KL[dataN_gr1$durchgang == 1], 
       dataN_gr2$KL[dataN_gr2$durchgang == 1], 
       var.equal = T)

### A2 - Vergleich des Konzentrationsscore zwischen Durchgang 1 und 2, gruppenunabhaengig

# F-Test auf gleiche Varianzen unter NV-Annahme
var.test(dataN_du1$KL, 
         dataN_du2$KL, 
         alternative = "two.sided")

# t-Test auf H1: groesserer Mittelwert (von Gruppe 1) unter NV-Annahme und gleicher Varianz
t.test(dataN_du1$KL, 
       dataN_du2$KL, 
       alternative = "greater", 
       paired = T)

### A2 - Vergleich der Bearbeitungszeit zwischen Durchgang 1 und 2, gruppenunabhaengig
deltaB = dataN_du2$B - dataN_du1$B 
    # positiv = hat laenger gebraucht = Verschlechterung

# t-Test auf H1: Mittelwert < 0 bei NV-Annahme
t.test(deltaB, 
       alternative = "less")

### A3 - Vergleich der Verbesserung von Durchgang 1 zu Durchgang 2 zwischen Gruppe 1 und 2
deltaKL_gr1 = dataN_gr1$KL[dataN_gr1$durchgang == 2] - dataN_gr1$KL[dataN_gr1$durchgang == 1]
deltaKL_gr2 = dataN_gr2$KL[dataN_gr2$durchgang == 2] - dataN_gr2$KL[dataN_gr2$durchgang == 1]
    # positiv = hoeherer Score = Verbesserung

# F-Test auf gleiche Varianzen der Differenzen unter NV-Annahme
var.test(deltaKL_gr1,
         deltaKL_gr2, 
         alternative = "two.sided")

# t-Test auf H1: groesser Mittelwert (von Gruppe 1) bei NV-Annahme und gleicher Varianz
t.test(deltaKL_gr1,
       deltaKL_gr2, 
       alternative = "greater", 
       var.equal = T)

