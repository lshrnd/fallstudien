setwd("C:/Users/jorri/Documents/GitHub/fallstudien/Projekt 2")

data = read.table("Konzentrationsdaten.txt", header = T)

# Extremwert entfernen
 data = data[- which(data$id == 14), ]

{
dataG1 = data[data$gruppe == 1, ]
dataG2 = data[data$gruppe == 2, ]
dataG1D1 = dataG1[dataG1$durchgang == 1, ]
dataG1D2 = dataG1[dataG1$durchgang == 2, ]
dataG2D1 = dataG2[dataG2$durchgang == 1, ]
dataG2D2 = dataG2[dataG2$durchgang == 2, ]
dataD1 = data[data$durchgang == 1, ]
dataD2 = data[data$durchgang == 2, ]
dataD1GU = dataD1[dataD1$test_typ == "gu", ]
dataD1UG = dataD1[dataD1$test_typ == "ug", ]
}

n = length(data$id) / 2
n

  # DESKRIPTION zu Aufgabe 1 KONZENTRATIONSSCORE

hist(dataD1GU$KL,
     main = "Hist KL Durchgang 1 GU",
     #xlim = c(-30, 20),
     #ylim = c(0, 5),
     breaks = seq(-30, 20, 2)
     )

hist(dataD1UG$KL,
     main = "Hist KL Durchgang 1 UG",
     #xlim = c(-30, 20),
     #ylim = c(0,5),
     breaks = seq(-30, 20, 2)
     )

summary(dataD1GU$KL, quantile.type = 2)
sd(dataD1GU$KL)
summary(dataD1UG$KL, quantile.type = 2)
sd(dataD1UG$KL)

##### DESKRIPTION zu Aufgabe 2

  # Konzentrationsleistung
hist(dataD1$KL,
     main = "Hist KL Durchgang 1 GES",
     #xlim = c(-30, 20),
     #ylim = c(0, 5),
     breaks = seq(-40, 30, 2)
     )

hist(dataD2$KL,
     main = "Hist KL Durchgang 2 GES",
     #xlim = c(-30, 20),
     #ylim = c(0, 5),
     breaks = seq(-40, 30, 2)
     )

summary(dataD1$KL, quantile.type = 2)
sd(dataD1$KL)
summary(dataD2$KL, quantile.type = 2)
sd(dataD1$KL)

  # Schauen ob der Quotient NV ist
hist(dataD1$KL / dataD2$KL)
shapiro.test(dataD1$KL / dataD2$KL)

  # Bearbeitungszeit

hist(dataD1$B,
     main = "Hist B Durchgang 1 GES",
     xlim = c(40, 250))

hist(dataD2$B,
     main = "Hist B Durchgang 2 GES")

####################################################
############# ENDE DESKRIPTION #####################
####################################################


# Extremwert (aus allen Datensaetzen) entfernen
data = data[- which(data$id == 14), ]

{
  dataG1 = data[data$gruppe == 1, ]
  dataG2 = data[data$gruppe == 2, ]
  dataG1D1 = dataG1[dataG1$durchgang == 1, ]
  dataG1D2 = dataG1[dataG1$durchgang == 2, ]
  dataG2D1 = dataG2[dataG2$durchgang == 1, ]
  dataG2D2 = dataG2[dataG2$durchgang == 2, ]
  dataD1 = data[data$durchgang == 1, ]
  dataD2 = data[data$durchgang == 2, ]
  dataD1GU = dataD1[dataD1$test_typ == "gu", ]
  dataD1UG = dataD1[dataD1$test_typ == "ug", ]
}

n = length(data$id) / 2
n

### Tests fuer Aufgabe 1 ###

# TODO



### Test fuer Aufgabe 2 ###

## Konzentrationsleistung

# TODO

## Bearbeitungszeit

  # Idee: Differenz D2 - D1 testen auf kleiner null?
deltaB = dataD2$B - dataD1$B
hist(deltaB,
     breaks = seq(-100, 60, 10) 
     )
summary(deltaB, quantile.type = 2)
sd(deltaB)
  
  # Teste deltaB auf Normalverteilung
shapiro.test(deltaB)
  # p = 0.1187
  # -> H0 (deltaB normalverteilt) nicht verwerfbar

  # einstichproben t-Test auf
  # H0 deltaB >= 0  vs  H1: deltaB < 0
t.test(deltaB, alternative = "less", mu = 0)



  # AUFGABENTEIL 3

  # Bilde Differenz der KL innerhalb einzelner Gruppen
deltaKLG1 = dataG1D2$KL - dataG1D1$KL
deltaKLG2 = dataG2D2$KL - dataG2D1$KL

  # histogramme der Differenzen
hist(deltaKLG1)
hist(deltaKLG2)

  # Teste auf Normalverteilung  
shapiro.test(deltaKLG1)
shapiro.test(deltaKLG2)
  # NV Annahme (H0) kann nicht abgelehnt werden

  # Teste ob Varianzen gleich
var.test(deltaKLG1, deltaKLG2, alternative = "two.sided")
    # -> Varianzen gleich

  # Zwei-Stichproben t-Test
  # H0: deltaKLG1 <= deltaKLG2  vs  H1: deltaKLG1 > deltaKLG2
t.test(deltaKLG1, deltaKLG2, alternative = "greater",
       var.equal = TRUE)
    # p-Wert = 0.783
    # H0 kann nicht abgelehnt werden