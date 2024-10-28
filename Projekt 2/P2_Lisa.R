setwd("C:/Users/lisah/Documents/GitHub/fallstudien/Projekt 2")

data = read.table("Konzentrationsdaten.txt", header = T)
data = data[- which(data$id == 14),]

library(scales)

data_g1 = data[data$gruppe == 1,]
data_g2 = data[data$gruppe == 2,]

data_d1 = data[data$durchgang == 1,]
data_d2 = data[data$durchgang == 2,]

# Histogramm von KL für jede Gruppe den ersten Durchgang
hist(data_g1$KL[data_g1$durchgang == 1], main = "G1, D1", breaks = seq(-40,30,5))
hist(data_g2$KL[data_g2$durchgang == 1], main = "G2, D1", breaks = seq(-40,30,5))

# Histogramm von KL
hist(data_d1$KL, breaks = seq(-40,30,5))
hist(data_d2$KL, add = T, col = alpha("red",0.25), breaks = seq(-40,30,5))


sd(data_g1$KL[data_g1$durchgang == 1])
sd(data_g2$KL[data_g2$durchgang == 1])

##############################################################################  


## Aufgabe 1: durchgang 1 - gruppen unterscheidung 
### Test 1 shapiro auf normalverteilungsannahme
shapiro.test(data_g1$KL[data_g1$durchgang == 1])
shapiro.test(data_g2$KL[data_g2$durchgang == 1])

### Test 2 f test auf gleiche varianzen
var.test(data_g1$KL[data_g1$durchgang == 1],data_g2$KL[data_g2$durchgang == 1])

#### test 3 zweistichproben t test auf gleiche mittelwerte
t.test(data_g1$KL[data_g1$durchgang == 1],data_g2$KL[data_g2$durchgang == 1], var.equal = F)


## Aufgabe 2 - gruppen ignorieren, durchgänge unterscheidung, KL
### Test 1 shapiro auf normalverteilungsannahme
shapiro.test(data_d1$KL) 
shapiro.test(data_d2$KL)
### Test 2 f test auf gleiche varianzen
var.test(data_d1$KL,data_d2$KL, alternative = "two.sided")
#### test 3 zweistichproben t test auf gleiche mittelwerte
t.test(data_d1$KL,data_d2$KL, alternative = "less")

### Test 1 shapiro auf normalverteilungsannahme
shapiro.test(data_d1$B)
shapiro.test(data_d2$B) # 

## Idee: differenzen bilden und darauf 
deltaB = data_d2$B - data_d1$B

hist(deltaB, main = "Veränderung in sek")
shapiro.test(deltaB)

### Test 3 t test auf mittelwert 
t.test(deltaB, alternative = "less")


### Aufgabe 3
# Idee I: Aufgabe 2 aufteilen in die beiden Gruppen --> iwi sinnlos
# Idee II: Differenz der Scores für beide Gruppen, 
#       dann die beiden vergleichen mit zweiseitge hypothese

# Idee II:
deltaKL_g1 = data_g1$KL[data_g1$durchgang == 2] - data_g1$KL[data_g1$durchgang == 1]
deltaKL_g2 = data_g2$KL[data_g2$durchgang == 2] - data_g2$KL[data_g2$durchgang == 1]
# je größer, desto besser der score beim 2. mal, desto größer die verbesserung 

# Test auf Normalverteilung
shapiro.test(deltaKL_g1)
shapiro.test(deltaKL_g2)

# Test auf gleiche Varianzen
var.test(deltaKL_g1,deltaKL_g2, alternative = "two.sided")

# Test auf gleiche Mittelwerte
t.test(deltaKL_g1,deltaKL_g2, alternative = "greater", var.equal = T)
