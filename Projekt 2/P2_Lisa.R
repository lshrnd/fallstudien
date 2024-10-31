setwd("C:/Users/lisah/Documents/GitHub/fallstudien/Projekt 2")

data = read.table("Konzentrationsdaten.txt", header = T)

library(lattice)
library(scales)

data_g1 = data[data$gruppe == 1,]
data_g2 = data[data$gruppe == 2,]

data_d1 = data[data$durchgang == 1,]
data_d2 = data[data$durchgang == 2,]



### Histogramm von KL fuer beide Gruppen fuer beide Durchgaenge (2x2)
par(mfrow=c(2,2), mar = c(2,2,2,2))
hist(data_g1$KL[data_g1$durchgang == 1], 
     main = "Gruppe 1, Durchgang 1", 
     breaks = seq(-40,30,2.5))
hist(data_g1$KL[data_g1$durchgang == 2], 
     main = "Gruppe 1, Durchgang 2", 
     breaks = seq(-40,30,2.5))
hist(data_g2$KL[data_g2$durchgang == 1], 
     main = "Gruppe 2, Durchgang 1", 
     breaks = seq(-40,30,2.5))
hist(data_g2$KL[data_g2$durchgang == 2], 
     main = "Gruppe 2, Durchgang 2", 
     breaks = seq(-40,30,2.5))

### Histogramm schoener nebeneinander (2x2) -- noch nicht gut gemacht
par(mfrow=c(1,1))
histogram(~ KL | durchgang + gruppe, 
          data = data,
          breaks = seq(-40,30,2.5),
          strip = strip.custom(strip.levels = c(TRUE,TRUE), 
                               strip.names = c(TRUE,TRUE),
                               bg = rep("lightgray",2), fg = rep("NA",2) 
          )
)

## meine favorites
cols_durchg = alpha(c("yellow","red"),0.5)
cols_durchg2 = alpha(c("#1E90FF","#FFA500"),c(1,0.5))

### Histogramm von KL fuer beide Gruppen (1x2)
par(mfrow=c(2,1), mar = c(2,2,2,2))
hist(data_g1$KL[data_g1$durchgang == 1], 
     main = "Gruppe 1 KL", 
     breaks = seq(-40,30,2.5),
     col = cols_durchg[1],
     freq = F)
hist(data_g1$KL[data_g1$durchgang == 2], 
     breaks = seq(-40,30,2.5),
     add = T,
     col = cols_durchg[2],
     freq = F)
hist(data_g2$KL[data_g2$durchgang == 1], 
     main = "Gruppe 2", 
     breaks = seq(-40,30,2.5),
     col = cols_durchg2[1],
     freq = F)
hist(data_g2$KL[data_g2$durchgang == 2], 
     breaks = seq(-40,30,2.5),
     add = T,
     col = cols_durchg2[2],
     freq = F)


# fuer Bearbeitungszeit analog 
par(mfrow=c(2,1), mar = c(2,2,2,2))
hist(data_g1$B[data_g1$durchgang == 1], 
     main = "Gruppe 1 B", 
     breaks = seq(55,205,5),
     col = cols_durchg[1],
     freq = F)
hist(data_g1$B[data_g1$durchgang == 2], 
     breaks = seq(55,205,5),
     add = T,
     col = cols_durchg[2],
     freq = F)
hist(data_g2$B[data_g2$durchgang == 1], 
     main = "Gruppe 2", 
     breaks = seq(55,205,5),
     col = grey(0.5,0.5),
     freq = F)
hist(data_g2$B[data_g2$durchgang == 2], 
     breaks = seq(55,205,5),
     add = T,
     col = grey(0,0.5),
     freq = F)


### Barplot von AR fuer beide Gruppen (1x2)
par(mfrow=c(2,1), mar = c(2,2,2,2))
ar_g1d1 = table(factor(data_g1$AR[data_g1$durchgang == 1], 
                       levels = 10:25))
ar_g1d2 = table(factor(data_g1$AR[data_g1$durchgang == 2], 
                       levels = 10:25))
barplot(rbind(ar_g1d1,ar_g1d2), 
        beside = T, 
        col = cols_durchg, 
        main = "Gruppe 1 AR",
        ylim = c(0,10))

ar_g2d1 = table(factor(data_g2$AR[data_g2$durchgang == 1], 
                       levels = 10:25))
ar_g2d2 = table(factor(data_g2$AR[data_g2$durchgang == 2], 
                       levels = 10:25))
barplot(rbind(ar_g2d1,ar_g2d2), 
        beside = T, 
        col = cols_durchg, 
        main = "Gruppe 2",
        ylim = c(0,10))

# fuer AA analog
par(mfrow=c(2,1), mar = c(2,2,2,2))
aa_g1d1 = table(factor(data_g1$AA[data_g1$durchgang == 1], 
                       levels = 0:16))
aa_g1d2 = table(factor(data_g1$AA[data_g1$durchgang == 2], 
                       levels = 0:16))
barplot(rbind(aa_g1d1,aa_g1d2), 
        beside = T, 
        col = cols_durchg, 
        main = "Gruppe 1 AA",
        ylim = c(0,10))

aa_g2d1 = table(factor(data_g2$AA[data_g2$durchgang == 1], 
                       levels = 0:16))
aa_g2d2 = table(factor(data_g2$AA[data_g2$durchgang == 2], 
                       levels = 0:16))
barplot(rbind(aa_g2d1,aa_g2d2), 
        beside = T, 
        col = cols_durchg, 
        main = "Gruppe 2",
        ylim = c(0,10))

## fuer AF analog
par(mfrow=c(2,1), mar = c(2,2,2,2))
af_g1d1 = table(factor(data_g1$AF[data_g1$durchgang == 1], 
                       levels = c(0:3,40:45)))
af_g1d2 = table(factor(data_g1$AF[data_g1$durchgang == 2], 
                       levels = c(0:3,40:45)))
barplot(rbind(af_g1d1,af_g1d2), 
        beside = T, 
        col = cols_durchg, 
        main = "Gruppe 1 AF",
        ylim = c(0,20))

af_g2d1 = table(factor(data_g2$AF[data_g2$durchgang == 1], 
                       levels = 0:50))
af_g2d2 = table(factor(data_g2$AF[data_g2$durchgang == 2], 
                       levels = 0:50))
barplot(rbind(af_g2d1,af_g2d2), 
        beside = T, 
        col = cols_durchg, 
        main = "Gruppe 2",
        ylim = c(0,20))


############################################################################## 
##############################################################################  
# Testen, ab hier mit Bereinigung des Ausreissers
data[which(data$id == 14),]

dataN = data[- which(data$id == 14),]

dataN_g1 = dataN[dataN$gruppe == 1,]
dataN_g2 = dataN[dataN$gruppe == 2,]

dataN_d1 = dataN[dataN$durchgang == 1,]
dataN_d2 = dataN[dataN$durchgang == 2,]


## Aufgabe 1: durchgang 1 - gruppen unterscheidung 
### Test 1 shapiro auf normalverteilungsannahme
shapiro.test(dataN_g1$KL[dataN_g1$durchgang == 1])
shapiro.test(dataN_g2$KL[dataN_g2$durchgang == 1])

### Test 2 f test auf gleiche varianzen
var.test(dataN_g1$KL[dataN_g1$durchgang == 1],
         dataN_g2$KL[dataN_g2$durchgang == 1])

#### test 3 zweistichproben t test auf gleiche mittelwerte
t.test(dataN_g1$KL[dataN_g1$durchgang == 1], dataN_g2$KL[dataN_g2$durchgang == 1], 
       var.equal = F)


## Aufgabe 2 - gruppen ignorieren, durchgaenge unterscheidung, KL
### Test 1 shapiro auf normalverteilungsannahme
shapiro.test(dataN_d1$KL) 
shapiro.test(dataN_d2$KL)

### Test 2 f test auf gleiche varianzen
var.test(dataN_d1$KL, dataN_d2$KL, 
         alternative = "two.sided")
#### test 3 zweistichproben t test auf gleiche mittelwerte
t.test(dataN_d1$KL, dataN_d2$KL, 
       alternative = "less", 
       paired = T)

### Test 1 shapiro auf normalverteilungsannahme
shapiro.test(dataN_d1$B)
shapiro.test(dataN_d2$B) # nope

## Idee: differenzen bilden und darauf 
deltaB = dataN_d2$B - dataN_d1$B

hist(deltaB, main = "Veraenderung in sek")
shapiro.test(deltaB)

### Test 3 t test auf mittelwert 
t.test(deltaB, alternative = "less")


### Aufgabe 3
# Idee I: Aufgabe 2 aufteilen in die beiden Gruppen --> iwi sinnlos
# Idee II: Differenz der Scores f?r beide Gruppen, 
#       dann die beiden vergleichen mit einseitige hypothese

# Idee II:
deltaKL_g1 = dataN_g1$KL[dataN_g1$durchgang == 2] - dataN_g1$KL[dataN_g1$durchgang == 1]
deltaKL_g2 = dataN_g2$KL[dataN_g2$durchgang == 2] - dataN_g2$KL[dataN_g2$durchgang == 1]
# je groesser, desto besser der score beim 2. mal, desto groesser die verbesserung 

# Test 1 auf Normalverteilung
shapiro.test(deltaKL_g1)
shapiro.test(deltaKL_g2)

# Test 2 auf gleiche Varianzen
var.test(deltaKL_g1,deltaKL_g2, 
         alternative = "two.sided")

# Test 3 auf hoeheren Mittelwert
t.test(deltaKL_g1,deltaKL_g2, 
       alternative = "greater", 
       var.equal = T)

