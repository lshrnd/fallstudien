data<- read.table("Konzentrationsdaten.txt", header = T)
mar_def = c(5, 4, 4, 2) + 0.1 # Default-Margin

#Nur Daten von Durchgang 1
datad1<-data[data$durchgang==1,]

#Wieviele Teilnehmer pro Gruppe
nrow(data[data$gruppe == 1 & data$durchgang == 1, ]) #Gruppe 1
nrow(data[data$gruppe == 2 & data$durchgang == 1, ]) #Gruppe 2
#Deskription

#Gemeinsames Histogramm für KL nach beiden Gruppen
par(mar=c(4.5,4.5,1,0.5))
hist(datad1$KL[datad1$gruppe==1], breaks = 20, xlim =c(-25,20), ylim =c(0,5),
     col=rgb(1,0,0,0.5), main ="", xlab = "Konzentrationsleistung", ylab="Häufigkeit",
     cex.axis=1.25, cex.lab=1.75)
hist(datad1$KL[datad1$gruppe==2], breaks = 5, xlim =c(-25,20), col=rgb(0,0,1,0.5), add=T)
axis(1, at = seq(-35, 20, by = 5), cex.axis = 1.25)
legend("topleft", legend=c("Gruppe 1", "Gruppe 2"), col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),
       pt.cex = 2, pch = 15, cex =1.75, bty="n", x.intersp =0.25)

#Gemeinsames Histogramm für Bearbeitungszeit nach Gruppe
par(mar=c(4.5,4.5,1,0.5))
hist(datad1$B[datad1$gruppe==1], breaks = 10, xlim =c(50,225),
     col=rgb(1,0,0,0.5), main ="", xlab = "Bearbeitungszeit in Sekunden", ylab="Häufigkeit",
     cex.axis=1.25, cex.lab=1.75)
hist(datad1$B[datad1$gruppe==2], breaks = 10, xlim =c(50,225), col=rgb(0,0,1,0.5), add=T)
axis(1, at = seq(50, 225, by = 25), cex.axis = 1.25)
legend("topright", legend=c("Gruppe 1", "Gruppe 2"), col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),
       pt.cex = 2, pch = 15, cex =1.75, bty="n", x.intersp =0.5)

#QQ-Plot Konzentrationsleistung mit Aussreiser Gruppe 1
par(mar=c(4.5,4.5,0.5,0.5))
qqnorm(datad1$KL[datad1$gruppe==1], main ="",
       cex.axis=1.25, cex.lab=1.75,
       xlab ="Theoretische Quantile", ylab ="empirische Quantile")
qqline(datad1$KL[datad1$gruppe==1])

#Ausreisser erkennen
data[which(data$KL<0),]
#Aussreisser entfernen
dataN = data[- which(data$id == 14),]
#Gruppenaufteilung
dataN_g1 = dataN[dataN$gruppe == 1,]
dataN_g2 = dataN[dataN$gruppe == 2,]
#Durchgang aufgeteilt
dataN_d1 = dataN[dataN$durchgang == 1,]
dataN_d2 = dataN[dataN$durchgang == 2,]

#QQ-Plot Konzentration Ausreisser entfernt Durchgang 1 Gruppe 1
par(mar=c(4.5,4.5,0.5,0.5))
qqnorm(dataN_d1$KL[dataN_d1$gruppe==1], main ="",
       cex.axis=1.25, cex.lab=1.75,
       xlab ="Theoretische Quantile", ylab ="empirische Quantile")
qqline(datad1$KL[datad1$gruppe==1])

#QQ-Plot Bearbeitungszeit Durchgang 1
par(mar=c(4.5,4.5,0.5,0.5))
qqnorm(dataN_d1$B, main ="",
       cex.axis=1.25, cex.lab=1.75,
       xlab ="Theoretische Quantile", ylab ="empirische Quantile")
qqline(dataN_d1$B)
#QQ-Plot Bearbeitungszeit Durchgang 2
par(mar=c(4.5,4.5,0.5,0.5))
qqnorm(dataN_d2$B, main ="",
       cex.axis=1.25, cex.lab=1.75,
       xlab ="Theoretische Quantile", ylab ="empirische Quantile")
qqline(dataN_d2$B)

############################################################################## 
##############################################################################  
# Testen, ab hier mit Bereinigung des Ausreissers

## Aufgabe 1: durchgang 1 - gruppen unterscheidung 
### Test 1 shapiro auf normalverteilungsannahme
#shapiro.test(dataN_g1$KL[dataN_g1$durchgang == 1])
#shapiro.test(dataN_g2$KL[dataN_g2$durchgang == 1])


#Shapiro-Francia
shapiro.francia<-function(x){
W <- cor(sort(x),
         qnorm(ppoints(x, a = 3/8)))^2
u <- log(length(x))
v <- log(u)
alpha <- -1.2725 + 1.0521 * (v - u)
beta <- 1.0308 - 0.26758 * (v + 2/u)
Z<- (log(1 - W) - alpha)/beta
pnorm(Z, lower.tail = FALSE)
}
#Testen auf Normalverteilungsannahme mit shaprio.francia
shapiro.francia(dataN_g1$KL[dataN_g1$durchgang == 1])
shapiro.francia(dataN_g2$KL[dataN_g2$durchgang == 1])

### Test 2 f test auf gleiche varianzen
var.test(dataN_g1$KL[dataN_g1$durchgang == 1],
         dataN_g2$KL[dataN_g2$durchgang == 1])

#### test 3 zweistichproben t test auf gleiche mittelwerte
t.test(dataN_g1$KL[dataN_g1$durchgang == 1], dataN_g2$KL[dataN_g2$durchgang == 1], 
       var.equal = T, alternative = "two.sided")


## Aufgabe 2 - gruppen ignorieren, durchgaenge unterscheidung, KL
### Test 1 shapiro auf normalverteilungsannahme
#shapiro.test(dataN_d1$KL) 
#shapiro.test(dataN_d2$KL)
#francia Variante
shapiro.francia(dataN_d1$KL) #Konzentration Durchgang 1
shapiro.francia(dataN_d2$KL) #Konzentration Durchgang 2

### Test 2 f test auf gleiche varianzen
var.test(dataN_d1$KL, dataN_d2$KL, 
         alternative = "two.sided")
#### test 3 zweistichproben t test auf gleiche mittelwerte
t.test(dataN_d1$KL, dataN_d2$KL, var.equal = T,
       alternative = "less", 
       paired = T)

### Test 1 shapiro auf normalverteilungsannahme Bearbeitungszeit
#shapiro.test(dataN_d1$B)
#shapiro.test(dataN_d2$B) # nope
#shaprio.francia
shapiro.francia(dataN_d1$B)
shapiro.francia(dataN_d2$B)

## Idee: differenzen bilden und darauf 
deltaB = dataN_d2$B - dataN_d1$B

hist(deltaB, main = "Veraenderung in sek")
#QQ-Plot Bearbeitungszeit Veränderung
par(mar=c(4.5,4.5,0.5,0.5))
qqnorm(deltaB, main ="",
       cex.axis=1.25, cex.lab=1.75,
       xlab ="Theoretische Quantile", ylab ="empirische Quantile")
qqline(deltaB)

#shapiro.test(deltaB)
shapiro.francia(deltaB)

### Test 3 t test auf mittelwert 
t.test(deltaB, alternative = "less", mu=0)


### Aufgabe 3
# Differenz der Scores für beide Gruppen, 
#       dann die beiden vergleichen mit einseitige hypothese

# 
deltaKL_g1 = dataN_g1$KL[dataN_g1$durchgang == 2] - dataN_g1$KL[dataN_g1$durchgang == 1]
deltaKL_g2 = dataN_g2$KL[dataN_g2$durchgang == 2] - dataN_g2$KL[dataN_g2$durchgang == 1]
# je groesser, desto besser der score beim 2. mal, desto groesser die verbesserung 

# Test 1 auf Normalverteilung
shapiro.francia(deltaKL_g1)
shapiro.francia(deltaKL_g2)

# Test 2 auf gleiche Varianzen
var.test(deltaKL_g1,deltaKL_g2, 
         alternative = "two.sided")

# Test 3 auf hoeheren Mittelwert
t.test(deltaKL_g1,deltaKL_g2, 
       alternative = "greater", 
       var.equal = T)
