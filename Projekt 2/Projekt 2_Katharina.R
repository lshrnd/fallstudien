data<- read.table("Konzentrationsdaten.txt", header = T)
mar_def = c(5, 4, 4, 2) + 0.1 # Default-Margin

#Nur Daten von Durchgang 1
datad1<-data[data$durchgang==1,]

#Deskription

#Gemeinsames Histogramm für KL nach beiden Gruppen
par(mar=c(4.5,4.5,1,0.5))
hist(datad1$KL[datad1$gruppe==1], breaks = 20, xlim =c(-25,20),
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
