setwd("C:/Users/Nils Mac/Documents/GitHub/fallstudien/Projekt 2")
data = read.table("Konzentrationsdaten.txt", header = T)

library(scales)

data_g1 = data[data$gruppe == 1,]
data_g2 = data[data$gruppe == 2,]

data_d1 = data[data$durchgang == 1,]
data_d2 = data[data$durchgang == 2,]


### Histogramm von KL fuer beide Gruppen fuer beide Durchgaenge
### für Bearbeitungszeit auch

### je nach dem für differenzen oder quotienten, je nachdem wie wir testen wollen


plot(data_d1$KL, data_d2$KL, 
     xlim = c(-30,20), ylim = c(-40,30)
     )
abline(coef = c(0,1))
# Einfärben nach Gruppe?

hist(data_d2$KL[data_d2$gruppe == 1] - data_d1$KL[data_d2$gruppe == 1], breaks = seq(-16,10,2), col = "blue")
hist(data_d2$KL[data_d2$gruppe == 2] - data_d1$KL[data_d2$gruppe == 2], breaks = seq(-16,10,2), add = T, col = alpha("yellow",0.5))
