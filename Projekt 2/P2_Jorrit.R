getwd()

data = read.table("Konzentrationsdaten.txt", header = T)

n = length(data$id) / 2
n

# TODO: Extremwert entfernen in Gruppe 1

dataD1 = data[data$durchgang == 1, ]
dataD2 = data[data$durchgang == 2, ]
dataD1GU = dataD1[dataD1$test_typ == "gu", ]
dataD1UG = dataD1[dataD1$test_typ == "ug", ]

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

  # DESKRIPTION zu Aufgabe 2

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

  # Bearbeitungszeit

hist(dataD1$B,
     main = "Hist B Durchgang 1 GES")

hist(dataD2$B,
     main = "Hist B Durchgang 2 GES")

  # Differenz D2 - D1 testen auf kleiner null?
deltaB = dataD2$B - dataD1$B
hist(deltaB)
summary(deltaB, quantile.type = 2)
sd(deltaB)
  # Quotient testen auf kleiner 1?
quotB = dataD2$B / dataD1$B
hist(quotB)
