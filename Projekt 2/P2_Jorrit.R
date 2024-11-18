##### Fallstudien 1 - Projekt 2
##### Autor: Jorrit Kuehne
#####

library(scales)

setwd("C:/Users/jorri/Documents/GitHub/fallstudien/Projekt 2")

data = read.table("Konzentrationsdaten.txt", header = T)

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

################## DESKRIPTION ALLER VARIABLEN (UNIVARIAT) #####################

    # gruppe
table(data$gruppe)

    # durchgang (irrelevant -> fuer Jede id liegen 2 Durchgaenge vor)
table(data$durchgang)

    # test_typ (irrelevant -> ableitbar aus gruppe und durchgang)
table(data$test_typ)

    # B (Bearbeitungszeit)

# Kennzahlen
summB = summary(data$B, quantile.type = 2)
summB
sdB = sd(data$B)
sdB

# Histogramm
{
hist(data$B,
     main = "",
     xlim = c(40, 220),
     ylim = c(0,30),
     xlab = "Bearbeitungszeit (s)",
     ylab = "abs. Hfkt",
     xaxt = "n"
     )
axis(1, at = seq(40, 220, 20))
}

# Boxplot
boxplot(data$B,
        horizontal = TRUE,
        xlab = "Bearbeitungszeit (s)"
        )

# 1-D Streudiagramm
stripchart(data$B,
           method = "jitter",
           pch = 16,
           col = alpha("blue", 0.3),
           #vertical = TRUE,
           add = TRUE
           )

    # AR (Anzahl Richtige)

# Kennzahlen
summary(data$AR, quantile.type = 2)
sd(data$AR)

# HauefigkeitsTabelle (gut)
htAR = table(data$AR)
htAR

plot(htAR) # (??)

# Balkendiagramm (gut)
barplot(table(data$AR),
        ylim = c(0, 25),
        xlab = "Anzahl Richtige",
        ylab = "abs. Hfkt"
        )

# Histogramm (hier eher nicht sinnvoll)
{
hist(data$AR,
     ylim = c(0, 30),
     xaxt = "n",
     main = "",
     ylab = "abs. Hfkt",
     xlab = "Anzahl Richtige"
     )
axis(1, at = seq(10, 26, 4))
}

# Boxplot (hier eher nicht sinnvoll)
boxplot(data$AR,
        horizontal = TRUE,
        xlab = "Anzahl Richtige"
        )

# 1-D Streudiagramm (??)
stripchart(data$AR,
           method = "stack",
           pch = 16,
           col = alpha("blue", 0.3),
           #vertical = TRUE,
           #add = TRUE
           )

    # AA (Anzahl Auslassungen) <=> 25 - AR

# Kennzahlen
summary(data$AA, quantile.type = 2)
sd(data$AA)

# HauefigkeitsTabelle ("spiegelt" die von AR)
htAA = table(data$AA)
htAA

# Balkendiagramm ("spiegelt" das von AR)
barplot(table(data$AA),
        ylim = c(0, 25),
        xlab = "Anzahl Auslassungen",
        ylab = "abs. Hfkt"
        )

    # AF (Anzahl Falscher)

# Kennzahlen
summary(data$AF, quantile.type = 2)
sd(data$AF)

# Haeufigkeitstabelle
htAF = table(data$AF)
htAF

# Balkendiagramm
barplot(table(data$AF))


# Versuch von stacked Barplots
m = matrix(data = c(rev(htAR), htAA, htAF, rep(0, 6)), ncol = 3)
t = as.table(m)
colnames(t) = c("AR", "AA", "AF")
t
.
barplot(t,
        horiz = TRUE,
        las = 1,
        #col = c("red", "blue")
        xaxt = "n"
        )

text(x = cumsum(rev(htAR)) - 1, 
     y = 1,
     labels = rev(names(htAR))
     )
text(x = cumsum(htAA) - 1,
     y = 2,
     labels = names(htAA)
     )
text(x = cumsum(htAF) - 1,
     y = 3,
     labels = names(htAF)
     )
.

    # KL (Konzentrationsleistung)

# Kennzahlen
summary(data$KL, quantile.type = 2)
sd(data$KL)

# Boxplot
boxplot(data$KL,
        horizontal = TRUE,
        xlab = "Konzentrationsleistung"
        )

# Stripchart
stripchart(data$KL,
           method = "jitter",
           pch = 16,
           col = alpha("blue", 0.5),
           xlab = "Konzentrationsleistung",
           add = TRUE
           )

# Histogramm
hist(data$KL,
     breaks = seq(-40, 30, 5))

# Spielerei
#plot(data$KL)
#points(lowess(data$KL), type = "l", col = "red")
#index = 1:82
#abline(lm(data$KL ~ index)$coefficients, col = "blue")

################################################################################

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

  ### Konzentrationsleistung
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

# D1
summary(dataD1$KL, quantile.type = 2)
sd(dataD1$KL)
# D2
summary(dataD2$KL, quantile.type = 2)
sd(dataD1$KL)
# Schauen ob der Quotient NV ist (unnoetig - da 2 stichproben t-test moegl.)
hist(dataD1$KL / dataD2$KL)
shapiro.test(dataD1$KL / dataD2$KL)


  ### Bearbeitungszeit
hist(dataD1$B,
     main = "Hist B Durchgang 1 GES",
     xlim = c(40, 250)
     )

hist(dataD2$B,
     main = "Hist B Durchgang 2 GES"
     )

# D1
summary(dataD1$B, quantile.type = 2)
sd(dataD1$B)
# D2
summary(dataD2$B, quantile.type = 2)
sd(dataD2$B)

##### DESKRIPTION zu Aufgabe 3: Unterschied in Verbesserung der KL?

  # siehe unten Tests zu Aufgabe 3


####################################################
############# ENDE DESKRIPTION #####################
####################################################


# Extremwert (aus allen Datensaetzen) entfernen
{
  data = data[- which(data$id == 14), ]
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

qqnorm(deltaB)
abline(mean(deltaB), sd(deltaB))

  # AUFGABENTEIL 3

  # Bilde Differenz der KL innerhalb einzelner Gruppen
deltaKLG1 = dataG1D2$KL - dataG1D1$KL
deltaKLG2 = dataG2D2$KL - dataG2D1$KL

  # DESKRIPTION der Differenzen
  # Histogramme
hist(deltaKLG1)
hist(deltaKLG2)
  # Kennzahlen
summary(deltaKLG1, quantile.type = 2)
sd(deltaKLG1)
summary(deltaKLG2, quantile.type = 2)
sd(deltaKLG2)

  # TESTEN der Hypothese

  # TEST auf Normalverteilung  
shapiro.test(deltaKLG1)
shapiro.test(deltaKLG2)
  # NV Annahme (H0) kann nicht abgelehnt werden

  # TEST ob Varianzen gleich
var.test(deltaKLG1, deltaKLG2, alternative = "two.sided")
    # -> Varianzen gleich

  # TEST Zwei-Stichproben t-Test
  # H0: deltaKLG1 <= deltaKLG2  vs  H1: deltaKLG1 > deltaKLG2
t.test(deltaKLG1, deltaKLG2, alternative = "greater",
       var.equal = TRUE)
    # p-Wert = 0.783
    # H0 kann nicht abgelehnt werden

#########################################################


#quantile(dataD1GU$KL, type = 2)

## normal QQ-Plot:

# y-Koordinate: Sample Quantile / sorted Sample Values
# x-Koordinate: theoretische p-Quantile der N(0,1) Vtlg,
#               für p entsprechend dem p der sampleQuantile (berechnet mit ppoints)

# INTERPRETATION:
#
# Wenn die Punkte auf der Winkelhalbierenden liegen kann man eine N(0,1) Vtlg vermuten
# Wenn die Punkte auf einer Linie liegen dann eine lineare Trafo der N(0,1) Vtlg
#   d.h. N(mu, sigma)

#### Wie geht ein QQ Plot? #####
qqnorm(dataD1GU$KL)
qqline(dataD1GU$KL,
       probs = c(0.25, 0.75),
       distribution = qnorm,
       qtype = 2
       )

sq = sort(dataD1GU$KL)  # these are the sample quantiles "y-positions"
ppoints(20) # gibt das "p" an (aus p-Quantil) für das theoretisches Quantil
            # berechnet werden soll, welches dann die "x-Position" darstellt
tq = qnorm(ppoints(20))  # gibt die x-Position an
# hier 20, weil lengtH(dataD1GU$KL) = 20 

plot(tq, sq)  # entspricht plot aus qqnorm


# Idee gerade aus mittelwert = y-Achsenacbschnitt und sd = Steigung
# als Referenz
abline(mean(dataD1GU$KL), 
       sd(dataD1GU$KL),
       col = "red"
       )


  # Lineares Modell und einfacher Konfidenzstreifen für qq plot von dataD1GU$KL
  # wäre viel zu kompliziert und würde methodenteil zu lang machen und ist
  # außerdem mega unnötig - da ich im lm ja auch eine NV annehme...
qqModel = lm(sq ~ tq)
qqModel$coefficients
abline(qqModel$coefficients, col = "blue")

sigma_sq_hat_2 = sum(qqModel$residuals^2) / (20 - 1 - 1)

X = cbind(1, sq)
C2 = solve(t(X) %*% X)

alpha = 0.05
tquantile = qt(1 - alpha / 2, df = 20 - 1 - 1)
g1 = function(xStar) {
  tquantile * sqrt(sigma_sq_hat_2 * (c(1, xStar) %*% C2 %*% c(1, xStar)))
}


tq_star = as.data.frame(tq)
sq_star = predict(qqModel, tq_star)

simpleKIStreifenLower = sq_star - sapply(tq_star$tq, g1) # Untere Grenze Einfacher KI Streifen
simpleKIStreifenUpper = sq_star + sapply(tq_star$tq, g1) # Obere  Grenze Einfacher KI Streifen

points(tq_star$tq, simpleKIStreifenLower, col = "black", type = "l")
points(tq_star$tq, simpleKIStreifenUpper, col = "black", type = "l")



qqplot(tq, sq, conf.level = 0.05)


### Versuch


m = summary(dataD1$KL)[4]
s = sd(dataD1$KL)
hist(dataD1$KL,
     freq = FALSE)

points(dnorm(seq(-10, 10, 0.5), 0, s), type = "l")
  


##### Versuch ###

qSd4 = qnorm(ppoints(20), sd = 4)

plot(tq, qSd4)
abline(0, 1)
abline(0, 4, col = "red")

eQ = qexp(ppoints(20))

plot(tq, eQ)

# qqline (qtype = 2 ! )
qqline(dataD1GU$KL,
       qtype = 2
)
