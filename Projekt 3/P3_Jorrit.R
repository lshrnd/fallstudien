##### Projekt 3 - Jorrit Kühne

# set working directory - ignore
setwd("C:/Users/jorri/Documents/GitHub/fallstudien/Projekt 3")

# read data
dataTable = read.table("Kuckuckseier.txt", header = TRUE)
dataTable

# extract groups
WP = dataTable$WP
BP = dataTable$BP[1:15]
RK = dataTable$RK[1:16]
ZK = dataTable$ZK[1:15]

# transform data structure
data = data.frame(c(WP, BP, RK, ZK), c(rep("WP", 45), rep("BP", 15),
                                         rep("RK", 16), rep("ZK", 15)))

names(data) = c("eggLength", "hostBird")


##### DESKRIPTION

### Kennzahlen
summWP = summary(WP, quantile.type = 2)
summWP
summWP[5] - summWP[2] # iqr
sd(WP)
summBP = summary(BP, quantile.type = 2)
summBP
summBP[5] - summBP[2] # iqr
sd(BP)
summRK = summary(RK, quantile.type = 2)
summRK
summRK[5] - summRK[2] # iqr
sd(RK)
summZK = summary(ZK, quantile.type = 2)
summZK
summZK[5] - summZK[2] # iqr
sd(ZK)

summAll = summary(data$eggLength, quantile.type = 2)
summAll
summAll[5] - summAll[2] # iqr
sd(data$eggLength)

### Parallele Boxplots
{
par(mar=c(4.5, 4.6, 0, 0) + 0.1)
boxplot(eggLength ~ hostBird,
        data = data,
        horizontal = F,
        xlab = "Wirtsvogelart",
        ylab = "Größe der Eier (mm)",
        cex.lab = 1.4,
        cex.axis = 1.4,
        varwidth = TRUE
        # outline= F,
        # ylim = c(19,25)
)
stripchart(eggLength ~ hostBird, 
           data = data, 
           add = T, 
           pch = 20,
           vertical = T, 
           method = "jitter", 
           jitter = 0.15,
           cex = 1.5)
}

### QQ-Plots
{
  par(mfrow = c(2, 2))
  par(mar = c(5, 5, 0, 0) + 0.1)
  # QQ-Plot Wiesenpieper
  qqnorm(WP,
         main = "",
         las = 1,
         xlab = "Theoretische Quantile",
         ylab = "Quantile WP",
         cex.lab = 1.5
  )
  abline(mean(WP), sd(WP))
  legend("bottomright",
         legend = c(paste("n = ", length(WP)),
                    paste("mean = ",round(mean(WP), 2)),
                    paste("sd = ", round(sd(WP), 2))),
         bty = "n"
  )
  # QQ-Plot Baumpieper
  qqnorm(BP,
         main = "",
         las = 1,
         xlab = "Theoretische Quantile",
         ylab = "Quantile BP",
         cex.lab = 1.5
  )
  abline(mean(BP), sd(BP))
  legend("bottomright",
         legend = c(paste("n = ", length(BP)),
                    paste("mean = ",round(mean(BP), 2)),
                    paste("sd = ", round(sd(BP), 2))),
         bty = "n"
  )
  # QQ-Plot Rotkehlchen
  qqnorm(RK,
         main = "",
         las = 1,
         xlab = "Theoretische Quantile",
         ylab = "Quantile RK",
         cex.lab = 1.5
  )
  abline(mean(RK), sd(RK))
  legend("bottomright",
         legend = c(paste("n = ", length(RK)),
                    paste("mean = ",round(mean(RK), 2)),
                    paste("sd = ", round(sd(RK), 2))),
         bty = "n"
  )
  # QQ-Plot Zaunkönig
  qqnorm(ZK,
         main = "",
         las = 1,
         xlab = "Theoretische Quantile",
         ylab = "Quantile ZK",
         cex.lab = 1.5
  )
  abline(mean(ZK), sd(ZK))
  legend("bottomright",
         legend = c(paste("n = ", length(ZK)),
                    paste("mean = ",round(mean(ZK), 2)),
                    paste("sd = ", round(sd(ZK), 2))),
         bty = "n"
  )
}



##### Testen der Globalhypothese

# H1234
aovH1234 = aov(eggLength ~ hostBird, data = data)
summary(aovH1234)
# p = 2.65e-07
# => Ablehnung zu alpha




##### Abschlußtestverfahren für Unterschiede ##### zu alpha = 0.05

### Teste Durchsschnittshypothesen mit Varianzanlyse
### auf Mittelwertgleichheit z.N. alpha = 0.05

# H123
aovH123 = aov(eggLength ~ hostBird, data = subset(data, data$hostBird != "ZK"))
summary(aovH123)
# p = 0.00305
# => Ablehnung zu alpha

# H124
aovH124 = aov(eggLength ~ hostBird, data = subset(data, data$hostBird != "RK"))
summary(aovH124)
# p = 8.56e-07
# => Ablehnung zu alpha

# H134
aovH134 = aov(eggLength ~ hostBird, data = subset(data, data$hostBird != "BP"))
summary(aovH134)
# p = 4.39e-05
# => Ablehnung zu alpha

# H234
aovH234 = aov(eggLength ~ hostBird, data = subset(data, data$hostBird != "WP"))
summary(aovH234)
# p = 4.43e-08
# => Ablehnung zu alpha

### Alle Durchschnittshypothesen werden zu alpha = 0.05 abgelehnt!

### Somit sind die Elementarhypothesen genau dann endgültig abzulehnen, wenn:
### 1) sie selbst ablehnen, und 
### 2) die kombinierten Hypothesen ablehnen, in denen sie vorkommen,
###    d.h. wenn sie selbst auch z.N. alpha / 2 ablehnt,
###         oder die komplementäre Hypothese z.N. alpha / 2 ablehnt (z.B. H12 zu H34)


### Elementarhypothesen (z.N. alpha und alpha / 2 [kominierte Hypothesen])

# H12 : mu(WP) = mu(BP)
t.test(WP, BP, var.equal = TRUE)
  # p = 0.001781
  # => Ablehnung zu alpha
  # => Ablehnung zu alpha / 2

  # H13 : mu(WP) = mu(RK)
  t.test(WP, RK, var.equal = TRUE)
    # p = 0.1039
  # => Keine Ablehnung zu alpha       => Keine endgültige Ablehnung möglich
  # => Keine Ablehnung zu alpha / 2           

# H14 : mu(WP) = mu(ZK)
t.test(WP, ZK, var.equal = TRUE)
  # p = 0.0004502
  # => Ablehnung zu alpha
  # => Ablehnung zu alpha / 2

# H23 : mu(BP) = mu(RK)
t.test(BP, RK, var.equal = TRUE)
  # p = 0.09326
  # => Keine Ablehnung zu alpha       => Keine endgültige Ablehnung möglich
  # => Keine Ablehnung zu alpha / 2           

# H24 : mu(BP) = mu(ZK)
t.test(BP, ZK, var.equal = TRUE)
  # p = 4.595e-07
  # => Ablehnung zu alpha
  # => Ablehnung zu alpha / 2

# H34 : mu(RK) = mu(ZK)
t.test(RK, ZK, var.equal = TRUE)
  # p = 3.469e-06
  # => Ablehnung zu alpha
  # => Ablehnung zu alpha / 2

### Somit folgt:

### H12 & H34 - lehnen beide zu aplha / 2 ab, und sind also endgültig abzulehnen

### H13 - lehnt zu alpha nicht ab, wird nicht endgültig abgelehnt

### H14 - lehnt zu alpha / 2 ab, ist somit endgültig abzulehnen

### H23 - lehnt zu alpha nicht ab, wird nicht endgültig abgelehnt

### H24 - lehnt zu alpha / 2 ab, ist somit endgültig abzulehnen


### Also: signifikanter Unterschied zwischen den Piepern
###       signifikanter Unterschied zwischen Kehlchen und König



##### Holm Prinzip ##### z.N. alpha = 0.05

# Globalhypothese siehe oben.
# Für Elementarhypothesen können Ergebnisse aus Abschlusstestverfahren
# genutzt werden, da hier selbiger Test durchgeführt wird.


