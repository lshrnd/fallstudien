##### Projekt 3 - Jorrit Kühne

library(DescTools)
library(MASS)

setwd("C:/Users/jorri/Documents/GitHub/fallstudien/Projekt 3")

dataTable = read.table("Kuckuckseier.txt", header = TRUE)

dataTable

WP = dataTable$WP
BP = dataTable$BP[1:15]
RK = dataTable$RK[1:16]
ZK = dataTable$ZK[1:15]

data = data.frame(c(WP, BP, RK, ZK), c(rep("WP", 45),
                                         rep("BP", 15),
                                         rep("RK", 16),
                                         rep("ZK", 15)))
names(data) = c("eggLength", "hostBird")


hist(WP,
     xlim = c(19,25))
hist(BP,
     xlim = c(19,25))
hist(RK,
     xlim = c(19,25))
hist(ZK,
     xlim = c(19,25))


qqnorm(WP)
qqnorm(BP)
qqnorm(RK)
qqnorm(ZK)


stripchart(WP, vertical = TRUE)

##### Abschlußtestverfahren für Unterschiede ##### zu alpha = 0.05

### Elementarhypothesen (z.N. alpha und alpha / 2 [kominierte Hypothesen])

# H12 : mu(WP) = mu(BP)
t.test(WP, BP)
  # p = 0.002157
  # => Ablehnung zu alpha
  # => Ablehnung zu alpha / 2

# H13 : mu(WP) = mu(RK)
t.test(WP, RK)
  # p = 0.05926
  # => Keine Ablehnung zu alpha       => Keine endgültige Ablehnung möglich
  # => Keine Ablehnung zu alpha / 2           

# H14 : mu(WP) = mu(ZK)
t.test(WP, ZK)
  # p = 0.0001617
  # => Ablehnung zu alpha
  # => Ablehnung zu alpha / 2

# H23 : mu(BP) = mu(RK)
t.test(BP, RK)
  # p = 0.0972
  # => Keine Ablehnung zu alpha       => Keine endgültige Ablehnung möglich
  # => Keine Ablehnung zu alpha / 2           

# H24 : mu(BP) = mu(ZK)
t.test(BP, ZK)
  # p = 5.543e-07
  # => Ablehnung zu alpha
  # => Ablehnung zu alpha / 2

# H34 : mu(RK) = mu(ZK)
t.test(RK, ZK)
  # p = 3.836e-06
  # => Ablehnung zu alpha
  # => Ablehnung zu alpha / 2


### Teste Durchsschnittshypothesen mit Varianzanlyse
### auf Mittelwertgleichheit z.N. alpha = 0.05

# TODO: Vortest auf gleiche Varianzen ?

# H1234
aovH1234 = aov(eggLength ~ hostBird, data = data)
summary(aovH1234)
  # p = 2.65e-07
  # => Ablehnung zu alpha

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

### Alle Durchschnittshypothesen werden lehnen zu alpha = 0.05 jeweiliges H0 ab!

### Somit sind die Elementarhypothesen genau dann endgültig abzulehnen, wenn:
### 1) sie selbst ablehnen, und 
### 2) die kombinierten Hypothesen ablehnen, in denen sie vorkommen,
###    d.h. wenn sie selbst auch z.N. alpha / 2 ablehnt,
###         oder die komplementäre Hypothese z.N. alpha / 2 ablehnt (z.B. H12 zu H34)


### Somit folgt:

### H12 & H34 - lehnen beide zu aplha / 2 ab, und sind also endgültig abzulehnen

### H13 - lehnt zu alpha nicht ab, wird nicht endgültig abgelehnt

### H14 - lehnt zu alpha / 2 ab, ist somit endgültig abzulehnen

### H23 - lehnt zu alpha nicht ab, wird nicht endgültig abgelehnt

### H24 - lehnt zu alpha / 2 ab, ist somit endgültig abzulehnen


### Also: signifikanter Unterschied zwischen den Piepern
###       signifikanter Unterschied zwischen Kehlchen und König



##### Scheffe Methode ##### z.N. alpha = 0.05

### a-priori Anova

# H1234
aovH1234 = aov(eggLength ~ hostBird, data = data)
summary(aovH1234)
  # p = 2.65e-07
  # => Ablehnung zu alpha

### post-hoc : Wo sind Unterschiede?

ScheffeTest(aovH1234)

### Leite mit Scheffe Methode (simultane KIs z.N. alpha her)
alpha = 0.05

# Hypothesen von Interesse - alle Elementarhypothesen

y = data$eggLength
  # Designmatrix
X = matrix(0, nrow = 91, ncol = 4)
X[1:45, 1] = 1
X[46:60, 2] = 1
X[61:76, 3] = 1
X[77:91, 4] = 1
#X = X[, 2:5]
X
n = length(X[ , 1])
p = length(X[1, ])
  # MoorePenroseInverse
#mpiX = ginv(X)
  # inverse
invXtX = solve(t(X) %*% X)
  # KQ-Schätzung
beta_hat = invXtX %*% t(X) %*% y
beta_hat
  # Varianschätzung
sigma2_hat = ( t(y) %*% ( diag(n) - X %*% invXtX %*% t(X) ) %*% y ) / (n - p)
sigma2_hat
  # t-Quantil df = n - p, alpha = 0.05
#tq005 = qt(alpha, n - p)
  # alpha-Quantil d. F-Vtlg mit 1, n - p df
fq005 = qf(alpha, 1, n - p)

  # function W
  # Short: calculates teststatistic W (after miller p.50) for 1-dimensional L
  #        and the values for this problem
  # result has to be compared with alpha-Quantil of fitting F distribution
W = function(L, beta_est = beta_hat, ginvX = mpiX, sigma2_est = sigma2_hat) {
  '( (L %*% beta_est) %*%
        solve( t(L) %*% invXtX %*% L) %*%
        (L %*% beta_est) ) / sigma2_est
  '
  (L %*% beta_hat)^2 * ( t(L) %*% invXtX %*% L ) / ( sigma2_hat )
}

# H12
l12 = c(1, -1, 0, 0)   
              # hat rang 1, also d = 1, daher ist Wurzel aus Teststatistik
              # in miller S.50 unten t-verteilt mit n - p freiheitsgraden

  # berechne t-verteilte Teststatistik - per Hand
TS = ( t(l12 %*% beta_hat) %*% 
        solve(t(l12) %*% ginv(t(mpiX) %*% (mpiX)) %*% l12) 
      %*% t(l12 %*% beta_hat)) / (sigma2_hat)  # miller S.50
sqrt(TS)
tq005  

(l12 %*% beta_hat)^2 * ( t(l12) %*% invXtX %*% l12 ) / ( sigma2_hat )



  # mit fct
W(l12)

fq005
# sqrt(TS) > t_{alpha, n-p}
#   => H_0 wird abgelehnt

# miller S.49 unten : 1-dim
plusMinus = sqrt(sigma2_hat) * qt(0.05 / 2, n - p) * sqrt( t(l12) %*% invXtX %*% l12 )
  
l12 %*% beta_hat - plusMinus
l12 %*% beta_hat + plusMinus

# H13
l13 = c(1, 0, -1, 0)
W(l13)
fq005

# H14
l14 = c(1, 0, 0, -1)
W(l14)

# H23
l23 = c(0, 1, -1, 0)
W(l23)

# H24
l24 = c(0, 1, 0, -1)
W(l24)

# H34
l34 = c(0, 0, 1, -1)
W(l34)

fq005
