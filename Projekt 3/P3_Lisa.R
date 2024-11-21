setwd("C:/Users/lisah/Documents/GitHub/fallstudien/Projekt 3")
data_raw = read.table("Kuckuckseier.txt", header = T)

{
# fuer pdfs exportieren
setwd("C:/Users/lisah/OneDrive/DSStudium/Fallstudien I")
data = list("WP" = data_raw[,1], 
            "BP" = data_raw[1:15,2], 
            "RK" = data_raw[1:16,3], 
            "ZK" = data_raw[1:15,4])

data_sep = data.frame("bird" = c(rep("WP",45),rep("BP",15),rep("RK",16),rep("ZK",15)),
                        length = c(data$WP,data$BP,data$RK,data$ZK))
  
}

### Deskription
apply(data_raw,2,summary, na.rm = T, quantile.type = 2)
apply(data_raw,2,sd, na.rm = T)
apply(data_raw,2,var,na.rm = T)
# in Bericht als Tabelle 

# Boxplot der Längen aufgeteilt nach Vogel
boxplot(length ~ bird, data = data_sep, horizontal = T)
stripchart(length ~ bird, data = data_sep, add = T, pch = 18, col = "darkblue")


### Testen auf NV-Annahme
shapiro.test(data$WP)
shapiro.test(data$BP)
shapiro.test(data$RK)
shapiro.test(data$ZK)
# alle p-Werte > 0.13 (shapiro ist besser für kleine n)

ks.test(data$WP,"pnorm",mean(data$WP),sd(data$WP))
ks.test(data$BP,"pnorm",mean(data$BP),sd(data$BP))
ks.test(data$RK,"pnorm",mean(data$RK),sd(data$RK))
ks.test(data$ZK,"pnorm",mean(data$ZK),sd(data$ZK))
# alle p-Werte > 0.28

### Abschlusstestverfahren

## Elementarhypothesen testen:

t.test(data$WP, data$BP)
t.test(data$WP, data$RK)
t.test(data$WP, data$ZK)
t.test(data$BP, data$RK)
t.test(data$BP, data$ZK)
t.test(data$RK, data$ZK)


t.test(data$WP, data$BP, var.equal = T)
t.test(data$WP, data$RK, var.equal = T)
t.test(data$WP, data$ZK, var.equal = T)
t.test(data$BP, data$RK, var.equal = T)
t.test(data$BP, data$ZK, var.equal = T)
t.test(data$RK, data$ZK, var.equal = T)

## 1-45,46-60,61-76,77-91
summary(aov(length ~ bird, data_sep[1:76,])) # 1 = 2 = 3
summary(aov(length ~ bird, data_sep[c(1:60,77:91),])) # 1 = 2 = 4
summary(aov(length ~ bird, data_sep[c(1:45,61:91),])) # 1 = 3 = 4
summary(aov(length ~ bird, data_sep[46:91,])) # 2 = 3 = 4


summary(aov(length~bird, data_sep))

