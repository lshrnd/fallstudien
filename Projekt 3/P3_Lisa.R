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

data_sep$bird = factor(data_sep$bird, levels = c("WP","BP","RK","ZK"))

kennzahlen = function(x){
  c("min" = min(x, na.rm = T),
    "max" = max(x, na.rm = T),
    "mean" = mean(x, na.rm = T),
    "var" = var(x, na.rm = T),
    "iqr" = IQR(x, type = 2, na.rm = T)
    )
}

}

### Deskription
apply(data_raw,2,kennzahlen)

# Boxplot der L�ngen aufgeteilt nach Vogel
pdf(file  = "3_Boxplot.pdf", width = 10)
par(mar=c(4.5, 5, 1.5, 2), family = "serif")

boxplot(length ~ bird, 
        data = data_sep, 
        horizontal = F,
        xlab = "Vogelart",
        ylab = "L�nge in mm",
        cex.lab = 1.6,
        cex.axis = 1.6
        # outline= F,
        # ylim = c(19,25)
        )

stripchart(length ~ bird, 
           data = data_sep, 
           add = T, 
           pch = 20,
           vertical = T, 
           method = "jitter", 
           jitter = 0.15,
           cex = 1.6)

dev.off()

# QQ-Plots der Stichproben
pdf(file = "3_QQPlots.pdf", width = 12, height = 10)
{
  par(mfrow=c(2,2), mar = c(5.5, 6, 2, 2), family = "serif")
  qqnorm(data$WP,
         main = "Wiesenpieper",
         xlab = expression("theoretische Quantile von" ~ N(0,1)),
         ylab = "Quantile der Stichprobe",
         cex.lab = 2.2,
         cex.axis = 2.2,
         cex.main = 2.2)
  abline(mean(data$WP),
         sd(data$WP))
  
  
  qqnorm(data$BP,
         main = "Baumpieper",
         xlab = expression("theoretische Quantile von" ~ N(0,1)),
         ylab = "Quantile der Stichprobe",
         cex.lab = 2.2,
         cex.axis = 2.2,
         cex.main = 2.2)
  abline(mean(data$BP),
         sd(data$BP))
  
  
  qqnorm(data$RK,
         main = "Rotkehlchen",
         xlab = expression("theoretische Quantile von" ~ N(0,1)),
         ylab = "Quantile der Stichprobe",
         cex.lab = 2.2,
         cex.axis = 2.2,
         cex.main = 2.2)
  abline(mean(data$RK),
         sd(data$ZK))
  
  
  qqnorm(data$ZK,
         main = "Zaunk�nig",
         xlab = expression("theoretische Quantile von" ~ N(0,1)),
         ylab = "Quantile der Stichprobe",
         cex.lab = 2.2,
         cex.axis = 2.2,
         cex.main = 2.2)
  abline(mean(data$ZK),
         sd(data$ZK))
}
dev.off()


#################################################################
### Testen auf NV-Annahme
shapiro.test(data$WP)
shapiro.test(data$BP)
shapiro.test(data$RK)
shapiro.test(data$ZK)
# alle p-Werte > 0.13 (shapiro ist besser f�r kleine n)

ks.test(data$WP,"pnorm",mean(data$WP),sd(data$WP))
ks.test(data$BP,"pnorm",mean(data$BP),sd(data$BP))
ks.test(data$RK,"pnorm",mean(data$RK),sd(data$RK))
ks.test(data$ZK,"pnorm",mean(data$ZK),sd(data$ZK))
# alle p-Werte > 0.28
#############

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


