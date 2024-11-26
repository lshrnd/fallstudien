library(tidyr)

data<- read.table("Kuckuckseier.txt", header = T)
#anderes Datenformat
daten<-pivot_longer(data, cols = everything(),
                    names_to = "Gruppe",values_to = "Werte")

mar_def = c(5, 4, 4, 2) + 0.1 # Default-Margin
WP<-data$WP
BP<-na.omit(data$BP)
RK<-na.omit(data$RK)
ZK<-na.omit(data$ZK)

###Deskription
gruppe_n<-c(length(WP),length(BP),length(RK),length(ZK))
label<-paste(name =c("WP","BP", "RK", "ZK"),"(n =",gruppe_n,")")
par(mar=c(5,7.5,0.5,0.5))
boxplot(WP, BP,RK,ZK, horizontal = T,
        names =label,
        xlab ="Länge der Kuckuckseier in Milimeter",
        las=1,
        cex.axis=1.25, cex.lab=1.75)

#Kennzahlen
sd(WP)
sd(BP)
sd(RK)
sd(ZK)

##QQ-Plots #WP
par(mar=c(4.5,4.5,0.5,0.5))
qqnorm(WP, main ="",
       cex.axis=1.25, cex.lab=1.75,
       xlab ="Theoretische Quantile", ylab ="empirische Quantile")
qqline(WP)
#QQ-Plots 
par(mar=c(4.5,4.5,0.5,0.5))
qqnorm(BP, main ="",
       cex.axis=1.25, cex.lab=1.75,
       xlab ="Theoretische Quantile", ylab ="empirische Quantile")
qqline(BP)
#QQ-Plots
par(mar=c(4.5,4.5,0.5,0.5))
qqnorm(RK, main ="",
       cex.axis=1.25, cex.lab=1.75,
       xlab ="Theoretische Quantile", ylab ="empirische Quantile")
qqline(RK)
#QQ-Plots
par(mar=c(4.5,4.5,0.5,0.5))
qqnorm(ZK, main ="",
       cex.axis=1.25, cex.lab=1.75,
       xlab ="Theoretische Quantile", ylab ="empirische Quantile")
qqline(ZK)

######Abschlusstestverfahren
##Einzelhypothesen
t.test(WP,BP) #H1H2 #ablehen
t.test(WP,RK) #H1H3 #behalten
t.test(WP,ZK) #H1H4 #ablehen
t.test(BP,RK) #H2H3 #behalten
t.test(BP,ZK) #H2H4 #ablehnen
t.test(RK,ZK) #H3H4 #ablehnen

###Durchschnittshypothesen Varianzanalyse
#testen auf Gleichheit Varianzen
bartlett.test(Werte~Gruppe, data = daten) 

#H1H2H3
anova(lm(Werte ~ Gruppe, data = subset(daten, Gruppe != "ZK")))
#H1H2H4
anova(lm(Werte ~ Gruppe, data = subset(daten, Gruppe != "RK")))
#H1H3H4
anova(lm(Werte ~ Gruppe, data = subset(daten, Gruppe != "BP")))
#H2H3H4
anova(lm(Werte ~ Gruppe, data = subset(daten, Gruppe != "WP")))
#H1H2H3H4
anova(lm(Werte~Gruppe, data=daten))

###Durchschnittshypothese Bonferroni
alpha<-0.05 
k<-2 #Anzahl Elementarhypothesen
# Funktion zur Berechnung und Überprüfung der p-Werte
Bonfe <- function(alpha, k, ...) {
  # Alle Paare von Vektoren als Liste übernehmen
  hyp_paare <- list(...)
  
  # Ergebnisliste
  results <- lapply(hyp_paare, function(pair) {
    if (length(pair) != 2) stop("Jedes Paar muss genau zwei Vektoren enthalten.")
    p_Wert <- t.test(pair[[1]], pair[[2]])$p.value
    Signifikanzniveau <- p_Wert > (alpha / k)
    return(list(p_Wert = p_Wert, Signifikanzniveau = Signifikanzniveau))
  })
  
  # Ausgabe als DataFrame zusammenfassen
  Ergebnis <- do.call(rbind, lapply(seq_along(results), function(i) {
    data.frame(
      Test = paste0("Test_", i),
      p_Wert = results[[i]]$p_Wert,
      Signifikanzniveau = results[[i]]$Signifikanzniveau
    )
  }))
  
  return(Ergebnis)
}

#H12H34
Bonfe(alpha, k, list(WP, BP), list(RK, ZK)) #ablehnen
#H13H24
Bonfe(alpha, k, list(WP, RK), list(BP, ZK)) #ablehnen
#H14H23
Bonfe(alpha, k, list(WP, ZK), list(BP, RK)) #ablehnen

##Ergebniss:alle Durchschnittshypothesen lehnen ab, 
##          alles bis auf H1H3 und H2H3 lehnt ab