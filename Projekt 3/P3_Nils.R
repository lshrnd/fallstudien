setwd("C:\\Users\\Nils Mac\\Documents\\GitHub\\fallstudien\\Projekt 3")
data = read.table("Kuckuckseier.txt", header = T)

{ ### Variablen einzeln speichern
WP <- na.omit(data$WP)
BP <- na.omit(data$BP)
RK <- na.omit(data$RK)
ZK <- na.omit(data$ZK)
}


