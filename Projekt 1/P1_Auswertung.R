library(scales)
library(dplyr)

data = read.csv("census_2022_2002.csv")
data = data[ - c(429,430),] # USA entfernen da NA

data22 = data[data$Year == 2022,]
data02 = data[data$Year == 2002,]
# Daten zu 2022 (fuer 1 bis 3)

data02_clean = na.omit(data02) 
data22_clean = semi_join(data22, data02_clean, by= "Country")
# Daten wenn beide Jahre existieren (fuer 4)
data_merge_clean<-merge(data02_clean, data22_clean, by= "Country",
                        suffixes = c("_2002","_2022"))
data_merge_clean<-data_merge_clean[, -c(2,7:9)]

#### allgemeine Variablen:
mar_def = c(5, 4, 4, 2) + 0.1 # Default-Margin
region_cols = c("Asia" = "red", "Europe" = "green", "Africa" = "blue", "Oceania" = NA, "Americas" = "yellow")
region_names = c("Asien", "Europa", "Afrika", "Ozeanien", "Amerika")


####
# Box-Plots der drei Lebenserwartung-Kennzahlen von 2022
boxplot(data22$Life_Expectancy_Overall, data22$Life_Expectancy_Male, data22$Life_Expectancy_Female, 
        names = c("Gesamt","Männlich","Weiblich"), las = 1, cex.axis = 0.7, main = "Boxplot der Lebenserwartungen aller Länder",
        xlab = "Alter in Jahre", horizontal = T)


# Bar-Plot der Haeufigkeiten der Regionen/Subregionen
par(mar=c(4, 5, 2.5, 2))
RegionTable = sort(table(data22$Region), decreasing = F)
barplot(RegionTable, col = alpha(region_cols[names(RegionTable)],0.25), xlim = c(0,60), horiz = T, las = 2,
        main = "Anzahl der Länder innerhalb einer Region")
par(mar=mar_def)

par(mar=c(4, 9, 2.5, 2))
SubregionTable = sort(table(data22$Subregion), decreasing = F)
barplot(SubregionTable, col = alpha(region_cols[data22$Region[match(names(SubregionTable), data22$Subregion)]],0.25),
        las = 2, cex.names = 0.9, horiz = T, main = "Anzahl der Länder innerhalb einer Subregion")
legend(19, 15, legend = region_names, pt.bg = alpha(region_cols, 0.25), pch = 22, col = "black", 
       pt.cex = 1.3, text.width = 2, x.intersp = 0.6, y.intersp = 0.8)
par(mar=mar_def)

# Box-Plot der Geburtenrate in 2022
boxplot(data22$Total_Fertility_Rate, horizontal = T, main = "Geburtenrate in 2022")


#### 4 - Jahresvergleich
Life_Expectancy_Change = (data22_clean$Life_Expectancy_Overall - data02_clean$Life_Expectancy_Overall) / data02_clean$Life_Expectancy_Overall
# Histogramm der Veraenderung der Lebenserwartung
histcols = alpha(c("red","red",rep("green",22)),0.15)
hist(Life_Expectancy_Change + 1, xlim = c(0.9,1.5), breaks = 20, col = histcols,
     xlab = "Anteil der vorherigen Lebenserwartung, 1.1 = +10%", ylab = "absolute Häufigkeit", main = "Veränderung der Lebenserwartung", )
  # TODO: xlab / main muss verbessert werden, so nicht aussagekräftig genug

# Scatterplot der Lebenserwartung in 2002 gegen die Veraenderung der Lebensrate
plot(data02_clean$Life_Expectancy_Overall,Life_Expectancy_Change, bg = alpha(region_cols[data02_clean$Region],0.7), pch = 21, col = "darkgrey",
     xlab = "Lebenserwartung in 2002 in Jahren", ylab = "Veränderung in Prozent", 
     main = "prozentuale Veränderung der Lebenserwartung von 2002 zu 2022")
legend(82, 0.43, legend = region_names, pt.bg = region_cols, pch = 21, col = "black", 
       pt.cex = 1.3, text.width = 4, x.intersp = 0.7, y.intersp = 0.8)

# Scatterplot der Lebenserwartung in 2002 gegen die Lebenserwartung in 2022             ## auskommentiert: Variante mit seperaten Symbolen
  # region_symb = c("Asia" = 16, "Europe" = 17, "Africa" = 18, "Oceania" = 20, "Americas" = 19) 
plot(data02_clean$Life_Expectancy_Overall,data22_clean$Life_Expectancy_Overall,
      #pch = region_symb[data02_clean$Region], col = alpha(region_cols[data02_clean$Region],0.7), 
     bg = alpha(region_cols[data02_clean$Region],0.7), pch = 21, col ="darkgrey",
     xlim = c(45,90), ylim = c(45,90), xlab = "Lebenserwartung in 2002 in Jahren", ylab = "Lebenserwartung in 2022 in Jahren", 
     main = "Lebenserwartung von 2002 und 2022 im Vergleich")
points(45:90, 45:90, type = "l", col = grey(0,0.3) )
legend("topleft", legend = c(region_names,"Diagonale"),
       #pch = c(16,17,18,20,19,NA),lty = c(rep(NA,5),1), col = c(region_cols,"darkgrey"),
       pch = c(rep(21,5),NA), lty = c(rep(NA,5),1), pt.bg = region_cols, col = c(rep("black",5),"darkgrey"),
       pt.cex = 1.1, text.width = 4, x.intersp = 0.7, y.intersp = 0.8)

#Dotchart für die Lebenserwatung im Vergleich 2002 gegen 2022
#Farblich Regionale Aufteilung auskommentiert
data_merge_clean<-data_merge_clean[order(
  data_merge_clean$Life_Expectancy_Overall_2002),]
dotchart(data_merge_clean$Life_Expectancy_Overall_2002, 
         xlab = "Lebenserwartung in Jahren", ylab = "Länder",
         main = "Veränderung der Lebenserwartung von 2002 zu 2022",
         pch = 1, cex = 0.7,
         xlim = range(c(data_merge_clean$Life_Expectancy_Overall_2022,
                        data_merge_clean$Life_Expectancy_Overall_2002)),
         lcolor = "White",
         #col = alpha(region_cols[data_merge_clean$Region_2022],0.7)
         )
points(data_merge_clean$Life_Expectancy_Overall_2022,
       1:nrow(data_merge_clean),  pch=16,
       #col = alpha(region_cols[data_merge_clean$Region_2022],0.7)
)
legend("topleft", legend = c("2002", "2022"), pch = c(1,16))
#legend("left", legend = region_names, fill = alpha(region_cols, 0.7),title = "Regionen")

#Dotchart Fertility im Vergleich 2002 gegen 2022
# Farbliche Aufteilung nach Region auskommentiert
data_merge_clean<-data_merge_clean[order(
  data_merge_clean$Total_Fertility_Rate_2002),]
dotchart(data_merge_clean$Total_Fertility_Rate_2002,
         main = "Veränderung der Fertilitätsrate von 2002 zu 2022",
         xlab = "Fertilitätsrate", pch=1,
         xlim = range(c(data_merge_clean$Total_Fertility_Rate_2022,
                        data_merge_clean$Total_Fertility_Rate_2002)),
         ylab = "Länder", 
         lcolor = "White",
        # col = alpha(region_cols[data_merge_clean$Region_2022],0.7)
         )
points(data_merge_clean$Total_Fertility_Rate_2022,
       1:nrow(data_merge_clean),  pch=16,
       #col = alpha(region_cols[data_merge_clean$Region_2022],0.7)
       )
legend("right", legend = c("2002", "2022"), pch = c(1,16), title = "Jahr")
#legend("bottomright", legend = region_names, fill = alpha(region_cols, 0.7),title = "Regionen")
