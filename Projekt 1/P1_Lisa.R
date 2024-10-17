# setwd("C:/Users/lisah/Documents/GitHub/fallstudien/Projekt 1")
data = read.csv("census_2022_2002.csv")
data22 = data[data$Year == 2022,]
data02 = data[data$Year == 2002,]
apply(data22, 2, unique)
library(scales)

# TODO big: Datensaetze cleanen, also NA entfernen, vorallem f?r 4
  # auch wichtig um korrekte Stichprobengroesse angeben zu koennen etc
# 1 - Haeufigkeitsverteilung
# Box-Plots der drei Lebenserwartung-Kennzahlen von 2022
boxplot(data22$Life_Expectancy_Overall, data22$Life_Expectancy_Male, data22$Life_Expectancy_Female, 
        names = c("LE Overall","LE Male","LE Female"), las = 1, cex.axis = 0.7,
        xlab = "Jahre", horizontal = T)

# Histogramm fuer die Lebenserwartungen 2022 einzeln
hist(data22$Life_Expectancy_Overall, breaks = seq(50,100,2), freq = F, xlim = c(50,100), ylim = c(0,0.09))
hist(data22$Life_Expectancy_Male, breaks = seq(50,100,2), freq = F, xlim = c(50,100), ylim = c(0,0.09))
hist(data22$Life_Expectancy_Female, breaks = seq(50,100,2), freq = F, xlim = c(50,100), ylim = c(0,0.09))

# maybe male female in einem hist

# Barplot der Regionen und Subregionen, absteigend sortiert
# dient dem Ueberblick ueber die Region-Einteilungen

# Idee: Farbschema erstellen, dass den Regionen Farben zuordnet und den Subregionen die Farbe ihrer Region
region_cols = alpha(c("Asia" = "red", "Europe" = "green", "Africa" = "blue", "Oceania" = NA, "Americas" = "yellow"),0.2)

RegionTable = sort(table(data22$Region), decreasing = F)
barplot(RegionTable, col = region_cols[names(RegionTable)], xlim = c(0,60), horiz = T, las = 2)

SubregionTable = sort(table(data22$Subregion), decreasing = F)
barplot(SubregionTable, col = region_cols[data22$Region[match(names(SubregionTable), data22$Subregion)]],
        las = 2, cex.names = 0.8, horiz = T)
  # col-Explanation: nimmt die Namen der Tables und ordnet sie den entsprechenden Regionen zu.
    # bei den Subregions wird anhand des DataFrames erst zugeordnet, welche Subregion welcher Region entspricht
# TODO: nur innerhalb den Regionen sortieren, nicht komplett

mosaicplot(~ Region + Subregion, data = data22, color = TRUE, las = 2)

# Geburtenrate
boxplot(data22$Total_Fertility_Rate, horizontal = T, main = "Geburtenrate in 2022")
hist(data22$Total_Fertility_Rate, breaks = 21, freq = F)

# 4 - Jahresvergleich

# prozentuale Veraenderung der Lebenserwartung in Abh. von der Geburtenrate in 2002
Life_Expectancy_Change = (data22$Life_Expectancy_Overall - data02$Life_Expectancy_Overall) / data02$Life_Expectancy_Overall
plot(data02$Total_Fertility_Rate, Life_Expectancy_Change)
plot(data02$Total_Fertility_Rate, data22$Life_Expectancy_Overall - data02$Life_Expectancy_Overall)

histcols = alpha(c("red","red",rep("green",22)),0.1)
hist(Life_Expectancy_Change, xlim = c(-0.1,0.5), breaks = 20, col = histcols)

# Lebenserwartung vorher vs nachher, unterteilt in Regionen
region_symb = c("Asia" = 16, "Europe" = 17, "Africa" = 18, "Oceania" = 20, "Americas" = 19)
plot(data02$Life_Expectancy_Overall,data22$Life_Expectancy_Overall,
     pch = region_symb[data02$Region], col = region_cols[data02$Region],
     xlim = c(40,90), ylim = c(40,90))
points(40:90, 40:90, type = "l", col = grey(0,0.3) )http://127.0.0.1:8261/graphics/plot_zoom_png?width=1184&height=861
legend("topleft", legend = c("Asia","Europe","Africa","Oceania","Americas"),
       pch = c(16,17,18,20,19), col = region_cols, cex = 0.5)
# alles ueber linie ist gestiegen, alles drunter gesunken. 
# man sieht wenig gesunken (auch siehe hist oben)
  # gar kein PLan ob das irgnen interpretativen Mehrwert hat

plot(data22$Total_Fertility_Rate,data22$Life_Expectancy_Overall)
plot(data02$Life_Expectancy_Overall,Life_Expectancy_Change, col = region_cols[data02$Region])

######################
tapply(data22$Life_Expectancy_Overall,data22$Region,mean, na.rm = T)

barplot( sort(
  tapply(data22$Life_Expectancy_Overall,data22$Subregion,median, na.rm = T)
), las = 2, cex.names = 0.4
)

min(data22$Life_Expectancy_Male, na.rm = T)

barplot(sort(table(data22$Region), decreasing = T))
barplot(sort(table(data22$Subregion), decreasing = T))
sum(table(data22$Subregion))
sum(table(data22$Region))

min(data02$Life_Expectancy_Overall, na.rm = T)

#
par(mar = c(4,13,4,4))
sorted = factor(paste(data22$Region,data22$Subregion, sep=": "))
boxplot(data22$Life_Expectancy_Overall ~ sorted, horizontal = T, las = 2, ylab = NA, cex = NA,
        col = alpha(rep(region_cols, times = c(5,4,4,4,4)),0.25)
        )
#boxplot(data22$Life_Expectancy_Overall sorted#boxplot(data22$Life_Expectancy_Overall ~ data22$Region, add = T, horizontal = T, at = c(3,7.5,11.5,15.5,19.5),
 #       boxwex = 3, col = alpha("white",0.5), names = NA, outline = F, las = 2)




