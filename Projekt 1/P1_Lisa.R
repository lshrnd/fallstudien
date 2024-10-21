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
boxplot(data22$Life_Expectancy_Overall ~ f, horizontal = T, las = 2, ylim = c(45,90),
        col = alpha(c(rep("blue",5),rep("red",4),rep("yellow",4),rep("green",4),NA,NA,NA,NA),0.25))

f = factor(data22$Subregion, levels = l1)

l0 = c("Western Africa", "Eastern Africa", "Middle Africa", "Northern Africa", "Southern Africa",
       "Western Asia", "South-Central Asia", "South-Eastern Asia", "Eastern Asia",
       "Caribbean", "South America", "Central America", "Northern America",
       "Southern Europe", "Northern Europe", "Eastern Europe", "Western Europe",
       "Polynesia", "Micronesia", "Melanesia", "Australia/New Zealand") 

l1 = c("Southern Africa", "Middle Africa", "Western Africa", "Eastern Africa", "Northern Africa",
  "South-Central Asia", "South-Eastern Asia", "Western Asia", "Eastern Asia",
  "Central America",  "South America", "Caribbean", "Northern America",
  "Eastern Europe", "Southern Europe", "Northern Europe", "Western Europe",
  "Micronesia", "Melanesia", "Polynesia", "Australia/New Zealand")


boxplot(data02$Life_Expectancy_Overall ~ f, horizontal = T, las = 2, ylim = c(45,90), )
boxplot(data22$Life_Expectancy_Overall ~ f, horizontal = T, las = 2, ylim = c(45,90), add = T,
        col = alpha(c(rep("blue",5),rep("red",4),rep("yellow",4),rep("green",4),NA,NA,NA,NA),1))

