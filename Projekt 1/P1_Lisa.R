# setwd("C:/Users/lisah/Documents/GitHub/fallstudien/Projekt 1")
data = read.csv("census_2022_2002.csv")
data22 = data[data$Year == 2022,]
apply(data22, 2, unique)
library(scales)

# Haeufigkeitsverteilung
# Box-Plots der drei Lebenserwartung-Kennzahlen von 2022
boxplot(data22$Life_Expectancy_Overall, data22$Life_Expectancy_Male, data22$Life_Expectancy_Female, 
        names = c("LE Overall","LE Male","LE Female"), las = 1, cex.axis = 0.7,
        xlab = "Jahre", horizontal = T)

# Histogramm fuer die Lebenserwartungen 2022 einzeln
hist(data22$Life_Expectancy_Overall, breaks = seq(50,100,2), freq = F, xlim = c(50,100), ylim = c(0,0.09))
hist(data22$Life_Expectancy_Male, breaks = seq(50,100,2), freq = F, xlim = c(50,100), ylim = c(0,0.09))
hist(data22$Life_Expectancy_Female, breaks = seq(50,100,2), freq = F, xlim = c(50,100), ylim = c(0,0.09))


# Barplot der Regionen und Subregionen, absteigend sortiert
# dient dem Ueberblick ueber die Region-Einteilungen

# Idee: Farbschema erstellen, dass den Regionen Farben zuordnet und den Subregionen die Farbe ihrer Region
region_cols = alpha(c("Asia" = "red", "Europe" = "green", "Africa" = "blue", "Oceania" = NA, "Americas" = "yellow"),0.2)

RegionTable = sort(table(data22$Region), decreasing = T)
barplot(RegionTable, col = region_cols[names(RegionTable)], ylim = c(0,60))

SubregionTable = sort(table(data22$Subregion), decreasing = T)
barplot(SubregionTable, col = region_cols[data22$Region[match(names(SubregionTable), data22$Subregion)]],
        las = 2, cex.names = 0.8)
  # col-Explanation: nimmt die Namen der Tables und ordnet sie den entsprechenden Regionen zu.
    # bei den Subregions wird anhand des DataFrames erst zugeordnet, welche Subregion welcher Region entspricht
# TODO: nur innerhalb den Regionen sortieren, nicht komplett

mosaicplot(~ Region + Subregion, data = data22, color = TRUE, las = 2)

# Geburtenrate
boxplot(data22$Total_Fertility_Rate, horizontal = T, main = "Geburtenrate in 2022")
hist(data22$Total_Fertility_Rate, breaks = 21, freq = F)

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
