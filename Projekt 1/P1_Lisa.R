# setwd("C:/Users/lisah/Documents/GitHub/fallstudien/Projekt 1")
data = read.csv("census_2022_2002.csv")
data22 = data[data$Year == 2022,]
apply(data22, 2, unique)

boxplot(data22[,3],data22[,4],data22[,5], horizontal = T, 
        names = c("LE Overall","LE Male","LE Female"), las = 1, cex.axis = 0.7,
        xlab = "Jahre")

tapply(data22$Life_Expectancy_Overall,data22$Region,mean, na.rm = T)

barplot( sort(
  tapply(data22$Life_Expectancy_Overall,data22$Subregion,median, na.rm = T)
), las = 2, cex.names = 0.4
)

### 