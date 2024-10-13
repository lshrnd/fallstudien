library(dplyr)
##

hist(census_2022_2002$Life_Expectancy_Overall)

hist(census_2022_2002$Total_Fertility_Rate)

plot(census_2022_2002$Life_Expectancy_Male, census_2022_2002$Life_Expectancy_Female)
plot(census_2022_2002$Life_Expectancy_Overall, census_2022_2002$Life_Expectancy_Male)
plot(census_2022_2002$Life_Expectancy_Overall, census_2022_2002$Life_Expectancy_Female)

#Aufteilen in Daten nach Jahr
census2002<-subset(census_2022_2002, census_2022_2002$Year == 2002)
census2022<-subset(census_2022_2002, census_2022_2002$Year == 2022)

plot(census2002$Life_Expectancy_Overall, census2022$Life_Expectancy_Overall)

#wieviele laender
length(unique(census_2022_2002$Country))
#welche Zeilen NA
census_2022_2002[apply(census_2022_2002, 1, function(x) any(is.na(x))), ]

#Zensus ohne NA 
zen02_clean<-na.omit(census2002) 
zen22_clean<- semi_join(census2022, zen02_clean, by= "Country")

#1.## Haeufigkeitverteilung mit geschlecht unterschied

#Lebenserwartung allgmein
hist(zen22_clean$Life_Expectancy_Overall)
table(zen22_clean$Life_Expectancy_Overall)
boxplot(zen22_clean$Life_Expectancy_Overall)

summary(zen22_clean$Life_Expectancy_Overall)
#Lebenserwartung Geschlecht
boxplot(zen22_clean$Life_Expectancy_Male, zen22_clean$Life_Expectancy_Female,
        names= c("Männer", "Frauen"),
        las=1, ylab=expression(paste("Lebenserwartung in Jahren")))

#Fertalitaet

#2. ## Bivariate zusammenhaenge

#Streudiagramm Zusammenhaenge bivariat
werte<-data.frame(Lebenserwatung_Allgemein=zen22_clean$Life_Expectancy_Overall,
                  Lebenserwartung_Männer=zen22_clean$Life_Expectancy_Male,
       Lebenserwartung_Frauen=zen22_clean$Life_Expectancy_Female,
        Fertilitätsrate=zen22_clean$Total_Fertility_Rate,
       Region=zen22_clean$Region)
farbe<-c("blue", "yellow", "black", "green", "red")
pairs(werte[, 1:4], pch=21, las=1,
      bg=farbe[as.factor(werte$Region)])

#Korrelation Lebensewartung person default
cor(zen22_clean$Life_Expectancy_Overall,
    zen22_clean$Life_Expectancy_Male)
#0.9921562
cor(zen22_clean$Life_Expectancy_Overall,
    zen22_clean$Life_Expectancy_Female)
#0.9921973
cor(zen22_clean$Life_Expectancy_Female,
    zen22_clean$Life_Expectancy_Male)
#0.9688588


#4. ##Jahresvergleich
data_2022_sorted <- zen22_clean[order(zen22_clean$Life_Expectancy_Overall), ]
data_2002_sorted<- zen02_clean[order(zen02_clean$Life_Expectancy_Overall),]

dotchart(data_2022_sorted$Life_Expectancy_Overall)

#Zusammenfügen daten
data<-merge(data_2022_sorted, data_2002_sorted, by="Country",
            suffixes= c("_2022","_2002"))
data<-data[order(data$Life_Expectancy_Overall_2002),]
#Erstelle ein Dotchart für die Lebenserwatung
dotchart(data$Life_Expectancy_Overall_2002, 
         xlab = "Lebenserwartung in Jahren",
         col="blue", pch = 16, cex = 0.7,
         xlim = range(c(data$Life_Expectancy_Overall_2022,
                        data$Life_Expectancy_Overall_2002)))
points(data$Life_Expectancy_Overall_2022, 1:nrow(data), col="red", pch=17)
legend("topleft", legend = c("2002", "2022"),
       col=c("blue", "red"), pch = c(16,17))

#Dotchart Fertility Basis 2002
data<-data[order(data$Total_Fertility_Rate_2002),]
dotchart(data$Total_Fertility_Rate_2002,
         xlab = "Fertilitätsrate",
         col="blue", pch=16,
         xlim = range(c(data$Total_Fertility_Rate_2022,
                        data$Total_Fertility_Rate_2002)))
points(data$Total_Fertility_Rate_2022, 1:nrow(data), col="red", pch=17)
legend("bottomright", legend = c("2002", "2022"),
       col=c("blue", "red"), pch = c(16,17))

#Dotchart Fertility Basis 2022
data<-data[order(data$Total_Fertility_Rate_2022),]
dotchart(data$Total_Fertility_Rate_2022,
         xlab = "Fertilitätsrate",
         col="red", pch=17,
         xlim = range(c(data$Total_Fertility_Rate_2022,
                        data$Total_Fertility_Rate_2002)))
points(data$Total_Fertility_Rate_2002, 1:nrow(data), col="blue", pch=16)
legend("bottomright", legend = c("2002", "2022"),
       col=c("blue", "red"), pch = c(16,17))