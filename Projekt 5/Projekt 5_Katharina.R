library(readxl)
#Daten einlesen
electiondata = read.csv("US_election_2024.csv", sep = ";", header = T)
#Datentypen anpassen
electiondata$Leading_Candidate = as.factor(electiondata$Leading_Candidate)
electiondata$Population_Density =
  as.numeric(gsub(",",".",electiondata$Population_Density))
electiondata$Median_Age =
  as.numeric(gsub(",",".",electiondata$Median_Age))
electiondata$HDI =
  as.numeric(gsub(",",".",electiondata$HDI))
electiondata$Unemployment_Rate =
  as.numeric(gsub(",",".",electiondata$Unemployment_Rate))
electiondata$Health_Insurance_Coverage =
  as.numeric(gsub(",",".",electiondata$Health_Insurance_Coverage))

#Deskription
#Boxplots einfach
boxplot(electiondata$Median_Age)
boxplot(electiondata$Birth_Rate)
boxplot(electiondata$HDI)
boxplot(electiondata$Unemployment_Rate)
boxplot(electiondata$Health_Insurance_Coverage)
boxplot(electiondata$Median_Rent)
#Boxplot Kandidat
boxplot(Population_Density~Leading_Candidate, data=electiondata,
        outline=F)
#Weiteres
plot(Population ~ Total_Area, data = electiondata, pch=19,
     xlab = "Gesamtfläche Bundesstaat in Square Miles",
     ylab = "Geschätzte Bevölkerungszahl",
     xlim = c(0, 700000),
     ylim = c(0, 40000000),
     col = ifelse(as.numeric(electiondata$Leading_Candidate) == 2, "red", "blue"))
legend("topright", legend = c("Harris", "Trump"),
       fill = c("blue", "red"))

#Modell
reg = glm(Leading_Candidate ~ ( Total_Area 
                               + Population 
                               +Population_Density
                               + Median_Age 
                               + Birth_Rate 
                               + HDI 
                               + Unemployment_Rate
                               + Health_Insurance_Coverage
                               + Median_Rent),
          data = electiondata,
          family = binomial())

summary(reg)
#Aufgabe 2
#Drop
drop1(glm(Leading_Candidate ~ ( Total_Area 
                                + Population 
                                +Population_Density
                                + Median_Age 
                                + Birth_Rate 
                                + HDI 
                                + Unemployment_Rate
                                + Health_Insurance_Coverage
                                + Median_Rent),
          data = electiondata,
          family = binomial()))
#Add
add1(glm(Leading_Candidate~Total_Area, data =electiondata, family = binomial()), gesamt_Modell)
add1(glm(Leading_Candidate~Total_Area+HDI, data =electiondata,
         family = binomial()), gesamt_Modell)
#
add1(glm(Leading_Candidate~Total_Area+HDI+Unemployment_Rate,
         data =electiondata,
         family = binomial()), gesamt_Modell)
#
add1(glm(Leading_Candidate~Total_Area+HDI+Unemployment_Rate+Population,
         data =electiondata,
         family = binomial()), gesamt_Modell)
#
add1(glm(Leading_Candidate~Total_Area
         +HDI+Unemployment_Rate+Population+Birth_Rate,
         data =electiondata,
         family = binomial()), gesamt_Modell)
#Reduziertes Modell

################### forward regression
electiondata$Leading_Candidate = as.numeric(electiondata$Leading_Candidate) - 1

min.model = lm(Leading_Candidate  ~ ( Total_Area + Population 
                                      + Population_Density
                                      + Median_Age 
                                      + Birth_Rate 
                                      + HDI 
                                      + Unemployment_Rate
                                      + Health_Insurance_Coverage
                                      + Median_Rent),
               data = electiondata
)

max.model = lm(Leading_Candidate  ~ ( Total_Area + Population 
                                      +Population_Density
                                      + Median_Age 
                                      + Birth_Rate 
                                      + HDI 
                                      + Unemployment_Rate
                                      + Health_Insurance_Coverage
                                      + Median_Rent)^2,
               data = electiondata
)

auto.forward = step(min.model, direction = "forward", 
                    scope = list(lower = min.model, upper = max.model))

auto.backward = step(max.model, direction = "backward",
                     scope = list(lower = min.model, upper = max.model))

auto.both = step(min.model, direction  = "both",
                 scope = list(lower = min.model, upper = max.model))

signif( coef(auto.forward),3)

signif( coef(auto.backward),3)
