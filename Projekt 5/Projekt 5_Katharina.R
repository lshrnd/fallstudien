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
# Paket glmnet laden
library(glmnet)

# Daten vorbereiten (nur numerische Variablen)
X <- as.matrix(electiondata[, c("Total_Area", "Population", "Population_Density", 
                        "Median_Age", "Birth_Rate", "HDI", 
                        "Unemployment_Rate", "Health_Insurance_Coverage", "Median_Rent")])
y <- electiondata$Leading_Candidate

# Lasso-Logistische Regression durchführen
lasso_model <- cv.glmnet(X, y, alpha = 1, family = "binomial")

# Beste Lambda-Parameter anzeigen
lasso_model$lambda.min

# Coefficients des Lasso-Modells anzeigen
coef(lasso_model, s = "lambda.min")
