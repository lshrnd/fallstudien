# SETUP
{
  library(readxl)
  
  setwd("C:/Users/lisah/Documents/GitHub/fallstudien/Projekt 5")
  electiondata = read.csv("US_election_2024.csv", sep = ";", header = T, dec = ",")
  
  Lead = electiondata$Leading_Candidate
  electiondata$Leading_Candidate = as.factor(electiondata$Leading_Candidate)
  
  str(electiondata)
}

## Volles Modell (alle Einflussvariablen)
reg = glm(Leading_Candidate ~ Total_Area 
                         + Population 
                         + Population_Density
                         + Median_Age 
                         + Birth_Rate 
                         + HDI 
                         + Unemployment_Rate
                         + Health_Insurance_Coverage
                         + Median_Rent,
    data = electiondata,
    family = binomial)

summary(reg)


#################
## Boxplot für jede numerische Variable, unterteilt nach Leading Cand
for(i in 3:11){
  boxplot(unlist(electiondata[i]) ~ Lead, horizontal = T,
          xlab = names(electiondata[i]))
}

## Kennzahlen für alle numerischen Variablen
lapply(electiondata[,3:11],summary, quantile.type = 2)

plot(electiondata[,3:11], col = electiondata$Leading_Candidate)
