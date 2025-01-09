# SETUP
{
  library(readxl)
  
  setwd("C:/Users/lisah/Documents/GitHub/fallstudien/Projekt 5")
  electiondata = read.csv("US_election_2024.csv", sep = ";", header = T)
  
  electiondata$Leading_Candidate = as.factor(electiondata$Leading_Candidate)
}

reg = glm(Leading_Candidate ~ (State 
                         + Total_Area 
                         + Population 
                         + Median_Age 
                         + Birth_Rate 
                         + HDI 
                         + Unemployment_Rate
                         + Health_Insurance_Coverage
                         + Median_Rent),
    data = electiondata,
    family = binomial())

summary(reg)



