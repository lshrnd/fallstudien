# SETUP
{
  library(readxl)
  library(gtools)
  library(pROC)
  
  setwd("C:/Users/lisah/Documents/GitHub/fallstudien/Projekt 5")
  electiondata = read.csv("US_election_2024.csv", sep = ";", header = T, dec = ",")
  
  Lead = electiondata$Leading_Candidate
  electiondata$Leading_Candidate = as.factor(electiondata$Leading_Candidate)
  electiondata$Leading_Candidate = as.numeric(electiondata$Leading_Candidate) - 1
  # 1 = Trump, 0 = Harris
  
  # [3] "Total_Area"               
  # [4] "Population"               
  # [5] "Population_Density"       
  # [6] "Median_Age"               
  # [7] "Birth_Rate"               
  # [8] "HDI"                      
  # [9] "Unemployment_Rate"        
  # [10] "Health_Insurance_Coverage"
  # [11] "Median_Rent"   
  
  str(electiondata)
  
  norm = function(x){
    m = mean(x)
    sd = sd(x)
    return ((x-m)/sd)
  }
}

### Volles Modell (alle Einflussvariablen)
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

### Datensatz normalisieren:
normedData = cbind(electiondata[,1:2],sapply(electiondata[,3:11],norm))

fit_full = glm(Leading_Candidate ~ Total_Area 
          + Population 
          + Population_Density
          + Median_Age 
          + Birth_Rate 
          + HDI 
          + Unemployment_Rate
          + Health_Insurance_Coverage
          + Median_Rent,
          data = normedData,
          family = binomial)

summary(fit_full)


##### Variablenselektion
### forward selection
dat = normedData[,2:11]
minm = glm(Leading_Candidate ~ 1, data = dat, family = binomial)
maxm = glm(Leading_Candidate ~ ., data = dat, family = binomial)
fit_forwardsearch = step(minm, direction = "forward", scope = list(lower = minm, upper = maxm))
signif(coef(fit_forwardsearch))
# Variablen mit Index 4,7,8,9,10,11

### exhaustive search
numbers = 3:11
# create all possible combos of variables per their col index
all_combinations <- list()
for (k in 1:length(numbers)) {
  comb_k <- unlist(unname(combinations(n = length(numbers), r = k, v = numbers)))
  all_combinations <- c(all_combinations, split(comb_k, row(comb_k)))
}

# returns the aic of the glm model that predicts 
# leading_candidate with all variables at the varIndices
fitglm = function(varIndices){
  index = c(2,varIndices)
  selected_data = normedData[,index]
  model = glm(Leading_Candidate ~., data = selected_data, family = binomial)
  return (model$aic)
}

# get aic for every combo and sort by aic
df = data.frame(I(all_combinations))
df$aic = sapply(df$all_combinations, fitglm)
df = df[order(unlist(df$aic)), ]

# get the names of the variables that result in the lowest aic
varIndexes = df$all_combinations[1]
names(normedData[,unlist(varIndexes)])
df$aic[1] # 30.16

fit_bestsubset = glm(Leading_Candidate ~., data = normedData[,c(2,unlist(varIndexes))], family = binomial)
confint(fit_bestsubset)


# find aic of the variables that are selected by forward search
row_index = which(sapply(df$all_combinations, function(x) all(x == c(4,7,8,9,10,11))))
df$all_combinations[row_index]
df$aic[row_index] # 32.23


### ROC-Kurve
par(mfrow = c(1,2))

preds_bestsubset = predict.glm(fit_bestsubset, type = "response")
roc_bestsubset = roc(response = normedData$Leading_Candidate, predictor = preds_bestsubset)
plot(roc_bestsubset, main = "ROC Best Subset", col = "blue", lwd = 2)
roc_bestsubset$auc

library(caret)

cv = train(as.factor(Leading_Candidate) ~., data = normedData[,-c(1)], method = "glm", family = binomial, 
      trControl = trainControl(method = "cv", number = 10))


################# Deskription ##############
### Boxplot für jede numerische Variable, unterteilt nach Leading Cand
for(i in 3:11){
  boxplot(unlist(electiondata[i]) ~ Lead, horizontal = T,
          xlab = names(electiondata[i]))
}

### Kennzahlen für alle numerischen Variablen
lapply(electiondata[,3:11],summary, quantile.type = 2)

### Scatterplot-Matrix, unterteilt nach Lead
plot(electiondata[,3:11], col = electiondata$Leading_Candidate)
