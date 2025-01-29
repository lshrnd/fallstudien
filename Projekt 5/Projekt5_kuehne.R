##### Fallstudien 1 - Projekt 5
#####
##### Jorrit Kuehne


# SETUP
{
  library(gtools)
  
  setwd("C:/Users/jorri/Documents/GitHub/fallstudien/Projekt 5")
  electiondata = read.csv("US_election_2024.csv", sep = ";", header = T, dec = ",")
  
  electiondata$Leading_Candidate = as.factor(electiondata$Leading_Candidate)
  str(electiondata)
  gerNames = c("Staat",
               "Führender Kandidat",
               "Fläche",
               "Bevölkerungsanzahl",
               "Bevölkerungsdichte",
               "Medianes Alter",
               "Geburtenrate",
               "HDI",
               "Arbeitslosenrate",
               "Krankenversicherungsrate",
               "Mediane Rente")
  einheiten = c("", "", "[sq mi]", "", "[1/(sq mi)]", "[Jahre]", "", "", "", "", "[USD]")
}

#### Deskription

pdf(file = "boxplots.pdf", width = 10)
par(mfrow = c(3,3))
par(mar = c(2,2,1.5,0.5) + 0.1)
i = 3
for(col in electiondata[ , 3:11]) { 
  boxplot(col ~ electiondata$Leading_Candidate, col = c("blue", "red"),
          xlab = "", ylab = "",
          main = paste(gerNames[i], einheiten[i]),
          cex.axis = 1.5, cex.main = 2)
  i = i + 1
}
dev.off()

par(mfrow = c(1,1))
par(mar = c(5, 4, 4, 2) + 0.1)

sapply(subset(electiondata[, 3:11], electiondata$Leading_Candidate == "Trump"), 
       function(col) {summary(col, quantile.type = 2)})
sapply(subset(electiondata[, 3:11], electiondata$Leading_Candidate == "Trump"),
       function(col) {sd(col)})

sapply(subset(electiondata[, 3:11], electiondata$Leading_Candidate == "Harris"), 
       function(col) {summary(col, quantile.type = 2)})
sapply(subset(electiondata[, 3:11], electiondata$Leading_Candidate == "Harris"),
       function(col) {sd(col)})


summary(electiondata$Leading_Candidate)

max(electiondata$Total_Area)
z = which(electiondata$Total_Area == max(electiondata$Total_Area))
electiondata$State[z]

max(electiondata$Population)
z = which(electiondata$Population == max(electiondata$Population))
electiondata$State[z]

max(electiondata$Population_Density)
z = which(electiondata$Population_Density == max(electiondata$Population_Density))
electiondata$State[z]



#### Model mit allen Variablen

fullModel = glm(Leading_Candidate ~
                  Total_Area +
                  Population + 
                  Population_Density +
                  Median_Age +
                  Birth_Rate +
                  HDI +
                  Unemployment_Rate +
                  Health_Insurance_Coverage +
                  Median_Rent
                ,
                family = binomial,
                data = electiondata)
summary(fullModel)

####

#### Variablenselektion - BestSubset

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
  selected_data = electiondata[,index]
  model = glm(Leading_Candidate ~., data = selected_data, family = binomial)
  return (model$aic)
}

# get aic for every combo and sort by aic
df = data.frame(I(all_combinations))
df$aic = sapply(df$all_combinations, fitglm)
df = df[order(unlist(df$aic)), ]

# get the names of the variables that result in the lowest aic
varIndexes = df$all_combinations[1]
names(electiondata[,unlist(varIndexes)])
df$aic[1] # 30.16

fit_bestsubset = glm(Leading_Candidate ~., data = electiondata[,c(2,unlist(varIndexes))], family = binomial)
summary(fit_bestsubset)
confint.default(fit_bestsubset)

####

#### ROC-Kurven und AUC

# set seed (cause of randomness in cv)
set.seed(19042101)

data = electiondata

### function: generateIndexSubsets
#
# Short: generates K (equally sized) subsets of the numbers 1 to n; no duplicates
generateIndexSubsets = function(n, K) {
  
  permutedRowIndices = sample(n)
  
  lapply(1:K, function(k) { i = 0
  subset = rep(NA, n / K + 1)
  while(i * 5 + k <= n) {
    subset[i + 1] = permutedRowIndices[i * 5 + k]
    i = i + 1
  }
  return(subset[!is.na(subset)])
  }
  )
}

# generate 5 index subsets
split = generateIndexSubsets(length(data[, 1]), 5)
split

electiondata[-split[[1]], ]

### function trainAndTestOnce
#
# Short: trains the glm model based on the given data without the observations
#         in the rows marked in testIndices
#
# Input: testIndices - vector of indices which are not used for model fitting but testing
trainAndTestOnce = function(data, formula, testIndices, ...) {
  model = glm(formula, data = data[-testIndices, ], ...)
  testPrediction = predict.glm(model, data[testIndices, ], type = "response")
  
  return(list("model" = model, "testPrediction" = testPrediction))
}

# all parameters formula
formelAllParams = Leading_Candidate ~
  Total_Area +
  Population + 
  Population_Density +
  Median_Age +
  Birth_Rate +
  HDI +
  Unemployment_Rate +
  Health_Insurance_Coverage +
  Median_Rent

# test ob es geht
trainAndTestOnce(electiondata, formelAllParams, split[[1]], family = "binomial")

##### DO THE CROSS-VALIDATION

# for all testsets fit all parameter log-regression model and predict via trainAndTestOnce
resCVfullModel = lapply(split, trainAndTestOnce, 
                        data = electiondata, 
                        formula = formelAllParams, 
                        family = "binomial")

# best subset formula
formelBestSubset = Leading_Candidate ~
  Population +
  Median_Age +
  Unemployment_Rate +
  Health_Insurance_Coverage +
  Median_Rent

# for all testsets fit best subset log-regression model and predict via trainAndTestOnce
resCVbssModel = lapply(split, trainAndTestOnce,
                       data = electiondata,
                       formula = formelBestSubset,
                       family = "binomial")



# put testPredictions back together (for drawing ROC-Curves)

predsFullModel = unlist(lapply(resCVfullModel, function(res) { return(res$testPrediction) }))
predsFullModel = predsFullModel[order(as.numeric(names(predsFullModel)))]

predsBssModel = unlist(lapply(resCVbssModel, function(res) { return(res$testPrediction) }))
predsBssModel = predsBssModel[order(as.numeric(names(predsBssModel)))]


plotRocDf = data.frame(electiondata$Leading_Candidate, predsFullModel, predsBssModel)


### function myPlotRoc
#
# Short: plots a TPR-TNR ROC-Curve for a vector of true labels, which has 
#        been sorted (decreasing) by the corresponding score/prob for the 
#        given target
# Input: truth - binary vector, result of sorting the score/probability of a
#                predictions performed by a classifier in decreasing order
#                and giving the true labels of the obs in that order.
#                (Can also be unsorted, but then probsTarget needs to be specified)
#        target - one of the two elements in truth (the one that corresponds
#                 to the positive class)
#        probsTarget - vector of same length as truth, giving the scores/probs
#                      for each of the observations
#
# Output: TPR-TNR ROC-Curve
#         auc-value
myPlotRoc = function(target, truth, probsTarget = NULL, add = FALSE, ...) {
  
  # eventually sort truth vector
  if(!is.null(probsTarget)) {
    dfToSort = data.frame("truth" = truth, "probsTarget" = probsTarget)
    dfToSort = dfToSort[order(-dfToSort$probsTarget), ]
    truth = dfToSort$truth
  }
  
  # set variables for plotting
  currentXpos = 0
  currentYpos = 0
  xStepSize = 1 / length(truth[truth != target])
  yStepSize = 1 / length(truth[truth == target])
  auc = 0
  
  # plot skeleton
  if(!add) {
    plot(NA, xlim = c(0,1), ylim = c(0,1), 
         xlab = "FPR", ylab = "TPR", 
         cex.axis = 1.5, cex.lab = 1.5, ...)
    abline(0, 1, lty = 2, col = "grey")
  }
  
  # plot TPR-TNR ROC-Curve
  for(value in truth) {
    if(value == target) {
      newYpos = currentYpos + yStepSize
      lines(c(currentXpos, currentXpos), c(currentYpos, newYpos), ...)
      currentYpos = newYpos
    } else {
      newXpos = currentXpos + xStepSize
      lines(c(currentXpos, newXpos), c(currentYpos, currentYpos), ...)
      currentXpos = newXpos
      # add to auc
      auc = auc + xStepSize * currentYpos
    }
  }
  
  return(auc)
}


# Ich bin mir unsicher, ob die probs fuer Trump oder fuer Harris sind
# aka wer das target sein muss.
# 
# Im folgenden wird Trump angenommen, das passte von den probs her augenscheinlich etwas besser.

pdf(file = "rocCurve.pdf")
par(mar = c(4.5,4,1,1) + 0.1)
# plot roc curve for cross-validated predictions fullModel
myPlotRoc(target = electiondata$Leading_Candidate[1], # Trump
          truth = electiondata$Leading_Candidate,
          probsTarget = plotRocDf$predsFullModel
)
# plot roc curve for cross-validated predictions of bestsubset model
myPlotRoc(target = electiondata$Leading_Candidate[1], # Trump
          truth = electiondata$Leading_Candidate,
          probsTarget = plotRocDf$predsBssModel,
          add = TRUE,
          col = "blue"
)
legend("bottomright", legend = c("maximales Modell", "reduziertes Modell"),
       lty = 1, col = c("black", "blue"), cex = 1.5)
dev.off()

# Zur Kontrolle die wenn man Harris als target annimmt

# plot roc curve for cross-validated predictions fullModel
myPlotRoc(target = electiondata$Leading_Candidate[5], # Harris
          truth = electiondata$Leading_Candidate,
          probsTarget = plotRocDf$predsFullModel
)
# plot roc curve for cross-validated predictions of bestsubset model
myPlotRoc(target = electiondata$Leading_Candidate[5], # Harris
          truth = electiondata$Leading_Candidate,
          probsTarget = plotRocDf$predsBssModel
)
