### File for manually implementing CrossValidation and ROC-Curves:

# SETUP
{
  # setwd
  setwd("C:/Users/jorri/Documents/GitHub/fallstudien/Projekt 5")
  # load data
  electiondata = read.csv("US_election_2024.csv", sep = ";", header = T, dec = ",")
  # transform data
  electiondata$Leading_Candidate = as.factor(electiondata$Leading_Candidate)
  data = electiondata
  
  # set seed (cause of randomness in cv)
  set.seed(19042101)
}

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
myPlotRoc = function(target, truth, probsTarget = NULL, ...) {
  
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
  plot(NA, xlim = c(0,1), ylim = c(0,1), 
       xlab = "TNR", ylab = "TPR", ...)
  abline(0, 1, lty = 2, col = "grey")
  
  # plot TPR-TNR ROC-Curve
  for(value in truth) {
    if(value == target) {
      newYpos = currentYpos + yStepSize
      lines(c(currentXpos, currentXpos), c(currentYpos, newYpos))
      currentYpos = newYpos
    } else {
      newXpos = currentXpos + xStepSize
      lines(c(currentXpos, newXpos), c(currentYpos, currentYpos))
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

# plot roc curve for cross-validated predictions fullModel
myPlotRoc(target = electiondata$Leading_Candidate[1], # Trump
          truth = electiondata$Leading_Candidate,
          probsTarget = plotRocDf$predsFullModel
          )
# plot roc curve for cross-validated predictions of bestsubset model
myPlotRoc(target = electiondata$Leading_Candidate[1], # Trump
          truth = electiondata$Leading_Candidate,
          probsTarget = plotRocDf$predsBssModel
          )


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
