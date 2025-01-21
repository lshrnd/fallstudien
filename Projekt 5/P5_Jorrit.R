

# SETUP
{
  library(corrplot)
  library(glmnet)
  library(pROC)
  library(ROCR)
  library(dplyr)
  library(caret)
  library(mlr3)
  library(mlr3learners)
  library(mlr3viz)
  
  setwd("C:/Users/jorri/Documents/GitHub/fallstudien/Projekt 5")
  electiondata = read.csv("US_election_2024.csv", sep = ";", header = T, dec = ",")
  
  electiondata$Leading_Candidate = as.factor(electiondata$Leading_Candidate)
  str(electiondata)
}

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

predict.glm(fullModel, electiondata, type = "response")

# deskription
# bivariat mir leading candidate
# abhängigkeiten (linear)
#
# variablen traffos probieren
#
# welches selektionsverfahren

corrplot(cor(electiondata[, 3:11]))


trump = subset(electiondata, Leading_Candidate == "Trump")
harris = subset(electiondata, Leading_Candidate == "Harris")

plot(electiondata[, 3:11], col = electiondata$Leading_Candidate)


# Normalisiere Datensatz

mus = sapply(electiondata[, 3:11], mean)
sds = sapply(electiondata[, 3:11], sd)

electiondataNormed = electiondata

for(i in 3:11) {
  electiondataNormed[, i] = (electiondataNormed[, i] - mus[i - 2]) / sds[i - 2]
}


fullModelNormedData = glm(Leading_Candidate ~
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
                family = "binomial",
                data = electiondataNormed)
summary(fullModelNormedData)

#glmnet(electiondata[, 2:11], Leading_Candidate, family = "binomial")


roccurve = roc(electiondata$Leading_Candidate, predict(fullModel))
plot(roccurve)
roccurve$auc

{
trainControlSplit = trainControl(method = "cv", number = 1)
model = train(Leading_Candidate ~ 
                Total_Area + 
                Population + 
                Population_Density +
                Median_Age +
                Birth_Rate +
                HDI +
                Unemployment_Rate +
                Health_Insurance_Coverage +
                Median_Rent,
                data = electiondata,
                method = "glm",
                family = binomial,
                trControl = trainControlSplit)
summary(model)
}


#### Cross-Validation for all-parameter model (with mlr3)


?mlr_learners
lrn()

# construct log_reg learner
logReg = lrn("classif.log_reg", predict_type = "prob")
# construct split
resampling = rsmp("cv", folds = 5)
# construct task for electiondata
?tsk
?TaskClassif
?as_task_classif
task_electiondata = as_task_classif(electiondata[, 2:11], target = "Leading_Candidate")

# do the cross-validation
resampleResults = resample(task_electiondata, logReg, resampling)

# plot Roc-Curve für cross-validation results
autoplot(resampleResults, type = "roc")


resampleResults$predictions()[[1]]

resampleResults$score(msr("classif.acc"))

resampleResults$score(msr("classif.tpr"))

resampleResults$help()


rrPreds = resampleResults$prediction()
rrPreds$prob
rrPreds$response
electiondata$Leading_Candidate

# gather all info needed for drawing roc curve manually
rocDf = data.frame("truth" = electiondata$Leading_Candidate,
                   "prediction" = rrPreds$response,
                   "probHarris" = rrPreds$prob[, 1],
                   "probTrump" = rrPreds$prob[ , 2])
rocDf

# order by prob for harris (decreasing)
rocDf_byHarris = rocDf[order(-rocDf$probHarris), ]

# order by prob for trump
rocDf[order(-rocDf$probTrump), ]

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

# target = Harris
myPlotRoc(target = rocDf_byHarris$prediction[1],
          truth = rocDf_byHarris$truth)
myPlotRoc(target = rocDf_byHarris$prediction[1],
          truth = rocDf$truth,
          probsTarget = rocDf$probHarris)

# target = Trump
myPlotRoc(target = rocDf$truth[1],
          truth = rocDf$truth,
          probsTarget = rocDf$probTrump)


# Anmerkungen:
#
# - meine ROC-Curves stimmen überhaupt nicht mit dem autoplot überein.
# - ROC-Curves verschieden, jenachdem welche Klasse das target ist
#     -> auch AUC values leicht unterschiedlich




### Bestsubset ROC-Curves with resampling:

varIndexes = df$all_combinations[1]
names(normedData[,unlist(varIndexes)])



# construct log_reg learner
logRegBSS = lrn("classif.log_reg", predict_type = "prob")
# construct split
resamplingBSS = rsmp("cv", folds = 5)
# construct task for electiondata - Best SubSet
task_electiondataBSS = as_task_classif(electiondata[, c(2, 4, 6, 9, 10, 11)], target = "Leading_Candidate")

# do the cross-validation
resampleResultsBSS = resample(task_electiondataBSS, logRegBSS, resamplingBSS)

# plot Roc-Curve für cross-validation results
autoplot(resampleResultsBSS, type = "roc")


resampleResultsBSS$score(msr("classif.auc"))

resampleResultsBSS$predictions()[[1]]

resampleResultsBSS$score(msr("classif.acc"))

resampleResultsBSS$score(msr("classif.tpr"))

resampleResults$help()


rrPredsBSS = resampleResultsBSS$prediction()
rrPredsBSS$row_ids
rrPredsBSS$prob
rrPredsBSS$response
electiondata$Leading_Candidate

# gather all info needed for drawing roc curve manually
rocDfBSS = data.frame("truth" = electiondata$Leading_Candidate,
                   "prediction" = rrPredsBSS$response,
                   "probHarris" = round(rrPredsBSS$prob[, 1], 2),
                   "probTrump" = round(rrPredsBSS$prob[ , 2], 2))
rocDfBSS[order(-rocDfBSS$probHarris), ]



# target = harris
myPlotRoc(target = rocDfBSS$truth[5],
          truth = rocDfBSS$truth,
          probsTarget = rocDfBSS$probHarris)
# target = trump
myPlotRoc(target = rocDfBSS$truth[1],
          truth = rocDfBSS$truth,
          probsTarget = rocDfBSS$probsTrump)



















# Versuch Kreuzvalidierung

logRegression_mod = getModelInfo("logReg", regex = FALSE)[[1]]
logRegression_mod$fit = function(x, y, wts, param, lev, last, classProbs, ...) {
  electiondata$Leading_Candidate = y
  glm(Leading_Candidate ~
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
      family = "binomial",
      data = electiondataNormed)
}
logRegression_mod$library = "stats"
logRegression_mod$type = "Classification"
logRegression_mod$parameters = data.frame(parameter = "parameter",
                                          class = "character",
                                          label = "parameter")
logRegression_mod$grid = function(x, y, len = NULL, search = "grid") 
  data.frame(parameter = "none")
logRegression_mod$predict = function(modelFit, newdata, submodels = NULL) {
  if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
  ## By default a matrix is returned; we convert it to a vector
  predict(modelFit, newdata)[,1]
}
logRegression_mod$prob = 0

mod = train(x = electiondata[ , 3:11], y = electiondata$Leading_Candidate, 
            method = logRegression_mod,
            trControl = trainControlSplit)




offset_mod <- getModelInfo("glm", regex = FALSE)[[1]]
offset_mod$fit <- function(x, y, wts, param, lev, last, classProbs, ...) {
  dat <- if(is.data.frame(x)) x else as.data.frame(x)
  dat$Postwt <- y
  glm(Postwt ~ Prewt + Treat + offset(Prewt), family = gaussian, data = dat)
}

mod <- train(x = anorexia[, 1:2], y = anorexia$Postwt, method = offset_mod)
coef(mod$finalModel)
