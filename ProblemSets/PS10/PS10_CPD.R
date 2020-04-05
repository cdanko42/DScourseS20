set.seed(100)

income <- read.table("ProblemSets/PS10/adult.data")
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)
#   age: continuous.
#   workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#   fnlwgt: continuous.
#   education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#   education-num: continuous.
#   marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
#   occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
#   relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#   race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#   sex: Female, Male.
#   capital-gain: continuous.
#   capital-loss: continuous.
#   hours-per-week: continuous.
#   native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.

######################
# Clean up the data
######################
# Drop unnecessary columns
income$native.country <- NULL
income$fnlwgt         <- NULL
# Make sure continuous variables are coded as such
income$age            <- as.numeric(income$age)
income$hours          <- as.numeric(income$hours)
income$education.num  <- as.numeric(income$education.num)
income$capital.gain   <- as.numeric(income$capital.gain)
income$capital.loss   <- as.numeric(income$capital.loss)
# Combine levels of categorical variables that currently have too many levels
levels(income$education) <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))
levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))
levels(income$race) <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))
levels(income$workclass) <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))
levels(income$occupation) <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))

# Break up the data:
n <- nrow(income)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
income.train <- income[train,]
income.test  <- income[test, ]

library(mlr)
library(magrittr)
library(tidyverse)

# Define the task:
theTask <- makeClassifTask(id = "taskname", data = income.train, target = "high.earner")

#Define tuning process
tuneMethod <- makeTuneControlRandom(maxit = 10L)

#Define resampling process by k-fold cross validation with 3 folds
resampleStrat <- makeResampleDesc(method = "CV", iters = 3)

library(rpart)
library(e1071)
library(kknn)
library(nnet)

#Create learners
tree <- makeLearner("classif.rpart", predict.type = "response")
logreg <- makeLearner("classif.glmnet", predict.type = "response")
nnet <- makeLearner("classif.nnet", predict.type = "response")
naivebayes <- makeLearner("classif.naiveBayes", predict.type = "response")
knn <- makeLearner("classif.kknn", predict.type = "response")
svm <- makeLearner("classif.svm", predict.type = "response")

#Create tree parameters
treeParams <- makeParamSet(makeIntegerParam("minsplit",lower=10,upper=50),
                           makeIntegerParam("minbucket",lower=5,upper=50), 
                           makeNumericParam("cp", lower=.001, upper=.2))

#Create logit parameters 
logitParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=3),
                            makeNumericParam("alpha",lower=0,upper=1))

#Create neural net parameters
nnetParams <- makeParamSet(makeIntegerParam("size",lower=1,upper=10),
                           makeNumericParam("decay",lower=.1,upper=.5), 
                           makeIntegerParam("maxit", lower=1000, upper=1000))

#Create kNN parameters
kNNParams <- makeParamSet(makeIntegerParam("k",lower=1,upper=30))

#Create SVM parameters
paramset <- list(2^-2, 2^-1,1, 2, 4, 2^10)
SVMParams <- makeParamSet(makeDiscreteParam("kernel", values="radial" ),
                          makeDiscreteParam("cost",values= paramset), 
                          makeDiscreteParam("gamma", values= paramset))

#Tune tree
tunemeasures <- list(f1, gmean)
tunedTree <- tuneParams(learner = tree,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = tunemeasures,       # RMSE performance measure, this can be changed to one or many
                         par.set = treeParams,
                         control = tuneMethod,
                         show.info = TRUE)
#Apply parameters to model
tree <- setHyperPars(learner=tree, par.vals=tunedTree$x)

#Verify performance using cross validated sample sets
resample(tree, theTask, resampleStrat, measures=tunemeasures)

#Train the final model
finalTree <- train(learner= tree, task=theTask)

#Predict in test set
treepredict <- predict(finalTree, newdata=income.test)
treepredict <- as.data.frame(treepredict)

#Evaluate peformance
library(caret)
confusionMatrix(treepredict$response, treepredict$truth)


#Tune logit
tunedLogit <- tuneParams(learner = logreg,
                        task = theTask,
                        resampling = resampleStrat,
                        measures = tunemeasures,      
                        par.set = logitParams,
                        control = tuneMethod,
                        show.info = TRUE)
#Apply parameters to model
logreg <- setHyperPars(learner=logreg, par.vals=tunedLogit$x)

#Verify performance using cross validated sample sets
resample(logreg, theTask, resampleStrat, measures=tunemeasures)


#Train the final model
finalLog <- mlr::train(learner= logreg, task=theTask)


#Predict in test set
logpredict <- predict(finalLog, newdata=income.test)
logpredict <- as.data.frame(logpredict)

#Evaluate peformance
confusionMatrix(logpredict$response, logpredict$truth)

#Generate tables for each set of parameters
logtable <- hux(tunedLogit$x, add_colnames =TRUE)


#Tune logit
tunedAI <- tuneParams(learner = nnet,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = tunemeasures,      
                         par.set = nnetParams,
                         control = tuneMethod,
                         show.info = TRUE)
#Apply parameters to model
nnet <- setHyperPars(learner=nnet, par.vals=tunedAI$x)

#Verify performance using cross validated sample sets
resample(nnet, theTask, resampleStrat, measures=tunemeasures)


#Train the final model
finalAI <- mlr::train(learner= nnet, task=theTask)

#Predict in test set
AIpredict <- predict(finalAI, newdata=income.test)
AIpredict <- as.data.frame(AIpredict)

#Evaluate peformance
confusionMatrix(AIpredict$response, AIpredict$truth)

#Generate tables for each set of parameters
AItable <- hux(tunedAI$x, add_colnames =TRUE)



#Verify naive Bayes performance using cross validated sample sets
resample(naivebayes, theTask, resampleStrat, measures=tunemeasures)


#Train the final model
finalBayes <- mlr::train(learner=naivebayes, task=theTask)


#Predict in test set
Bayespredict <- predict(finalBayes, newdata=income.test)
Bayespredict <- as.data.frame(Bayespredict)

#Evaluate peformance
confusionMatrix(Bayespredict$response, Bayespredict$truth)

#Tune k-nerarnest neighbor
knnTuned <- tuneParams(learner = knn,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = tunemeasures,      
                         par.set = kNNParams,
                         control = tuneMethod,
                         show.info = TRUE)
#Apply parameters to model
knn <- setHyperPars(learner=knn, par.vals=knnTuned$x)

#Verify performance using cross validated sample sets
resample(knn, theTask, resampleStrat, measures=tunemeasures)


#Train the final model
finalknn <- mlr::train(learner= knn, task=theTask)


#Predict in test set
knnpredict <- predict(finalknn, newdata=income.test)
knnpredict <- as.data.frame(knnpredict)

#Evaluate peformance
confusionMatrix(knnpredict$response, knnpredict$truth)


#Tune svm
tunedSVM <- tuneParams(learner = svm,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = tunemeasures,      
                         par.set = SVMParams,
                         control = tuneMethod,
                         show.info = TRUE)
#Apply parameters to model
svm <- setHyperPars(learner=svm, par.vals=tunedSVM$x)

#Verify performance using cross validated sample sets
resample(svm, theTask, resampleStrat, measures=tunemeasures)


#Train the final model
finalSVM <- mlr::train(learner= svm, task=theTask)


#Predict in test set
SVMpredict <- predict(finalSVM, newdata=income.test)
SVMpredict <- as.data.frame(SVMpredict)

#Evaluate peformance
confusionMatrix(SVMpredict$response, SVMpredict$truth)

#Generate tables for each set of parameters
svmtable <- hux(tunedSVM$x, add_colnames =TRUE)







