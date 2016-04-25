##########################################
# Load the dataset and remove account # #
##########################################
accounts <- read.csv('accounts_churn_contact_flag_final.csv', stringsAsFactors = F)
str(accounts)
accounts$ ï..account <- NULL
accounts$CONTRACT_FLAG <- as.factor(accounts$CONTRACT_FLAG)
accounts$churn <- as.factor(accounts$churn)
accounts$indseg1 <- as.factor(accounts$indseg1)
accounts$Corp_Maj_Flag <- as.factor(accounts$Corp_Maj_Flag)
accounts$dunsstat <- as.factor(accounts$dunsstat)
accounts$Customer_Size <- as.factor(accounts$Customer_Size)
accounts$DUNSSBUS <- NULL
accounts$DUNSPUBL <- NULL
accounts$contract_group <- NULL
accounts$sales_201509 <- NULL
accounts$sales_201510 <- NULL
accounts$sales_201511 <- NULL

# Create some scatter plots
library(ggplot2)
ggplot(accounts, aes(x = TRANS12X, y = TENURE, color = churn)) + geom_point()

# Create some histograms
library(ggplot2)
ggplot(accounts, aes(RECENCY)) + geom_bar()
ggplot(accounts, aes(TENURE)) + geom_bar()

# Some EDA
names(accounts)
table(accounts$Customer_Size)
table(accounts$CONTRACT_FLAG)
table(accounts$mro_decile)
table(accounts$indseg0)

# 22.7% accounts lapsed in the data.
table(accounts$churn)/nrow(accounts)

# Check how the churn values are distributed across different categorical variables.
sort(tapply(accounts$churn, accounts$indseg0, mean), decreasing = T)
table(accounts$churn, accounts$indseg0)
sort(tapply(accounts$churn, accounts$Customer_Size, mean), decreasing = T)
table(accounts$churn, accounts$Customer_Size)
# Looks like indseg0 and customer_size are two factors that can
# separate churn and non-churn.
sort(tapply(accounts$churn, accounts$CONTRACT_FLAG, mean), decreasing = T)
table(accounts$churn, accounts$CONTRACT_FLAG)
sort(tapply(accounts$churn, accounts$multisite, mean), decreasing = T)
# INVSOLFLG is also important
sort(tapply(accounts$churn, accounts$INVSOLFLG, mean), decreasing = T)
# mro_decile is significant
sort(tapply(accounts$churn, accounts$mro_decile, mean), decreasing = T)
sort(tapply(accounts$churn, accounts$contract_group, mean), decreasing = T)
table(accounts$churn, accounts$distance_far)
sort(tapply(accounts$churn, accounts$distance_far, mean), decreasing = T)

#######################################################
# Feature engineering
#######################################################
library(dplyr)
# WA_S12X ranges from -206700 to large number, hard to get any sense out of it.
accounts <- mutate(accounts, discount = (WA_S12X - (SALES12X - FINDS12X))/WA_S12X)
ggplot(accounts, aes(discount)) + geom_bar()
accounts <- mutate(accounts, sellertype = CSG %/% 10000)
accounts$CSG <- NULL
accounts$sellertype <- as.factor(accounts$sellertype)
accounts <- mutate(accounts, distance_far = (DISTANCE >= 5))
accounts <- mutate(accounts, SOW = SALES12X/mrospend)
# INCLUDING DISTANCE_FAR DIDN'T BOOST THE MODEL PERFORMANCE.
accounts <- mutate(accounts, trans_3month = TRANS01 + TRANS02 + TRANS03)


# split the dataset to training/test
library(caTools)
set.seed(88)
spl <- sample.split(accounts$churn, SplitRatio = 0.8)
accounts_train <- subset(accounts, spl == T)
accounts_test <- subset(accounts, spl == F)
table(accounts_train$churn)/nrow(accounts_train)
# The distribution of churn in the training data is the same as in original data.

########################################################
# Baseline model
########################################################
table(accounts_test$churn)
# Baseline model accuracy is 77.3%.

########################################################
# Build a logistic regression model
########################################################
logReg <- glm(churn ~ CONTACTS + TENURE + log(TRANS12X) + LINES12X  + indseg1 + contract_group + log(mrospend) + sellertype, data = accounts_train, family = binomial)
summary(logReg)

# All variables are significant.

# Evaluate the model
library(caret)
varImp(logReg)
predict_logReg <- predict(logReg, newdata = accounts_test, type = 'response')
# Note that both arguments in the confusionMatrix have to have the same values (either T/F or 0/1)
## Also note that positive argument is needed to specify which result is defined as true.
confusionMatrix(predict_logReg >= 0.5, accounts_test$churn==1, positive = "TRUE")
# accuracy is 84.09%
# Sensitivity is 55.10%
## May prefer models with higher overall accuracy but also lower false negative, thus higher sensitivity.


# Calculate AUC value and generate the ROC curve. AUC = 0.8905507
library(ROCR)
ROCRpred <- prediction(predict_logReg, accounts_test$churn)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf <- performance(ROCRpred, "tpr", "fpr")
plot(perf)

########################################################
# Build a CART model
########################################################
library(rpart)
library(rpart.plot)
CART <- rpart(churn ~ CONTACTS + TENURE + log(TRANS12X) + LINES12X  + indseg1 + 
                contract_group + log(mrospend) + sellertype, data = accounts_train, 
              method = "class", control = rpart.control(minsplit = 5, cp = 0.005))
CART <- rpart(churn ~ log(TRANS12X), data = accounts_train, method = "class", minbucket = 25)
prp(CART)
# Evaluate the model
predictCART <- predict(CART, newdata = accounts_test, type = "class")
confusionMatrix(predictCART, accounts_test$churn, positive = "1")
# accuracy is 83.74%
# sensitivity is 51.95%
## Note that the model that includes a lot of variables and the one that only includes log(TRANS12X)
## have exactly the same metrics.

library(ROCR)
predictROC <- predict(CART, newdata = accounts_test)
ROCRpred <- prediction(predictROC[,2], accounts_test$churn)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf <- performance(ROCRpred, "tpr", "fpr")
plot(perf)
# AUC value = 0.8249959


########################################################
# 10 fold Cross validation
########################################################

## logistic regression model
k <- 10
list <- 1:k
accounts$id <- sample(1:k, nrow(accounts), replace = T)
table(accounts$id)/nrow(accounts)
accuracy <- vector()
sensitivity <- vector()
AUC <- vector()
for (i in 1:k) {
  trainset <- subset(accounts, id %in% list[-i])
  testset <- subset(accounts, id %in% c(i))
  logReg <- glm(churn ~ CONTACTS + TENURE + log(TRANS12X) + LINES12X  + indseg1 + contract_group + log(mrospend) + sellertype, data = trainset, family = binomial)
  predict_logReg <- predict(logReg, newdata = testset, type = 'response')
  # confusion_matrix <- table(testset$churn == 1, predict_logReg >= 0.5)
  # accuracy <- (confusion_matrix[[1]] + confusion_matrix[[4]])/nrow(testset)
  # sensitivity <- confusion_matrix[[4]]/(confusion_matrix[[2]] + confusion_matrix[[4]])
  # print(paste("training set churn % is", table(trainset$churn)/nrow(trainset)))
  # print(paste("test set churn % is", table(testset$churn)/nrow(testset)))
  cm <- confusionMatrix(predict_logReg >= 0.5, testset$churn==1, positive = "TRUE")
  # print (confusionMatrix(predict_logReg >= 0.5, testset$churn==1, positive = "TRUE")$overall['Accuracy'])
  # print (confusionMatrix(predict_logReg >= 0.5, testset$churn==1, positive = "TRUE")$byClass["Sensitivity"])
  ROCRpred <- prediction(predict_logReg, testset$churn)
  accuracy <- c(accuracy, cm$overall['Accuracy'])
  sensitivity <- c(sensitivity, cm$byClass["Sensitivity"])
  AUC <- c(AUC, as.numeric(performance(ROCRpred, "auc")@y.values))
}
print (paste("Average accuracy is", mean(accuracy)))
print (paste("Average sensitivity is", mean(sensitivity)))
print (paste("Average AUC value is", mean(AUC)))


## Random forest model using randomForest package
library(randomForest)
set.seed(18)
# randomForest function doesn't recognize log(TRANS12X)
RF <- randomForest(churn ~ CREDIT + CONTACTS + RECENCY + TENURE + TRANS12X + LINES12X  +
                     indseg1 + mrospend + contract_group + sellertype, 
                   data = accounts_train, 
                   ntree = 1000, 
                   nodesize = 2, 
                   do.trace = T)
RF_predict <- predict(RF, newdata = accounts_test)
library(caret)
confusionMatrix(RF_predict, accounts_test$churn, positive = "1")
# Accuracy is 83.97%
# Sensitivity is 58.95%.
# Kappa = 0.524
varImp(RF)
varImpPlot(RF)



########################################################
# CARET training
########################################################

## Random forest model using caret package
# create a stratified random sample of the data into training and test sets:
library(caret)
set.seed(998)
inTraining <- createDataPartition(accounts$churn, p = 0.2, list = F)
training <- accounts[inTraining,]
testing <- accounts[-inTraining,]
levels(training$churn) <- c("No", "Yes")
levels(testing$churn) <- c("No", "Yes")
#### Need to make sure all the categorical predictors have the same levels between 
#### training and testing data, otherwise predict function will throw out errors.
levels(training$indseg1) <- levels(accounts$indseg1)
levels(testing$indseg1) <- levels(accounts$indseg1)
# levels(training$contract_group) <- levels(accounts$contract_group)
# levels(testing$contract_group) <- levels(accounts$contract_group)
levels(training$sellertype) <- levels(accounts$sellertype)
levels(testing$sellertype) <- levels(accounts$sellertype)
levels(training$dunsstat) <- levels(accounts$dunsstat)
levels(testing$dunsstat) <- levels(accounts$dunsstat)
levels(training$Customer_Size) <- levels(accounts$Customer_Size)
levels(testing$Customer_Size) <- levels(accounts$Customer_Size)
levels(training$Corp_Maj_Flag) <- levels(accounts$Corp_Maj_Flag)
levels(testing$Corp_Maj_Flag) <- levels(accounts$Corp_Maj_Flag)
table(training$churn)/nrow(training)
table(testing$churn)/nrow(testing)


# Use non-formula form to speed up.
# Never use the default nodesize -- 1. 
## The first model is used to calculate AUC value and the 2nd one calculates accuracy and 
## Kappa. 
# Try ntree = 400, 300, 1000. Can't tune via tuneGrid. have to manually try.
# RF <- train(training[is.na(training$DISTANCE) != T, c("DISTANCE", "RECENCY", "TENURE", "RET_T12", "TRANS12X", "TRANS24X", "LINES12X", "indseg1",
#                        "contract_group", "sellertype", "EPEDN12X", "trans_3month", "EBUN12X",
#                        "Customer_Size", "Corp_Maj_Flag", "SOW", "dunsstat")], 
#             training[is.na(training$DISTANCE)!=T,]$churn,
#             # nodesize = 1, 
#             ntree = 1000,
#             method = "rf", 
#             metric = "ROC", 
#             trControl = trainControl(method = "cv", number = 5, classProbs = TRUE, 
#                                      summaryFunction = twoClassSummary),
#             tuneGrid = expand.grid(mtry = 2),
#             do.trace = T)
# RF
set.seed(80)
## Tune mtry
RF_tuning <- train(training[is.na(training$DISTANCE) != T, c("DISTANCE", "RECENCY", "TENURE", "RET_T12", "TRANS12X", "TRANS24X", "LINES12X", "indseg1",
                                                            "sellertype", "EPEDN12X", "trans_3month", "EBUN12X",
                                                            "Customer_Size", "Corp_Maj_Flag", "SOW", "dunsstat")], 
                  training[is.na(training$DISTANCE) != T,]$churn,
                  # nodesize = 1, 
                  ntree = 500,
                  method = "rf", 
                  metric = "ROC",
                  trControl = trainControl(method = "cv", 
                                           number = 5,
                                           summaryFunction = multiClassSummary,
                                           classProbs = TRUE),
                  # tuneGrid = expand.grid(mtry = c(2, 3, 4)),
                  tuneLength = 5,
                  do.trace = T) 
RF_tuning
ggplot(RF_tuning)
## Use mtry = 2.
## Automatic feature selection by RF.
set.seed(80)
RF_RFE_1000 <- train(training[complete.cases(training),c(1:198, 201, 203)],
                training[complete.cases(training), 199],
                ntree = 1000,
                method = "rf", 
                metric = "ROC",
                trControl = trainControl(method = "cv", 
                                         number = 5,
                                         summaryFunction = multiClassSummary,
                                         classProbs = TRUE),
                tuneGrid = expand.grid(mtry = c(14, 16, 18)),
                do.trace = T)
RF_RFE_1000
varImp(RF_RFE_1000)
ggplot(RF_RFE_1000)
plot(varImp(RF_RFE_1000), top = 20)
# mtry = c(2, 3, 4) ntree = 1000   BEST
# Best mtry = 4
# ROC: 0.8959951
# Accuracy: 0.8467142
# Kappa: 0.5322052
# Sens: 0.9238528

# mtry = c(2, 3, 4) ntree = 500
# best mtry = 4
# ROC: 0.8958161
# Accuracy: 0.8459994
# Kappa: 0.5300067
# Sens: 0.9234023

# mtry = c(16, 12, 8) ntree = 1000, sellertype excluded
# best mtry = 16
# ROC: 0.8972314
# Accuracy: 0.8470793
# Kappa: 0.5335432
# Sens: 0.9239116

# mtry = c(14, 16, 18) ntree = 1000, including sellertype
# best mtry = 18
# ROC: 0.8972072
# Accuracy: 0.8475659
# Kappa: 0.5358697
# Sens: 0.9235198

pred <- predict(RF_RFE_1000, newdata = testing[complete.cases(testing),c(1:198, 201, 203)], 
                type = "prob")
confusionMatrix(pred[, 2] >= 0.5, testing[complete.cases(testing), ]$churn == "Yes", positive = "TRUE")
## Evaluate the model on testing data.
# Accuracy: 0.8477  
# Kappa: 0.5378
# Sens: 0.5880
library(ROCR)
ROCRpred <- prediction(pred[,2], testing[complete.cases(testing), ]$churn)
as.numeric(performance(ROCRpred, "auc")@y.values)
# AUC value = 0.8985856


set.seed(80)
RF <- train(training[is.na(training$DISTANCE) != T, c("DISTANCE", "RECENCY", "TENURE", "RET_T12", "TRANS12X", "TRANS24X", "LINES12X", "indseg1",
                       "sellertype", "trans_3month", "EBUN12X",
                       "Customer_Size", "Corp_Maj_Flag", "SOW", "dunsstat")], 
            training[is.na(training$DISTANCE) != T,]$churn,
            # nodesize = 1, 
            ntree = 1000,
            method = "rf", 
            metric = "ROC",
            trControl = trainControl(method = "cv", 
                                     number = 5,
                                     summaryFunction = multiClassSummary,
                                     classProbs = TRUE),
            tuneGrid = expand.grid(mtry = 2),
            do.trace = T)
RF
ggplot(RF)
## Evaluate the model on CV data.
# ROC = 0.896901 (requires class probabilities) 
# Sens = 0.9113742  
# Accuracy = 0.8457903    (random forest votes for the binary outcome, default cutoff
# is 1/k (k is the # of classes), in our case, cutoff = 0.5.
# But for a single tree, what is the cut off??
# Kappa = 0.5481246 
varImp(RF, scale = F)
plot(varImp(RF, scale = F))
plot(varImp(RF, scale = T))
ggplot(RF)
## Only include the predictors from the model, otherwise we have to make sure all
## categorical variables have the same levels between training and testing.
pred <- predict(RF, newdata = testing[is.na(testing$DISTANCE) != T, c("DISTANCE","RECENCY", "TENURE", "RET_T12", "TRANS12X", "TRANS24X", "LINES12X", "indseg1",
                                         "sellertype", "trans_3month", "EBUN12X",
                                         "Customer_Size", "Corp_Maj_Flag", "SOW", "dunsstat")], 
                type = "prob")
confusionMatrix(pred[, 2] >= 0.5, testing[is.na(testing$DISTANCE) != T, ]$churn == "Yes", positive = "TRUE")
## Evaluate the model on testing data.
# Accuracy: 0.8461  
# Kappa: 0.5508
# Sens: 0.6277
library(ROCR)
ROCRpred <- prediction(pred[,2], testing[is.na(testing$DISTANCE) != T,]$churn)
as.numeric(performance(ROCRpred, "auc")@y.values)
# AUC value = 0.8977378
plot(perf, colorize=T, 
     print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(1.2,1.2), 
     avg="threshold", 
     lwd=3)

############################################################################
############################################################################
## Logistic regression model using caret

# When class probabilities are requested, train puts them into a data frame with a column 
# for each class. If the factor levels are not valid variable names, they are automatically
# changed (e.g. "0" becomes "X0").
levels(training$churn) <- c("No", "Yes")
# preProcValues <- preProcess(training, method = "scale")
# trainTransformed <- predict(preProcValues, training)
# testTransformed <- predict(preProcValues, testing)
set.seed(80)
# Note that to calculate the ROC curve, we need the model to predict the
# class probabilities. The classProbs option will also do this. However, if we set
# classProbs = TRUE, we won't be able to calculate accuracy later, AUC will be calculated
# instead. 
# logReg_caret <- train(churn ~ DISTANCE + RECENCY + TENURE + RET_T12 + log(TRANS12X) + log(TRANS24X + 1) + LINES12X  + indseg1 + 
#                         contract_group + sellertype + EPEDN12X + trans_3month + EBUN12X + Customer_Size + SOW + Corp_Maj_Flag, 
#                       data = training, 
#                       method = "glm", 
#                       metric = "ROC",
#                       trControl = trainControl(method = "cv", 
#                                                number = 10, 
#                                                summaryFunction = twoClassSummary,
#                                                classProbs = TRUE), 
#                       family = binomial)
# Following threw an error "all the ROC metric values are missing", if including twoClassSummary.
## set summaryFunction to be multiClassSummary and set classProbs = T, metric = "ROC"
## This way one can see both ROC and accuracy, Kappa and sensitivity.
##################################################################################
## RFE feature selection  -- doesn't work
##################################################################################

set.seed(80)
logReg_caret <- train(churn ~ DISTANCE + RECENCY + TENURE + RET_T12 + log(TRANS12X) + log(TRANS24X + 1) + LINES12X  + indseg1 + 
                        sellertype + EPEDN12X + trans_3month + EBUN12X + Customer_Size + SOW + Corp_Maj_Flag, 
                      data = training, 
                      method = "glm", 
                      metric = "ROC",
                      trControl = trainControl(method = "cv", 
                                               number = 5,
                                               summaryFunction = multiClassSummary,
                                               classProbs = TRUE),
                      family = binomial)
logReg_caret
varImp(logReg_caret)
# ROC = 0.8957514
# Accuracy = 0.8447169
# Sensitivity = 0.9233785
# Kappa = 0.5301955

summary(logReg_caret)
# Note that for predict.train under caret, type argument can only be "raw" or "prob"
# Also note that pred actually is a two column data frame.
pred <- predict(logReg_caret, newdata = testing, type = "prob")
confusionMatrix(pred[, 2] >= 0.5, testing$churn == 1, positive = "TRUE")
## Need to exclude the missing values in distance, otherwise confusionMatrix won't work.
confusionMatrix(pred[, 2] >= 0.5, testing[is.na(testing$DISTANCE) != T,]$churn == "Yes", positive = "TRUE")
## Accuracy = 0.8456
## Kappa = 0.5333
## Sensitivity = 0.5790
library(ROCR)
ROCRpred <- prediction(pred[,2], testing$churn)
ROCRpred <- prediction(pred[,2], testing[is.na(testing$DISTANCE) != T,]$churn)
as.numeric(performance(ROCRpred, "auc")@y.values)
# AUC value = 0.8964744
perf <- performance(ROCRpred, "tpr", "fpr")
plot(perf)
plot(perf, colorize=T)
# Plot the cutoff points on the curve
plot(perf, colorize=T, 
     print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(1.2,1.2), 
     avg="threshold", 
     lwd=3)


cutoffs <- data.frame(cut=perf@alpha.values[[1]], 
                      fpr=perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
head(subset(cutoffs, fpr < 0.2))


############################################################################
############################################################################
## Naive Bayes using caret
set.seed(80)
NB <- train(churn ~ DISTANCE + RECENCY + TENURE + RET_T12 + log(TRANS12X) + log(TRANS24X + 1) + LINES12X  + indseg1 + 
              sellertype + EPEDN12X + trans_3month + EBUN12X + Customer_Size + SOW + Corp_Maj_Flag, 
            data = training[is.na(training$DISTANCE) != T,],
            method = "nb", 
            metric = "ROC",
            trControl = trainControl(method = "cv", 
                                     number = 5,
                                     summaryFunction = multiClassSummary,
                                     classProbs = TRUE))
NB
## Poor performance.

############################################################################
############################################################################
## K Nearesting Neighbors
library(caret)
set.seed(80)
KNN <- train(churn ~ DISTANCE + RECENCY + TENURE + RET_T12 + log(TRANS12X) + log(TRANS24X + 1) + LINES12X  + indseg1 + 
               sellertype + EPEDN12X + trans_3month + EBUN12X + Customer_Size + SOW + Corp_Maj_Flag, 
             data = training[is.na(training$DISTANCE) != T,],
            method = "knn", 
            metric = "ROC",
            trControl = trainControl(method = "cv", 
                                     number = 5,
                                     summaryFunction = multiClassSummary,
                                     classProbs = TRUE),
            tuneGrid = expand.grid(.k = c(3, 5, 7, 9, 11, 15, 21, 25, 31, 41, 51, 75, 101)))
KNN
varImp(KNN)
### can't generate the variable importance.
plot(KNN)
# ROC: 0.8648815
# Accuracy: 0.8178258
# Kappa: 0.4440642
# Sensitivity: 0.9093484
pred <- predict(KNN, 
                testing[is.na(testing$DISTANCE) != T,], 
                type = "prob")
confusionMatrix(pred[, 2] >= 0.5, testing[is.na(testing$DISTANCE) != T,]$churn == "Yes", positive = "TRUE")
# Accuracy: 0.8166
# Kappa: 0.4454
# Sens: 0.5149
library(ROCR)
ROCRpred <- prediction(pred[,2], testing[is.na(testing$DISTANCE) != T,]$churn)
as.numeric(performance(ROCRpred, "auc")@y.values)
# AUC: 0.8657443


############################################################################
############################################################################
## Support Vector Machines
set.seed(80)
# Use the formula interface which will create dummy variables and that seems to be 
# required by SVM.

####
# Need to test whether excluding the cases where distance is null makes any difference.
####
SVM_linear <- train(churn ~ DISTANCE + RECENCY + TENURE + RET_T12 + log(TRANS12X) + log(TRANS24X + 1) + LINES12X  + indseg1 + 
                      sellertype + EPEDN12X + trans_3month + EBUN12X + Customer_Size + SOW + Corp_Maj_Flag, 
                    data = training[is.na(training$DISTANCE) != T,],
                    method = "svmLinear",
                    metric = "ROC",
                    trControl = trainControl(method = "cv", 
                                             number = 5,
                                             summaryFunction = multiClassSummary,
                                             classProbs = TRUE),
                    tuneGrid = expand.grid(.C = c(0.003, 0.005)))
SVM_linear
varImp(SVM_linear)
### Can't generate the variable importance.
plot.svm(SVM_linear)
alphaindex(SVM_linear$finalModel)
coef(SVM_linear$finalModel)
# C: 0.0001
# ROC: 0.8889124
# Accuracy: 0.8363622
# Kappa: 0.5332976
# Sens: 0.8940237

# C: 0.0002
# ROC: 0.8900054
# Sens: 0.9198379
# 
# C: 0.0005
# ROC: 0.8926540
# Sens: 0.9234348

# New data
# C: 0.001
# ROC: 0.8941213
# Sens: 0.9264734

# C: 0.002
# ROC: 0.8930657
# Accuracy: 0.8424252
# Kappa: 0.5178491
# Sens: 0.9262858

# new data    best
# C: 0.003
# ROC: 0.8942656
# Accuracy: 0.8443978
# Kappa: 0.5230891
# Sens: 0.9282178

# new data
# C: 0.004
# ROC: 0.8942407
# Accuracy: 0.8442673
# Kappa: 0.5222802
# Sens: 0.9284616

# new data
# C = 0.005            
# ROC: 0.8941937
# Accuracy: 0.8441367
# Kappa: 0.5218228
# Sens 0.9284241

# New data
# C: 0.008
# ROC: 0.8940964
# Accuracy: 0.8439482
# Kappa: 0.5206024
# Sens: 0.9288180

# new data
# C = 0.01
# ROC: 0.8940795
# Accuracy: 0.8439627
# Kappa: 0.5206120
# Sens: 0.9288555

# C = 0.05
# ROC: 0.8927389
# Accuracy: 0.8418014
# Kappa: 0.5135234
# Sens: 0.9278051

# C = 0.1
# ROC: 0.8928954
# Accuracy: 0.8421207
# Kappa: 0.5163433
# Sens: 0.9265109

# C: 0.5
# ROC: 0.8850622
# Accuracy: 0.8368700
# Kappa: 0.4993981
# Sens: 0.9269048
# Evaluate the model on test set.
pred <- predict(SVM_linear, newdata = testing, type = "prob")
confusionMatrix(pred[, 2] >= 0.5, testing[is.na(testing$DISTANCE) != T, ]$churn == "Yes", positive = "TRUE")
# Accuracy: 0.8441
# Kappa: 0.5231
# Sens: 0.5601
library(ROCR)
ROCRpred <- prediction(pred[,2], testing[is.na(testing$DISTANCE) != T, ]$churn)
as.numeric(performance(ROCRpred, "auc")@y.values)
# AUC: 0.8943726
set.seed(80)
SVM_RBF <- train(churn ~ DISTANCE + RECENCY + TENURE + RET_T12 + log(TRANS12X) + log(TRANS24X + 1) + LINES12X  + indseg1 + 
                   sellertype + EPEDN12X + trans_3month + EBUN12X + Customer_Size + SOW + Corp_Maj_Flag, 
                 data = training[is.na(training$DISTANCE) != T,],
                    method = "svmRadial", 
                    metric = "ROC",
                    trControl = trainControl(method = "cv", 
                                             number = 5,
                                             summaryFunction = multiClassSummary,
                                             classProbs = TRUE),
                    tuneGrid = expand.grid(.C = rep(0.003, 2),
                                           .sigma = c(0.001, 0.01)))
# sigma is a smoothing parameter


############################################################################
############################################################################
## Gradient boosting machine
## 3 tuning parameters: # of iterations, complexity of the tree, learning rate
## Note: can try to reduce # of trees, lower shrinkage. 
## try to reduce the increments of n.trees -- 20
set.seed(80)
## automatic feature selection
gbm_RFE <- train(training[complete.cases(training),c(1:198, 201, 203)],
                 training[complete.cases(training), 199],
             method = "gbm",
             metric = "ROC",
             trControl = trainControl(method = "cv", 
                                      number = 5,
                                      summaryFunction = multiClassSummary,
                                      classProbs = TRUE),
             tuneGrid = expand.grid(interaction.depth = c(5, 7, 9),
                                    n.trees = (1:30)*20,
                                    shrinkage = c(0.07, 0.05, 0.1),
                                    n.minobsinnode = 20))
## Save output to txt file.
sink("gbm_RFE.txt")
print(gbm_RFE)
sink()
unlink("gbm_RFE.txt")
plot(varImp(gbm_RFE), top = 20)
ggplot(gbm_RFE)
# interaction.depth = c(5, 7, 9),
# n.trees = (1:30)*20,
# shrinkage = c(0.07, 0.05, 0.1),
# n.minobsinnode = 20))
# BEST ntrees = 280, depth = 9, shrinkage = 0.05
# ROC: 0.8992695  
# Accuracy: 0.8486762  
# Kappa: 0.5451691  
# Sens: 0.9192503   
pred <- predict(gbm_RFE, newdata = testing[complete.cases(testing),c(1:198, 201, 203)], 
                type = "prob")
confusionMatrix(pred[, 2] >= 0.5, testing[complete.cases(testing), ]$churn == "Yes", positive = "TRUE")
## Evaluate the model on testing data.
# Accuracy: 0.8488
# Kappa: 0.5466
# Sens: 0.6068
library(ROCR)
ROCRpred <- prediction(pred[,2], testing[complete.cases(testing), ]$churn)
as.numeric(performance(ROCRpred, "auc")@y.values)
# AUC value = 0.9002506



gbm <- train(churn ~ DISTANCE + RECENCY + TENURE + RET_T12 + log(TRANS12X) + log(TRANS24X + 1) + LINES12X  + indseg1 + 
                      sellertype + EPEDN12X + trans_3month + EBUN12X + Customer_Size + SOW + Corp_Maj_Flag, 
                    data = training[is.na(training$DISTANCE) != T,],
                    method = "gbm",
                    metric = "ROC",
                    trControl = trainControl(method = "cv", 
                                             number = 5,
                                             summaryFunction = multiClassSummary,
                                             classProbs = TRUE),
                    tuneGrid = expand.grid(interaction.depth = c(5, 7, 9),
                                           n.trees = (1:25)*20,
                                           shrinkage = c(0.07, 0.05, 0.1),
                                           n.minobsinnode = 20))
gbm
gbm$finalModel
plot(gbm)
varImp(gbm)
# Among 1, 5, 9 Best depth = 5   
# ROC: 0.8985801
# Accuracy: 0.8474002
# Kappa: 0.5461822
# Sens: 0.9183518
# Among 2, 3, 4,best depth =  2
# ROC: 0.8985513
# Accuracy: 0.8473711
# Kappa: 0.5455359
# Sens: 0.9188019
# Among 6, 7, 8 best depth = 7
# ROC: 0.8986632
# Accuracy: 0.8470376
# Kappa: 0.5454162
# Sens: 0.9178265

# Among depth 5, 7, 9, ntress = (1:20)*25, shrinkage = 0.1, 0.05
# best shrinkage = 0.05
# ROC: 0.8987033
# Accuracy: 0.8470956
# Kappa: 0.5449344
# Sens: 0.9184455

# Among depth 5, 7, 9, ntress = (1:20)*25, shrinkage = c(0.07, 0.05, 0.1)  BEST
# n.trees = 200
# interaction.depth = 9
# shrinkage = 0.05
# ROC: 0.8987161
# Accuracy: 0.8470231
# Kappa: 0.5467440
# Sens: 0.9166074


pred <- predict(gbm, newdata = testing, type = "prob")
confusionMatrix(pred[, 2] >= 0.5, testing[is.na(testing$DISTANCE) != T, ]$churn == "Yes", positive = "TRUE")
# Accuracy: 0.847
# Kappa: 0.5465
# Sens: 0.6060
library(ROCR)
ROCRpred <- prediction(pred[,2], testing[is.na(testing$DISTANCE) != T, ]$churn)
as.numeric(performance(ROCRpred, "auc")@y.values)
# AUC: 0.899218

############################################################################
############################################################################
## Ridge/Lasso regression -- GLMNET
library(caret)
## Note that x for train needs to be a matrix, otherwise code won't run. 
set.seed(80)
### Automatic feature selection
glmnet_RFE <- train(data.matrix(training[complete.cases(training),c(1:198, 201)]),
                training[complete.cases(training), 199],
                    method = "glmnet",
                    metric = "ROC",
                    trControl = trainControl(method = "cv", 
                                             number = 5,
                                             summaryFunction = multiClassSummary,
                                             classProbs = TRUE),
                    tuneGrid = expand.grid(.alpha = seq(0, 1, 0.05),
                                           .lambda = c(1e-05, 2e-05, 3e-05, 4e-05, 5e-05)))
glmnet
varImp(glmnet)
plot(glmnet)
plot(glmnet, metric = "Sensitivity")

# alpha = seq(0, 1, 0.05),
# lambda = c(1e-05, 2e-05, 3e-05, 4e-05, 5e-05)
# Best alpha = 1 and lambda = 2e-05
# ROC: 0.8921677
# Accuracy: 0.8430034
# Kappa: 0.5300179
# Sens: 0.9140406

set.seed(80)
glmnet <- train(churn ~ DISTANCE + RECENCY + TENURE + RET_T12 + log(TRANS12X) + log(TRANS24X + 1) + LINES12X  + indseg1 + 
                  sellertype + EPEDN12X + trans_3month + EBUN12X + Customer_Size + SOW + Corp_Maj_Flag, 
                data = training[is.na(training$DISTANCE) != T,],
                    method = "glmnet",
                    metric = "ROC",
                    trControl = trainControl(method = "cv", 
                                             number = 5,
                                             summaryFunction = multiClassSummary,
                                             classProbs = TRUE),
                    tuneGrid = expand.grid(.alpha = seq(0, 1, 0.05),
                                           .lambda = c(0.0001, 0.0005, 0.001, 0.005)))
### manual feature selection
# alpha = c(0, 0.05, 1),
# lambda = c(0.01, 0.1)
# Best alpha = 0.05 and lambda = 0.01
# ROC: 0.8934721
# Accuracy: 0.8440207
# Kappa: 0.5206648
# Sens:0.9289868

# alpha = c(0, 0.05, 0.5, 1),
# lambda = c(0.01, 0.005, 0.001)
# best alpha = 0.05, lamda = 0.001
# ROC: 0.8941201
# Accuracy: 0.8445283
# Kappa: 0.5273298
# Sens: 0.9251604

# alpha = c(0, 0.05, 0.01),
# lambda = c(0.005, 0.001, 0.0005)
# Best alpha = 0.05 and lambda = 0.0005
# ROC: 0.8943079
# Accuracy: 0.8446734
# Kappa: 0.5281346
# Sens: 0.9249541

# alpha = c(0.05, 0.025),   
# lambda = c(0.001, 0.0005, 0.0001)
# best alpha = 0.05 and lambda = 1e-04   
# ROC: 0.8943337
# Accuracy: 0.8446734
# Kappa: 0.5281125
# Sens: 0.9249729

# alpha = seq(0, 1, 0.05)          BEST
# lambda = c(0.0001, 0.0005, 0.001, 0.005)
# best alpha = 1 and lambda = 1e-04
# ROC: 0.8954814
# Accuracy: 0.8448620
# Kappa: 0.5295136
# Sens: 0.9244102

pred <- predict(glmnet, newdata = testing, type = "prob")
confusionMatrix(pred[, 2] >= 0.5, testing[is.na(testing$DISTANCE) != T, ]$churn == "Yes", positive = "TRUE")
# Accuracy: 0.8453
# Kappa: 0.5313
# Sens: 0.5750
library(ROCR)
ROCRpred <- prediction(pred[,2], testing[is.na(testing$DISTANCE) != T, ]$churn)
as.numeric(performance(ROCRpred, "auc")@y.values)
# AUC: 0.8962534

############################################################################
############################################################################
## Neural Network
## single hidden layer.
## parameters to tune: 
## size: number of units in the hidden layer
## decay: parameter for weight decay
set.seed(80)
NN <- train(churn ~ DISTANCE + RECENCY + TENURE + RET_T12 + log(TRANS12X) + log(TRANS24X + 1) + LINES12X  + indseg1 + 
                  sellertype + EPEDN12X + trans_3month + EBUN12X + Customer_Size + SOW + Corp_Maj_Flag, 
                data = training[is.na(training$DISTANCE) != T,],
                method = "nnet",
                metric = "ROC",
            maxit = 500,
                trControl = trainControl(method = "cv", 
                                         number = 5,
                                         summaryFunction = multiClassSummary,
                                         classProbs = TRUE),
                tuneGrid = expand.grid(.size = c(6, 9, 12),
                                       .decay = c(4, 10, 13, 15)))
NN
plot(NN)
varImp(NN)
# maxit = 100
# size = c(1, 5, 10),
# decay = c(0, 0.001, 0.1)
# best size = 1, decay = 0.1
# ROC: 0.8952092
# Accuracy: 0.8447169
# Kappa: 0.5502434
# Sens: 0.9059723

# size = c(1, 2, 3),
# decay = c(0.001, 0.5, 1)
# Best size = 1 and decay = 1
# ROC: 0.8959476
# Accuracy: 0.8449345
# Kappa: 0.5441154
# Sens: 0.9121432

# size = c(1, 2, 3),
# decay = c(1, 5, 10)
# best size = 1, decay = 5
# ROC: 0.8961101
# Accuracy: 0.8455001
# Kappa: 0.5474254
# Sens: 0.9110554

# size = c(1, 5, 10),
# decay = c(2, 3, 4, 5)
# best size = 1, decay = 4
# ROC: 0.8960830
# Accuracy: 0.8451955
# Kappa: 0.5461504
# Sens: 0.9111866

# size = c(1, 4, 6, 7),
# decay = c(2, 3, 4, 5, 6, 7, 8)
# Best size = 6, decay = 5
# ROC: 0.8966811
# Accuracy: 0.8464429
# Kappa:0.5434766
# Sens: 0.9175827

# size = c(1, 6, 8, 9, 10),
# decay = c(3, 4, 5, 6, 7, 8, 9, 10)))
# Best size = 6 decay = 7
# ROC: 0.8963273
# Accuracy: 0.8457467
# Kappa: 0.5436204
# Sens: 0.9151818

# maxit = 300
# size = c(4, 6, 8, 9, 10),  
# decay = c(4, 5, 6, 7, 8, 9, 10)
# best size = 9, decay = 4              
# ROC: 0.8984885
# Accuracy: 0.8472841
# Kappa: 0.5473744
# Sens: 0.9169075

# maxit = 500              BEST
# size = c(6, 9, 12),
# decay = c(4, 10, 13, 15)
# Best size = 12 decay = 4
# ROC: 0.8985449
# Accuracy: 0.8477918
# Kappa: 0.5485173
# Sens: 0.9175640


pred <- predict(NN, newdata = testing, type = "prob")
confusionMatrix(pred[, 2] >= 0.5, testing[is.na(testing$DISTANCE) != T, ]$churn == "Yes", positive = "TRUE")
# Accuracy: 0.8473
# Kappa: 0.548
# Sens: 0.6110
library(ROCR)
ROCRpred <- prediction(pred[,2], testing[is.na(testing$DISTANCE) != T, ]$churn)
as.numeric(performance(ROCRpred, "auc")@y.values)
# AUC: 0.8992348

############################################################################
############################################################################
# COMPARING THE PERFORMANCES OF DIFFERENT MODELS
compare_perf <- resamples(list(LogReg = logReg_caret, randomForest = RF, randomForest_RFE = RF_RFE_1000, K_Nearest = KNN, SVM = SVM_linear, GBM = gbm, GLMNET = glmnet, NeuralNetwork = NN))
summary(compare_perf)
splom(compare_perf, metric = "ROC")
parallelplot(compare_perf, metric = "ROC")
dotplot(compare_perf)
dotplot(compare_perf, metric = "ROC")
dotplot(compare_perf, metric = "Accuracy")
dotplot(compare_perf, metric = "Sensitivity")
rocDiffs <- diff(compare_perf, metric = "ROC")
summary(rocDiffs)
dotplot(rocDiffs, metric = "ROC")
bwplot(compare_perf, metric = "ROC")
