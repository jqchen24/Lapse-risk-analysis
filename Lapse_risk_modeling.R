##########################################
# Load the dataset and remove account # #
##########################################
accounts <- read.csv('accounts_churn_contact_flag.csv', stringsAsFactors = F)
str(accounts)
accounts$ ï..account <- NULL
accounts$contract_group <- as.factor(accounts$contract_group)
accounts$churn <- as.factor(accounts$churn)
accounts$indseg1 <- as.factor(accounts$indseg1)

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

#######################################################
# Feature engineering
#######################################################
library(dplyr)
accounts <- mutate(accounts, discount = (WA_S12X - (SALES12X - FINDS12X))/WA_S12X)
ggplot(accounts, aes(discount)) + geom_bar()
accounts <- mutate(accounts, sellertype = CSG %/% 10000)
accounts$sellertype <- as.factor(accounts$sellertype)

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
RF <- randomForest(churn ~ CONTACTS + TENURE + TRANS12X + LINES12X  + indseg1 + mrospend 
                   + contract_group + sellertype, data = accounts_train, do.trace = T)
RF_predict <- predict(RF, newdata = accounts_test)
library(caret)
confusionMatrix(RF_predict, accounts_test$churn, positive = "1")
# Accuracy is 83.85%
# Sensitivity is 59.28%.
varImp(RF)
varImpPlot(RF)


## Random forest model using caret package
# create a stratified random sample of the data into training and test sets:
library(caret)
set.seed(998)
inTraining <- createDataPartition(accounts$churn, p = 0.75, list = F)
training <- accounts[inTraining,]
testing <- accounts[-inTraining,]
table(training$churn)/nrow(training)
table(testing$churn)/nrow(testing)

fitControl <- trainControl(method = "cv", number = 10)
set.seed(80)
RF <- train(churn ~ CONTACTS + TENURE + TRANS12X + LINES12X  + indseg1 + mrospend + 
              contract_group + sellertype, data = training, method = "rf", metric = "Kappa",
            trControl = fitControl)
## train only tune mtry for random forest.

############################################################################
############################################################################
## Logistic regression model using caret
# Note that to calculate the ROC curve, we need the model to predict the
# class probabilities. The classProbs option will also do this. However, if we set
# classProbs = TRUE, we won't be able to calculate accuracy later, AUC will be calculated
# instead. 
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, summaryFunction = twoClassSummary,
                           classProbs = TRUE)
set.seed(80)
# When class probabilities are requested, train puts them into a data frame with a column 
# for each class. If the factor levels are not valid variable names, they are automatically
# changed (e.g. "0" becomes "X0").
levels(training$churn) <- c("No", "Yes")
logReg_caret <- train(churn ~ DISTANCE + CREDIT + CONTACTS + RECENCY + TENURE + log(TRANS12X) + LINES12X  + indseg1 + mrospend + 
                        contract_group + sellertype, data = training, method = "glm", 
                      trControl = fitControl, family = "binomial")
logReg_caret
# ROC = 0.8910434
# Sensitivity = 0.9255932
summary(logReg_caret)
# Note that for predict.train under caret, type argument can only be "raw" or "prob"
# Also note that pred actually is a two column data frame.
pred <- predict(logReg_caret, newdata = testing, type = "prob")
confusionMatrix(pred[, 2] >= 0.5, testing$churn == 1, positive = "TRUE")
## Need to exclude the missing values in distance, otherwise confusionMatrix won't work.
confusionMatrix(pred[, 2] >= 0.5, testing[is.na(testing$DISTANCE) != T,]$churn == 1, positive = "TRUE")
## Accuracy = 84.01%
## Kappa = 51.07%
## Sensitivity = 55.09%
library(ROCR)
ROCRpred <- prediction(pred[,2], testing$churn)
ROCRpred <- prediction(pred[,2], testing[is.na(testing$DISTANCE) != T,]$churn)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf <- performance(ROCRpred, "tpr", "fpr")
plot(perf)
# AUC value = 0.891354