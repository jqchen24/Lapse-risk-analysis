# install.packages(c("caret", "ggplot2", "pROC", "ROCR", "e1071", "randomForest", "dplyr"))
library(caret)
library(dplyr)
library(ggplot2)
library(ROCR)
library(foreign)

######## Need to deal with sellertype next time building the model. 
######## Look up CSG in future alignment.
######## Remove industry "other" from data.

#### Need to re-train RF model because in training data discount has Inf values and missing values.

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
### this hasn't been saved. So INVSOLFLG in accounts is still integer.
accounts$INVSOLFLG <- as.factor(accounts$INVSOLFLG)
accounts$Customer_Size <- as.factor(accounts$Customer_Size)
### Newly added, haven't saved. So mro_decile in accounts is still integer.
accounts$mro_decile <- as.factor(accounts$mro_decile)
accounts$DUNSSBUS <- NULL
accounts$DUNSPUBL <- NULL
accounts$contract_group <- NULL
accounts$sales_current <- accounts$sales_201508
accounts$sales_201508 <- NULL
accounts$sales_201509 <- NULL
accounts$sales_201510 <- NULL
accounts$sales_201511 <- NULL


# Create some scatter plots
library(ggplot2)

## MISSING VALUES WITH DISTANCE and contact count
summary(accounts$DISTANCE)
summary(accounts$contact_count)
ggplot(accounts, aes(x = DISTANCE)) + geom_histogram(binwidth = 50)
accounts[is.na(accounts$DISTANCE), ]$DISTANCE <- median(accounts$DISTANCE, na.rm = T)
accounts[is.na(accounts$contact_count), ]$contact_count <- 0

# Create some visuals
library(ggplot2)
ggplot(accounts, aes(x = TRANS12X, y = churn)) + geom_point()
ggplot(accounts, aes(x = TENURE, y = churn)) + geom_point()
ggplot(accounts, aes(x = RECENCY, y = churn)) + geom_point()
ggplot(accounts, aes(x = TRANS12X, y = TENURE, color = churn)) + geom_point() + theme(legend.position = "bottom")
ggplot(accounts, aes(x = TRANS12X, y = RECENCY, color = churn)) + geom_point()
ggplot(accounts, aes(x = LINES12X, y = RECENCY, color = churn)) + geom_point()
ggplot(accounts, aes(RECENCY)) + geom_bar()
ggplot(accounts, aes(TENURE)) + geom_bar()
ggplot(accounts, aes(x = TENURE)) + geom_histogram(aes(fill = churn))
ggplot(accounts, aes(x = RECENCY)) + geom_histogram(aes(fill = churn))
ggplot(accounts, aes(x = TRANS12X)) + geom_histogram(aes(fill = churn)) + xlim(0, 500)
ggplot(accounts, aes(x = LINES12X)) + geom_histogram(aes(fill = churn))
ggplot(accounts, aes(x = RECENCY)) + geom_histogram(aes(fill = churn))
ggplot(accounts, aes(x = DISTANCE)) + geom_histogram(aes(fill = churn))
ggplot(accounts, aes(x = RECENCY)) + geom_bar(aes(fill = churn))
ggplot(accounts, aes(x = LINES12X)) + geom_bar(aes(fill = churn))
ggplot(accounts, aes(x = TENURE)) + geom_bar(aes(fill = churn))
ggplot(accounts, aes(x = TRANS12X)) + geom_bar(aes(fill = churn)) + xlim(0, 200)
ggplot(accounts, aes(x = abs(SOW))) + geom_histogram(aes(fill = churn)) + xlab("SOW")
ggplot(accounts, aes(x = RECENCY, color = churn, fill = churn)) + geom_density()
ggplot(accounts, aes(x = TENURE, color = churn, fill = churn)) + geom_density()
ggplot(accounts, aes(x = LINES12X, color = churn)) + geom_density()
ggplot(accounts, aes(x = TRANS12X, color = churn)) + geom_density()
## two categorical variables
accounts <- mutate(accounts, industry = ifelse(indseg1 == 0, "International/export",
                                                           ifelse(indseg1 == 1, "Government",
                                                                  ifelse(indseg1 == 2, "Healthcare",
                                                                         ifelse(indseg1 == 3, "Heavy MFG",
                                                                                ifelse(indseg1 == 4, "Light MFG",
                                                                                       ifelse(indseg1 == 5, "Hospitality",
                                                                                              ifelse(indseg1 == 6, "Commercial Services",
                                                                                                     ifelse(indseg1 == 7, "Retail",
                                                                                                            ifelse(indseg1 == 8, "Wholesale",
                                                                                                                   ifelse(indseg1 == 9, "Transportation",
                                                                                                                          ifelse(indseg1 == 10, "Contractors",
                                                                                                                                 ifelse(indseg1 == 11, "Property Mgt",
                                                                                                                                        ifelse(indseg1 == 12, "Natural resources", 
                                                                                                                                               ifelse(indseg1 == 13, "Resellers", "Other")))))))))))))))
accounts <- mutate(accounts, coverage = ifelse(sellertype == 83, "TSR", 
                                             ifelse(sellertype %in% c(84, 88), "AM",
                                                    ifelse(sellertype == 86, "TSA", 
                                                           ifelse(sellertype == 89, "Government ARM", "Not covered")))))
ggplot(accounts, aes(x = industry)) + geom_bar(aes(fill = churn)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
accounts <- mutate(accounts, contract_type = ifelse(Corp_Maj_Flag == "C", "National Accts",
                                                                ifelse(Corp_Maj_Flag == "G", "Government GPO",
                                                                       ifelse(Corp_Maj_Flag == "H", "Healthcare GPO",
                                                                              ifelse(Corp_Maj_Flag == "M", "National Accts",
                                                                                     ifelse(Corp_Maj_Flag == "N", "Local Contracts",
                                                                                            ifelse(Corp_Maj_Flag == "P", "Commercial GPO",
                                                                                                   ifelse(Corp_Maj_Flag == "S", "Select Advantage",
                                                                                                          ifelse(Corp_Maj_Flag == "X", "Non-contract Local", "")))))))))
accounts$contract_type <- as.factor(accounts$contract_type)
ggplot(accounts, aes(x = CONTRACT_FLAG)) + geom_bar(aes(fill = churn))
ggplot(accounts, aes(x = contract_type)) + geom_bar(aes(fill = churn)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(accounts, aes(x = Customer_Size)) + geom_bar(aes(fill = churn))
ggplot(accounts, aes(x = Corp_Maj_Flag)) + geom_bar(aes(fill = churn))
ggplot(accounts, aes(x = sellertype)) + geom_bar(aes(fill = churn))
ggplot(accounts, aes(x = INVSOLFLG)) + geom_bar(aes(fill = churn))
ggplot(accounts, aes(x = multisite)) + geom_bar(aes(fill = churn))
ggplot(accounts, aes(x = mro_decile)) + geom_bar(aes(fill = churn))
ggplot(accounts, aes(x = SOW)) + geom_bar(aes(fill = churn))
ggplot(accounts, aes(x = mro_decile, fill = churn)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = TENURE, fill = churn)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = industry, fill = churn)) + geom_bar(position = "fill") + ylab("% of accounts") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_x_discrete(limits=c("International/export","Government","Healthcare", "Heavy MFG", "Light MFG", "Hospitality", "Commercial Services", "Retail", "Wholesale", "Transportation", "Contractors", "Property Mgt", "Natural resources", "Resellers"))
ggplot(accounts, aes(x = CONTRACT_FLAG, fill = churn)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = contract_type, fill = churn)) + geom_bar(position = "fill") + ylab("% of accounts")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(accounts, aes(x = INVSOLFLG, fill = churn)) + geom_bar(position = "fill") + ylab("% of accounts") + xlab("KeepStock")
ggplot(accounts, aes(x = sellertype, fill = churn)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = LINES12X, fill = churn)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = RECENCY, fill = churn)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = SOW, fill = churn)) + geom_histogram(position = "fill") + ylab("% of accounts") + xlim(c(0, max(accounts$SOW)))
accounts <- mutate(accounts, SOW_range = ifelse(SOW < 0, "0",
                                                ifelse(SOW < 0.2, "20%",
                                                       ifelse(SOW < 0.4, "40%",
                                                              ifelse(SOW < 0.6, "60%",
                                                                     ifelse(SOW < 0.8, "80%", "100%"))))))
accounts$SOW_range <- as.factor(accounts$SOW_range)
ggplot(accounts, aes(x = SOW_range, fill = churn)) + geom_bar(position = "fill") + ylab("% of accounts") + scale_x_discrete(limits=c("0","20%","40%", "60%", "80%", "100%"))
## using different colors.
ggplot(accounts, aes(discount)) + geom_histogram() + xlim(c(0, 1))
ggplot(accounts, aes(x = discount, fill = churn)) + geom_histogram(position = "fill") + ylab("% of accounts") + xlim(c(0, 1))
ggplot(accounts, aes(x = coverage, fill = churn)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = TRANS12X, fill = churn)) + geom_bar(position = "fill") + ylab("% of accounts") + xlim(c(0,100))
ggplot(accounts, aes(x = GP24X, fill = churn)) + geom_bar(position = "fill") + ylab("% of accounts") + xlim(c(0, 100))
accounts <- mutate(accounts, distance_range = ifelse(DISTANCE < 2, "< 2",
                                                     ifelse(DISTANCE < 5, "< 5",
                                                            ifelse(DISTANCE < 10, "< 10",
                                                                   ifelse(DISTANCE < 15, "< 15", "> 15")))))
accounts$distance_range <- as.factor(accounts$distance_range)
ggplot(accounts, aes(x = distance_range, fill = churn)) + geom_bar(position = "fill") + ylab("% of accounts") + scale_x_discrete(limits = c("< 2", "< 5", "< 10", "< 15", "> 15"))
ggplot(accounts, aes(x = CONTACTS, fill = churn)) + geom_bar(position = "fill") + ylab("% of accounts") + xlim(c(0, 100))

# Some EDA
names(accounts)
table(accounts$Customer_Size)
table(accounts$CONTRACT_FLAG)
table(accounts$mro_decile)
table(accounts$indseg0)
### return the variables that have missing values
sort(colSums(is.na(accounts)), decreasing = T)


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

## retrieve the factor variables.
colnames(training[, lapply(training, is.factor) == T])
colnames(testing_201605[, lapply(testing_201605, is.factor) == T])


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
## make sure the levels in training set and prediction set are the same.
levels(accounts$sellertype) <- c(levels(accounts$sellertype), "20", "73", "74", "75", "76",
                                 "77", "78")
levels(training$sellertype) <- c(levels(accounts$sellertype), "20", "73", "74", "75", "76",
                                 "77", "78")
levels(testing$sellertype) <- c(levels(accounts$sellertype), "20", "73", "74", "75", "76",
                                "77", "78")
levels(training$dunsstat) <- levels(accounts$dunsstat)
levels(testing$dunsstat) <- levels(accounts$dunsstat)
levels(training$Customer_Size) <- levels(accounts$Customer_Size)
levels(testing$Customer_Size) <- levels(accounts$Customer_Size)
levels(training$Corp_Maj_Flag) <- levels(accounts$Corp_Maj_Flag)
levels(testing$Corp_Maj_Flag) <- levels(accounts$Corp_Maj_Flag)
table(training$churn)/nrow(training)
table(testing$churn)/nrow(testing)


#######################################################
## Principal components analysis (PCA)
#######################################################
pca <- prcomp(training, center = T, scale = T)

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

## Automatic feature selection by RF.
set.seed(80)
RF_RFE_1000 <- train(training[, c(1:197, 199, 201, 203)],
                training[, 198],
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
getTrainPerf(RF_RFE_1000)
ggplot(varImp(RF_RFE_1000), top = 20)
ggplot(RF_RFE_1000)
plot(varImp(RF_RFE_1000), top = 20)
# mtry = c(2, 3, 4) ntree = 1000
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
# ROC: 0.8972970
# Accuracy: 0.8468511
# Kappa: 0.5336724
# Sens: 0.9230694

# picked
# mtry = c(14, 16, 18) ntree = 1000 replace NA in distance as median, recode missing value
# in contact_count to 0.
# best mtry = 16
# ROC: 0.8971984
# Accuracy: 0.8457963
# Kappa: 0.5366113
# Sens: 0.9219089
pred_train <- predict(RF_RFE_1000, newdata = training[, c(1:197, 199, 201, 203)], type = "prob")
confusionMatrix(pred_train[, 2] >= 0.5, training$churn == "Yes", positive = "TRUE")
###### 100% for all metrics!!!!!!!! Why?
pred <- predict(RF_RFE_1000, newdata = testing[,c(1:197, 199, 201, 203)], 
                type = "prob")
confusionMatrix(pred[, 2] >= 0.5, testing$churn == "Yes", positive = "TRUE")
## Evaluate the model on testing data.
# Accuracy: 0.8466 
# Kappa: 0.5403
# Sens: 0.5923
testing <- mutate(testing, lapse_score = pred[,2 ])
write.csv(testing, "testing_accounts_scored.csv", row.names = F)

library(ROCR)
ROCRpred <- prediction(pred[,2], testing$churn)
as.numeric(performance(ROCRpred, "auc")@y.values)
# AUC value = 0.8983736
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
cutoffs
head(subset(cutoffs, fpr < 0.2))
plot(performance(ROCRpred, measure="lift", x.measure="rpp"), colorize=TRUE)
plot(performance(ROCRpred, measure="sens", x.measure="spec"), colorize=TRUE)


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
# ROC = 0.8954986
# Accuracy = 0.8447858
# Sensitivity = 0.9236085
# Kappa = 0.5307743

summary(logReg_caret)
# Note that for predict.train under caret, type argument can only be "raw" or "prob"
# Also note that pred actually is a two column data frame.
pred <- predict(logReg_caret, newdata = testing, type = "prob")
confusionMatrix(pred[, 2] >= 0.5, testing$churn == "Yes", positive = "TRUE")
## Accuracy = 0.8455
## Kappa = 0.5333
## Sensitivity = 0.5794
library(ROCR)
ROCRpred <- prediction(pred[,2], testing$churn)
as.numeric(performance(ROCRpred, "auc")@y.values)
# AUC value = 0.8964048
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
### Haven't been updated after taking care of the missing values.
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

SVM_linear <- train(churn ~ DISTANCE + RECENCY + TENURE + RET_T12 + log(TRANS12X) + log(TRANS24X + 1) + LINES12X  + indseg1 + 
                      sellertype + EPEDN12X + trans_3month + EBUN12X + Customer_Size + SOW + Corp_Maj_Flag, 
                    data = training,
                    method = "svmLinear",
                    metric = "ROC",
                    trControl = trainControl(method = "cv", 
                                             number = 5,
                                             summaryFunction = multiClassSummary,
                                             classProbs = TRUE),
                    tuneGrid = expand.grid(.C = c(0.001, 0.003, 0.005, 0.0005)))
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

## after taking care of the missing values
# C: 0.003
# ROC: 0.8936316
# Accuracy: 0.8441650
# Kappa: 0.5231861
# Sens: 0.9279044

# Evaluate the model on test set.
pred <- predict(SVM_linear, newdata = testing, type = "prob")
confusionMatrix(pred[, 2] >= 0.5, testing$churn == "Yes", positive = "TRUE")
# Accuracy: 0.8439
# Kappa: 0.5227
# Sens: 0.5595
library(ROCR)
ROCRpred <- prediction(pred[,2], testing$churn)
as.numeric(performance(ROCRpred, "auc")@y.values)
# AUC: 0.8942408
set.seed(80)
SVM_RBF <- train(churn ~ DISTANCE + RECENCY + TENURE + RET_T12 + log(TRANS12X) + log(TRANS24X + 1) + LINES12X  + indseg1 + 
                   sellertype + EPEDN12X + trans_3month + EBUN12X + Customer_Size + SOW + Corp_Maj_Flag, 
                 data = training,
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
gbm_RFE_NA <- train(training[, c(1:197, 199, 201, 203)],
                    training[, 198],
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
gbm_RFE_NA
ggplot(gbm_RFE_NA) + theme(legend.position = "bottom")
ggplot(varImp(gbm_RFE_NA), top = 20)
sink("gbm_RFE_NA.txt")
print(gbm_RFE_NA)
sink()
unlink("gbm_RFE_NA.txt")
# interaction.depth = c(5, 7, 9),
# n.trees = (1:30)*20,
# shrinkage = c(0.07, 0.05, 0.1)
# Best ntrees = 180 interaction.depth = 9, shrinkage = 0.05
# ROC: 0.8991428  
# Accuracy: 0.8477596  
# Kappa: 0.5493070  
# Sens: 0.9172394 

## after the missing values in distance and contact_count have been taken care of,
# best ntrees = 340, interaction depth = 7, shrinkage = 0.05
# ROC: 0.8991958  
# Accuracy: 0.8477163  
# Kappa: 0.5479150  
# Sens: 0.9183227  


pred <- predict(gbm_RFE_NA, newdata = testing[, c(1:197, 199, 201, 203)], 
                type = "prob")
confusionMatrix(pred[, 2] >= 0.5, testing$churn == "Yes", positive = "TRUE")
## Evaluate the model on testing data.
# Accuracy: 0.8478
# Kappa: 0.5495
# Sens: 0.6118
library(ROCR)
ROCRpred <- prediction(pred[,2], testing$churn)
as.numeric(performance(ROCRpred, "auc")@y.values)
# AUC value = 0.9000721

### Exclude missing values
set.seed(80)
gbm_RFE <- train(training[complete.cases(training),c(1:197, 200, 202, 203)],
                 training[complete.cases(training), 198],
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
pred <- predict(gbm_RFE, newdata = testing[complete.cases(testing),c(1:197, 200, 202, 203)], 
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

## Manually select features.
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
glmnet_RFE <- train(data.matrix(training[, c(1:197, 199, 201, 203)]),
                training$churn,
                    method = "glmnet",
                    metric = "ROC",
                    trControl = trainControl(method = "cv", 
                                             number = 5,
                                             summaryFunction = multiClassSummary,
                                             classProbs = TRUE),
                    tuneGrid = expand.grid(.alpha = seq(0, 1, 0.05),
                                           .lambda = c(1e-05, 2e-05, 4e-05, 1e-06, 3e-06, 5e-06, 1e-04)))
glmnet_RFE
ggplot(glmnet_RFE)
ggplot(varImp(glmnet_RFE), top = 20)
ggplot(glmnet_RFE, metric = "ROC") + theme(legend.position = "bottom")
ggplot(glmnet_RFE, metric = "Accuracy")
ggplot(glmnet_RFE, metric = "Sensitivity")

# alpha = seq(0, 1, 0.05),
# lambda = c(1e-05, 2e-05, 3e-05, 4e-05, 5e-05)
# Best alpha = 1 and lambda = 2e-05
# ROC: 0.8921677
# Accuracy: 0.8430034
# Kappa: 0.5300179
# Sens: 0.9140406

## after taking care of the missing values
# alpha = seq(0, 1, 0.05),
# lambda = c(1e-05, 2e-05, 4e-05, 1e-06, 3e-06, 5e-06, 1e-04)
# best alpha = 1 and lambda = 1e-05
# ROC: 0.8916793
# Accuracy: 0.8409459
# Kappa: 0.5309761
# Sens: 0.9112626
## no difference among 1e-06, 3e-06, 5e-06, 1e-05
 

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
confusionMatrix(pred[, 2] >= 0.5, testing$churn == "Yes", positive = "TRUE")
# Accuracy: 0.8451
# Kappa: 0.5311
# Sens: 0.5752
library(ROCR)
ROCRpred <- prediction(pred[,2], testing$churn)
as.numeric(performance(ROCRpred, "auc")@y.values)
# AUC: 0.8961263

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
                data = training,
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

## after taking care of missing values
# maxit = 500              
# size = c(6, 9, 12),
# decay = c(4, 10, 13, 15)
# best size = 9, decay = 4
# ROC: 0.8981794
# Accuracy: 0.8467346
# Kappa: 0.5465576
# Sens: 0.9163242


pred <- predict(NN, newdata = testing, type = "prob")
confusionMatrix(pred[, 2] >= 0.5, testing$churn == "Yes", positive = "TRUE")
# Accuracy: 0.8477
# Kappa: 0.55
# Sens: 0.6139
library(ROCR)
ROCRpred <- prediction(pred[,2], testing$churn)
as.numeric(performance(ROCRpred, "auc")@y.values)
# AUC: 0.8991015

############################################################################
############################################################################
# COMPARING THE PERFORMANCES OF DIFFERENT MODELS
compare_perf <- resamples(list(LogReg = logReg_caret, randomForest = RF, randomForest_RFE = RF_RFE_1000, K_Nearest = KNN, SVM = SVM_linear, GBM = gbm, GBM_RFE_NA = gbm_RFE_NA, GLMNET = glmnet, GLMNET_RFE = glmnet_RFE, NeuralNetwork = NN))
summary(compare_perf)
splom(compare_perf, metric = "ROC")
parallelplot(compare_perf, metric = "ROC")
dotplot(compare_perf)
dotplot(compare_perf, metric = "ROC")
dotplot(compare_perf, metric = "Accuracy")
dotplot(compare_perf, metric = "Sensitivity")
dotplot(compare_perf, metric = "Specificity")
dotplot(compare_perf, metric = "Kappa")
rocDiffs <- diff(compare_perf, metric = "ROC")
summary(rocDiffs)
dotplot(rocDiffs, metric = "ROC")
bwplot(compare_perf, metric = "ROC")



#############################################################################
#############################################################################
## Test data on 201510 data
testing_201510 <- read.csv("test_201510.csv", stringsAsFactors = F)
testing_201510$ï..account <- NULL
testing_201510$CONTRACT_FLAG <- as.factor(testing_201510$CONTRACT_FLAG)
testing_201510$churn <- as.factor(testing_201510$churn)
testing_201510$indseg1 <- as.factor(testing_201510$indseg1)
testing_201510$Corp_Maj_Flag <- as.factor(testing_201510$Corp_Maj_Flag)
testing_201510$dunsstat <- as.factor(testing_201510$dunsstat)
testing_201510$Customer_Size <- as.factor(testing_201510$Customer_Size)
testing_201510$DUNSSBUS <- NULL
testing_201510$DUNSPUBL <- NULL
testing_201510$contract_group <- NULL
testing_201510$sales_201511 <- NULL
testing_201510$sales_201512 <- NULL
testing_201510$sales_201601 <- NULL
testing_201510$sales_current <- testing_201510$sales_201510
testing_201510$sales_201510 <- NULL
names(testing_201510)
### return the variables that have missing values
sort(colSums(is.na(testing_201510)), decreasing = T)
testing_201510[is.na(testing_201510$DISTANCE),]$DISTANCE <- median(testing_201510$DISTANCE, na.rm = T)
testing_201510[is.na(testing_201510$contact_count),]$contact_count <- 0

### Feature engineering
library(dplyr)
# WA_S12X ranges from -206700 to large number, hard to get any sense out of it.
testing_201510 <- mutate(testing_201510, discount = (WA_S12X - (SALES12X - FINDS12X))/WA_S12X)
testing_201510 <- mutate(testing_201510, sellertype = CSG %/% 10000)
testing_201510$CSG <- NULL
testing_201510$sellertype <- as.factor(testing_201510$sellertype)
testing_201510 <- mutate(testing_201510, distance_far = (DISTANCE >= 5))
testing_201510 <- mutate(testing_201510, SOW = SALES12X/mrospend)
# INCLUDING DISTANCE_FAR DIDN'T BOOST THE MODEL PERFORMANCE.
testing_201510 <- mutate(testing_201510, trans_3month = TRANS01 + TRANS02 + TRANS03)
levels(testing_201510$churn) <- c("No", "Yes")
levels(testing_201510$indseg1) <- levels(training$indseg1)
levels(testing_201510$sellertype) <- levels(training$sellertype)
levels(testing_201510$dunsstat) <- levels(training$dunsstat)
levels(testing_201510$Customer_Size) <- levels(training$Customer_Size)
levels(testing_201510$Corp_Maj_Flag) <- levels(training$Corp_Maj_Flag)

#############################################################################
## Use RF_RFE_1000 to make predictions
## exclude missing values
pred_201510 <- predict(RF_RFE_1000, newdata = testing_201510[,c(1:197, 199, 201, 203)], 
                type = "prob")
confusionMatrix(pred_201510[, 2] >= 0.5, testing_201510$churn == "Yes", positive = "TRUE")
## Evaluate the model on testing data.
# Accuracy: 0.8392  
# Kappa: 0.5142
# Sens: 0.5429
library(ROCR)
ROCRpred_201510 <- prediction(pred_201510[,2], testing_201510$churn)
as.numeric(performance(ROCRpred_201510, "auc")@y.values)
# AUC value = 0.8932202

#############################################################################
## Use gbm_RFE_NA to make predictions
pred <- predict(gbm_RFE_NA, newdata = testing_201510[, c(1:197, 199, 201, 203)], 
                type = "prob")
confusionMatrix(pred[, 2] >= 0.5, testing_201510$churn == "Yes", positive = "TRUE")
## Evaluate the model on testing data.
# Accuracy: 0.8393
# Kappa: 0.5198
# Sens: 0.5578
library(ROCR)
ROCRpred <- prediction(pred[,2], testing_201510$churn)
as.numeric(performance(ROCRpred, "auc")@y.values)
# AUC value = 0.8946041

#############################################################################
## Use logReg to make predictions
pred <- predict(logReg_caret, newdata = testing_201510, type = "prob")
confusionMatrix(pred[, 2] >= 0.5, testing_201510$churn == "Yes", positive = "TRUE")
## Accuracy = 0.8349
## Kappa = 0.4863
## Sensitivity = 0.4949
library(ROCR)
ROCRpred <- prediction(pred[,2], testing_201510$churn)
as.numeric(performance(ROCRpred, "auc")@y.values)
# AUC value = 0.8909908

#############################################################################
## Train RF on entire account dataset
levels(accounts$churn) <- c("No", "Yes")
set.seed(80)
RF_RFE_full <- train(accounts[, c(1:197, 199, 201, 203)],
                     accounts$churn,
                     ntree = 500,
                     method = "rf", 
                     metric = "ROC",
                     trControl = trainControl(method = "cv", 
                                              number = 5,
                                              summaryFunction = multiClassSummary,
                                              classProbs = TRUE),
                     tuneGrid = expand.grid(mtry = c(14, 16, 18)),
                     do.trace = T)
#############################################################################
## Use RF_RFE_full to make predictions
pred_201510_full <- predict(RF_RFE_full, newdata = testing_201510[,c(1:197, 199, 201, 203)], 
                       type = "prob")
## use 0.3 as cutoff.
confusionMatrix(pred_201510_full[, 2] >= 0.5, testing_201510$churn == "Yes", positive = "TRUE")
## Evaluate the model on testing data.
# Accuracy: 0.8372  
# Kappa: 0.5018
# Sens: 0.5218
library(ROCR)
ROCRpred_201510_full <- prediction(pred_201510_full[,2], testing_201510$churn)
as.numeric(performance(ROCRpred_201510_full, "auc")@y.values)
# AUC value = 0.8932202

############################################################################
## Make predictions on latest data.
## Better remove uncommon sellertypes next time.
accounts_to_predict <- read.csv("accounts_to_predict.csv", stringsAsFactors = T)
names(accounts_to_predict)
colnames(accounts_to_predict[, lapply(accounts_to_predict, is.factor) == T])

#### GREATEST TRICK to level set
# common <- intersect(names(training), names(accounts_to_predict)) 
# for (p in common) { 
#   if (class(training[[p]]) == "factor") { 
#     levels(accounts_to_predict[[p]]) <- levels(training[[p]]) 
#   } 
# }
accounts_to_predict$CONTRACT_FLAG <- factor(accounts_to_predict$CONTRACT_FLAG, levels = levels(training$CONTRACT_FLAG))
accounts_to_predict$indseg1 <- factor(accounts_to_predict$indseg1, levels = levels(training$indseg1))
accounts_to_predict$Corp_Maj_Flag <- factor(accounts_to_predict$Corp_Maj_Flag, levels = levels(training$Corp_Maj_Flag))
accounts_to_predict$dunsstat <- factor(accounts_to_predict$dunsstat, levels = levels(training$dunsstat))
accounts_to_predict$Customer_Size <- factor(accounts_to_predict$Customer_Size, levels = levels(training$Customer_Size))
# accounts_to_predict$mro_decile <- factor(accounts_to_predict$mro_decile, levels = levels(training$mro_decile))
# accounts_to_predict$INVSOLFLG <- factor(accounts_to_predict$INVSOLFLG, levels = levels(training$INVSOLFLG))
accounts_to_predict$DUNSSBUS <- NULL
accounts_to_predict$DUNSPUBL <- NULL
accounts_to_predict$contract_group <- NULL
accounts_to_predict$sales_current <- accounts_to_predict$sales_201607
accounts_to_predict$sales_201607 <- NULL
names(accounts_to_predict)
### return the variables that have missing values
sort(colSums(is.na(accounts_to_predict)), decreasing = T)
accounts_to_predict[is.na(accounts_to_predict$DISTANCE),]$DISTANCE <- median(accounts_to_predict$DISTANCE, na.rm = T)
accounts_to_predict[is.na(accounts_to_predict$contact_count),]$contact_count <- 0
### Remove the cases with missing values
accounts_to_predict <- accounts_to_predict[complete.cases(accounts_to_predict),]


### Feature engineering
# WA_S12X ranges from -206700 to large number, hard to get any sense out of it.
# Note that if WA_S12x = 0, discount will be a missing value.
accounts_to_predict <- mutate(accounts_to_predict, discount = ((WA_S12X + 1) - (SALES12X - FINDS12X))/(WA_S12X+1))
summary(accounts_to_predict$discount)
accounts_to_predict[accounts_to_predict$discount < 0, ]$discount <- 0
accounts_to_predict[accounts_to_predict$discount > 1, ]$discount <- 1
accounts_to_predict <- mutate(accounts_to_predict, sellertype = CSG %/% 10000)
accounts_to_predict$CSG <- NULL
accounts_to_predict$sellertype <- factor(accounts_to_predict$sellertype, levels(training$sellertype))
accounts_to_predict <- mutate(accounts_to_predict, distance_far = (DISTANCE >= 5))
# accounts_to_predict <- mutate(accounts_to_predict, SOW = SALES12X/mrospend)
# INCLUDING DISTANCE_FAR DIDN'T BOOST THE MODEL PERFORMANCE.
accounts_to_predict <- mutate(accounts_to_predict, trans_3month = TRANS01 + TRANS02 + TRANS03)
#############################################################################
## Use RF_RFE_1000 to make predictions
## missing values in sellertype -- originally 35.
accounts_to_predict <- accounts_to_predict[is.na(accounts_to_predict$sellertype) != T, ]
pred_201606 <- predict(RF_RFE_1000, newdata = accounts_to_predict[,c(2:199, 201, 203)], 
                       type = "prob")
## Create the churn prediction variable.
accounts_to_predict <- mutate(accounts_to_predict, lapse_score = pred_201606[,2 ])
accounts_to_predict <- mutate(accounts_to_predict, lapse_flag = ifelse(lapse_score >= 0.25, 1,0))
accounts_to_predict$lapse_flag <- as.factor((accounts_to_predict$lapse_flag))
## profile the customers
## Need to convert lapse_flag to factor first, otherwise won't work.
## Need to aggregate data by lapse flag and the interested variable.
by_decile <- group_by(accounts_to_predict, mro_decile, lapse_flag)
decile_count <- summarise(by_decile,
                          count = n())
DF <- data.frame(mro_decile = decile_count$mro_decile, lapse_flag = decile_count$lapse_flag, count = decile_count$count)
ggplot(DF, aes(x = mro_decile, fill = count)) + geom_bar()
## Notice the difference with and without position = "fill
ggplot(accounts_to_predict, aes(x = mro_decile, fill = lapse_flag)) + geom_bar()
ggplot(accounts_to_predict, aes(x = mro_decile, fill = lapse_flag)) + geom_bar(position = "fill")
ggplot(accounts_to_predict, aes(x = mro_decile, fill = lapse_flag)) + geom_bar() +
  scale_y_continuous(labels = percent_format())
## display the % of accounts by two variables.
ggplot(DF, aes(x = mro_decile, y = count, fill = lapse_flag)) + geom_bar(stat = "identity") + ylab("%")
ggplot(DF, aes(x = mro_decile, y = count, fill = lapse_flag)) + geom_bar(position = "fill", stat = "identity") + ylab("%")
## fastest plot
accounts_to_predict <- mutate(accounts_to_predict, industry = ifelse(indseg1 == 0, "International/export",
                                                                     ifelse(indseg1 == 1, "Government",
                                                                            ifelse(indseg1 == 2, "Healthcare",
                                                                                   ifelse(indseg1 == 3, "Heavy MFG",
                                                                                          ifelse(indseg1 == 4, "Light MFG",
                                                                                                 ifelse(indseg1 == 5, "Hospitality",
                                                                                                        ifelse(indseg1 == 6, "Commercial Services",
                                                                                                               ifelse(indseg1 == 7, "Retail",
                                                                                                                      ifelse(indseg1 == 8, "Wholesale",
                                                                                                                             ifelse(indseg1 == 9, "Transportation",
                                                                                                                                    ifelse(indseg1 == 10, "Contractors",
                                                                                                                                           ifelse(indseg1 == 11, "Property Mgt",
                                                                                                                                                  ifelse(indseg1 == 12, "Natural resources", 
                                                                                                                                                         ifelse(indseg1 == 13, "Resellers", "Other")))))))))))))))
accounts_to_predict$industry <- as.factor(accounts_to_predict$industry)
accounts_to_predict <- accounts_to_predict[accounts_to_predict$industry != "Other", ]
ggplot(accounts_to_predict, aes(x = mro_decile, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts_to_predict, aes(x = industry, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
accounts_to_predict <- mutate(accounts_to_predict, contract_type = ifelse(Corp_Maj_Flag == "C", "National Accts",
                                                                          ifelse(Corp_Maj_Flag == "G", "Government GPO",
                                                                                 ifelse(Corp_Maj_Flag == "H", "Healthcare GPO",
                                                                                        ifelse(Corp_Maj_Flag == "M", "National Accts",
                                                                                               ifelse(Corp_Maj_Flag == "N", "Local Contracts",
                                                                                                      ifelse(Corp_Maj_Flag == "P", "Commercial GPO",
                                                                                                             ifelse(Corp_Maj_Flag == "S", "Select Advantage",
                                                                                                                    ifelse(Corp_Maj_Flag == "X", "Non-contract Local", "")))))))))
accounts_to_predict$contract_type <- as.factor(accounts_to_predict$contract_type)
ggplot(accounts_to_predict, aes(x = CONTRACT_FLAG, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts_to_predict, aes(x = contract_type, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(accounts_to_predict, aes(x = INVSOLFLG, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts_to_predict, aes(x = sellertype, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts_to_predict, aes(x = TENURE, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts_to_predict, aes(x = LINES12X, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts_to_predict, aes(x = RECENCY, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts")
## using different colors.
ggplot(accounts_to_predict, aes(x = TRANS12X, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts") + xlim(c(0,100)) + scale_fill_brewer(palette = "Blues")
ggplot(accounts_to_predict, aes(x = TRANS12X, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts")
accounts_to_predict <- mutate(accounts_to_predict, SOW_range = ifelse(SOW < 0, "0",
                                                                      ifelse(SOW < 0.2, "20%",
                                                                             ifelse(SOW < 0.4, "40%",
                                                                                    ifelse(SOW < 0.6, "60%",
                                                                                           ifelse(SOW < 0.8, "80%", "100%"))))))
accounts_to_predict$SOW_range <- as.factor(accounts_to_predict$SOW_range)
accounts_to_predict_active <- accounts_to_predict[accounts_to_predict$sales_current >0, ]
ggplot(accounts_to_predict_active, aes(x = TRANS12X, fill = lapse_flag)) + geom_bar(position = "fill") + xlim(c(0, 100)) + ylab("% of accounts")
ggplot(accounts_to_predict_active, aes(x = RECENCY, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts_to_predict_active, aes(x = LINES12X, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts_to_predict_active, aes(x = TENURE, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts")
accounts_to_predict_active[accounts_to_predict_active$sellertype %in% c(20, 31:34, 41:43, 71, 81, 82, 85),]$sellertype <- 0
table(accounts_to_predict_active$sellertype)
accounts_to_predict_active$sellertype <- factor(accounts_to_predict_active$sellertype)
accounts_to_predict_active <- mutate(accounts_to_predict_active, coverage = ifelse(sellertype == 0, "not covered",
                                                                                   ifelse(sellertype == 83, "FAR",
                                                                                          ifelse(sellertype %in% c(84, 88), "AM",
                                                                                                 ifelse(sellertype == 89, "Government ARM", 
                                                                                                        ifelse(sellertype == 86, "TSA",
                                                                                                               ifelse(sellertype %in% c(73:78), "ISA", "not covered")))))))
ggplot(accounts_to_predict_active, aes(x = coverage, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts_to_predict_active, aes(x = INVSOLFLG, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts_to_predict_active, aes(x = mro_decile, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts_to_predict_active, aes(x = industry, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(accounts_to_predict_active, aes(x = CONTRACT_FLAG, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts_to_predict_active, aes(x = contract_type, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts_to_predict_active, aes(x = SOW_range, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts") + scale_x_discrete(limits=c("0","20%","40%", "60%", "80%", "100%"))
ggplot(accounts_to_predict_active, aes(x = GP12X, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts") + xlim(c(0, 100))
accounts_to_predict_active <- mutate(accounts_to_predict_active, distance_range = ifelse(DISTANCE < 2, "< 2",
                                                                                         ifelse(DISTANCE < 5, "< 5",
                                                                                                ifelse(DISTANCE < 10, "< 10",
                                                                                                       ifelse(DISTANCE < 15, "< 15", "> 15")))))
accounts_to_predict_active$distance_range <- as.factor(accounts_to_predict_active$distance_range)
ggplot(accounts_to_predict_active, aes(x = distance_range, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts") + scale_x_discrete(limits = c("< 2", "< 5", "< 10", "< 15", "> 15"))
ggplot(accounts_to_predict_active, aes(x = CONTACTS, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts") + xlim(c(0, 100))

## See how the at-risk accounts are covered.
accounts_to_predict <- mutate(accounts_to_predict, coverage = ifelse(sellertype == 0, "not covered",
                                                                                   ifelse(sellertype == 83, "FAR",
                                                                                          ifelse(sellertype %in% c(84, 88), "AM",
                                                                                                 ifelse(sellertype == 89, "Government ARM", 
                                                                                                        ifelse(sellertype == 86, "TSA",
                                                                                                               ifelse(sellertype %in% c(73:78), "ISA", "not covered")))))))
table(accounts_to_predict[accounts_to_predict$lapse_flag==1, ]$coverage)

#### Export.
write.csv(accounts_to_predict[, c(1, 204:205)], "accounts_scored.csv", row.names = F)
write.csv(accounts_to_predict[, c(1, 19, 204:205)], "accounts_scored.csv", row.names = F)
############################################################################
# Measure running time of RF
library(caret)
time_start <- proc.time()
set.seed(80)
RF_time <- train(training[, c(1:197, 199, 201, 203)],
                 training[, 198],
                 ntree = 200,
                 method = "rf", 
                 metric = "ROC",
                 trControl = trainControl(method = "cv", 
                                          number = 2,
                                          summaryFunction = multiClassSummary,
                                          classProbs = TRUE),
                 tuneGrid = expand.grid(mtry = c(16)),
                 do.trace = T)
proc.time() - time_start

###################################
## Reproducible example
data(mtcars)
time_start <- proc.time()
set.seed(80)
RF_time <- randomForest(mtcars[, c(2:11)],
                 mtcars[, 1],
                 ntree = 50000,
                 nodesize = 1,
                 tuneGrid = expand.grid(mtry = c(1, 3, 6, 9, 12, 15, 18, 21)),
                 do.trace = T)
proc.time() - time_start

####################################
## paralell R
####################################
library(doSNOW)
library(foreach)
registerDoSNOW(makeCluster(1, type = "SOCK"))
time_start <- proc.time()
set.seed(80)
RF_time <- foreach(ntree = rep(50000, 1), .combine = combine, .packages = "randomForest") %dopar%
  randomForest(mtcars[, c(2:11)],
               mtcars[, 1],
               ntree = ntree,
               nodesize = 1,
               tuneGrid = expand.grid(mtry = c(1, 3, 6, 9, 12, 15, 18, 21)),
               do.trace = T)
proc.time() - time_start

install.packages("doSNOW")
install.packages("foreach")
library(doSNOW)
library(foreach)
library(randomForest)
stopCluster(makeCluster(10, type = "SOCK"))
registerDoSNOW(makeCluster(10, type = "SOCK"))
time_start <- proc.time()
set.seed(80)
RF_time <- foreach(ntree = rep(1000, 10), .combine = combine, .packages = "randomForest") %dopar%
  randomForest(training[, c(1:197, 199, 201, 203)],
               training[, 198],
               ntree = ntree,
               nodesize = 1,
               tuneGrid = expand.grid(mtry = 16),
               do.trace = T)
proc.time() - time_start

time_start <- proc.time()
set.seed(80)
RF_time <- randomForest(training[, c(1:197, 199, 201, 203)],
                        training[, 198],
                        ntree = 1000,
                        nodesize = 1,
                        tuneGrid = expand.grid(mtry = 16),
                        do.trace = T)
proc.time() - time_start

registerDoSNOW(makeCluster(1, type = "SOCK"))
time_start <- proc.time()
set.seed(80)
x <- matrix(runif(500), 1000)
y <- gl(2, 500)
RF_time <- foreach(ntree = rep(1000, 1), .combine = combine, .packages = "randomForest") %dopar%
  randomForest(x, y, ntree = ntree)
proc.time() - time_start

time_start <- proc.time()
set.seed(80)
RF_time <- randomForest(x, y, ntree = 1000)
proc.time() - time_start

# Remove other in industry
accounts <- accounts[accounts$indseg1 != 14, ]
accounts$industry <- factor(accounts$industry)

## Remove uncommon sellertypes. 
accounts$sellertype <- factor(accounts$sellertype)
accounts[accounts$sellertype %in% c(31:34, 41:43, 71, 81, 82, 85),]$sellertype <- 0
table(accounts$sellertype)
accounts$sellertype <- factor(accounts$sellertype)
table(accounts$sellertype)
table(accounts$seller)

testing_201604[testing_201604$sellertype %in% c(20, 31:34, 41:43, 71, 81, 82, 85),]$sellertype <- 0
table(testing_201604$sellertype)
testing_201604$sellertype <- factor(testing_201604$sellertype)
testing_201604 <- mutate(testing_201604, coverage = ifelse(sellertype == 0, "not covered",
                                                           ifelse(sellertype == 83, "FAR",
                                                                  ifelse(sellertype %in% c(84, 88), "AM",
                                                                         ifelse(sellertype == 86, "TSA",
                                                                                ifelse(sellertype %in% c(73:78), "ISA", "not covered"))))))
ggplot(testing_201604, aes(x = coverage, fill = lapse_flag)) + geom_bar(position = "fill") + ylab("% of accounts")

