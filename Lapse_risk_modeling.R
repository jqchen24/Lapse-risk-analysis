##########################################
# Load the dataset and remove account # #
##########################################
accounts <- read.csv('accounts_churn_contact_flag.csv', stringsAsFactors = F)
str(accounts)
accounts$ ï..account <- NULL

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
logReg <- glm(churn ~ CONTACTS + TENURE + TRANS12X + SALES12X + LINES12X + 
                contract_group, data = accounts_train, family = binomial)

# Evaluate the model
library(caret)
predict_logReg <- predict(logReg, newdata = accounts_test, type = 'response')
# Note that both arguments in the confusionMatrix have to have the same values (either T/F or 0/1)
confusionMatrix(accounts_test$churn == 1, predict_logReg >= 0.5)
# accuracy is 83.99%

