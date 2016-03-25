accounts <- read.csv('accounts_churn_contact_flag.csv', stringsAsFactors = F)
str(accounts)
accounts$ ï..account <- NULL
library(ggplot2)
ggplot(accounts, aes(RECENCY)) + geom_bar()
ggplot(accounts, aes(TENURE)) + geom_bar()
names(accounts)
table(accounts$Customer_Size)
table(accounts$CONTRACT_FLAG)
table(accounts$mro_decile)
table(accounts$indseg0)
# 22.7% accounts lapsed in the data.
table(accounts$churn)/nrow(accounts)
sort(tapply(accounts$churn, accounts$indseg0, mean), decreasing = T)
table(accounts$churn, accounts$indseg0)
sort(tapply(accounts$churn, accounts$Customer_Size, mean), decreasing = T)
table(accounts$churn, accounts$Customer_Size)
# Looks like indseg0 and customer_size are two factors that can
# separate churn and non-churn.
sort(tapply(accounts$churn, accounts$CONTRACT_FLAG, mean), decreasing = T)
table(accounts$churn, accounts$CONTRACT_FLAG)

# split the dataset to training/test
library(caTools)
set.seed(88)
spl <- sample.split(accounts$churn, SplitRatio = 0.8)
accounts_train <- subset(accounts, spl == T)
accounts_test <- subset(accounts, spl == F)
table(accounts_train$churn)/nrow(accounts_train)
# The distribution of churn in the training data is the same as in original data.

