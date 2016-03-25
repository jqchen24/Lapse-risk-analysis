accounts <- read.csv('accounts_churn_contact_flag.csv', stringsAsFactors = F)
str(accounts)
accounts$ ï..account <- NULL
library(ggplot2)
ggplot(accounts, aes(RECENCY)) + geom_bar()
ggplot(accounts, aes(TENURE)) + geom_bar()
