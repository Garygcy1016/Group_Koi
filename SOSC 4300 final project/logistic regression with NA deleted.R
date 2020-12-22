library(ggplot2)
library(cowplot)
library(randomForest)

data <- read.csv('/Users/choyuching/Downloads/0133.csv')
head(data)
str(data)

data$head_vice<-as.factor(data$head_vice)
data$rankErin<-as.factor(data$rankErin)
str(data)

nrow(data[is.na(data$corrupted_amount) | is.na(data$sentence_details),])
data[is.na(data$corrupted_amount) | is.na(data$sentence_details),]
nrow(data)
data <- data[!(is.na(data$corrupted_amount) | is.na(data$sentence_details)),]
nrow(data)

xtabs(~ monetary_gain + gender, data=data)
xtabs(~ monetary_gain + status_type, data=data)
xtabs(~ monetary_gain + year_born, data=data)
xtabs(~ monetary_gain + native_province, data=data)
xtabs(~ monetary_gain + corruption_location, data=data)
xtabs(~ monetary_gain + local_official, data=data)
xtabs(~ monetary_gain + rankErin, data=data)
xtabs(~ monetary_gain + sentence_details, data=data)
xtabs(~ monetary_gain + sector, data=data)
xtabs(~ monetary_gain + corrupted_amount, data=data)
xtabs(~ monetary_gain + connections_with_tigers, data=data)
xtabs(~ monetary_gain + head_vice, data=data)
xtabs(~ monetary_gain + oversea, data=data)


logistic <- glm(monetary_gain ~ gender, data=data, family="binomial")
summary(logistic)
