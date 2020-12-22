library(ggplot2)
library(cowplot)
library(randomForest)

data <- read.csv("/Users/choyuching/Downloads/Final dataset_2.csv")
head(data)
str(data)

##Cleaning 
data$rank <- as.factor(data$rank)
str(data)

nrow(data[is.na(data$monetary_gain),])
data[is.na(data$monetary_gain),] 
nrow(data)
data <- data[!(is.na(data$monetary_gain),]
nrow(data)

## Now determine how many rows have "NA" (aka "Missing data"). If it's just
## a few, we can remove them from the dataset, otherwise we should consider
## imputing the values with a Random Forest or some other imputation method.
nrow(data[is.na(data$sentence_details) | is.na(data$corrupted_amount),])
data[is.na(data$sentence_details) | is.na(data$corrupted_amount),]
nrow(data)
data <- data[!(is.na(data$sentence_details) | is.na(data$corrupted_amount)),]
nrow(data)

##only 243 observations after NAs has been dropped

#####################################
##
## Now we can do some quality control by making sure all of the factor
## levels are represented by officials with and without monetary_gain
##
## NOTE: We also want to exclude variables that only have 1 or 2 samples in
## a category since +/- one or two samples can have a large effect on the
## odds/log(odds)
##
##
#####################################
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
xtabs(~ monetary_gain + oversea, data=data)
xtabs(~ monetary_gain + head_vice, data=data)

#####################################
##
## Now we are ready for some logistic regression. First we'll create a very
## simple model that uses predict
##
#####################################

## let's start super simple and see if Eank (1-highest,10-lowest) is a good
## predictor...
## First, let's just look at the raw data...
xtabs(~ monetary_gain + rankErin, data=data)

###########
logistic <- glm(monetary_gain ~ rankErin, data=data, family="binomial")
summary(logistic)

## Now calculate the overall "Pseudo R-squared" and its p-value

## NOTE: Since we are doing logistic regression...
## Null devaince = 2*(0 - LogLikelihood(null model))
##               = -2*LogLikihood(null model)
## Residual deviacne = 2*(0 - LogLikelihood(proposed model))
##                   = -2*LogLikelihood(proposed model)
ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
Rsquare<- (ll.null - ll.proposed) / ll.null

## chi-square value = 2*(LL(Proposed) - LL(Null))
## p-value = 1 - pchisq(chi-square value, df = 2-1)
1 - pchisq(2*(ll.proposed - ll.null), df=1)
1 - pchisq((logistic$null.deviance - logistic$deviance), df=1)

## Lastly, let's  see what this logistic regression predicts, given
## that an official is either Tiger or Fly (and no other data about them).

predicted.data <- data.frame(
  probability.of.monetary_gain=logistic$fitted.values,
  status_type=data$status_type)

## We can plot the data...
ggplot(data=predicted.data, aes(x=status_type, y=probability.of.monetary_gain)) +
  geom_point(aes(color=status_type), size=5) +
  xlab("Status_type") +
  ylab("Predicted probability of having monetary_gain")

## Since there are only two probabilities (one for females and one for males),
## we can use a table to summarize the predicted probabilities.
xtabs(~ probability.of.monetary_gain + status_type, data=predicted.data)


#####################################
##
## Now we will use all of the data available to predict monetary_gain
##
#####################################

logistic <- glm(monetary_gain ~ ., data=data, family="binomial")
summary(logistic)

## Now calculate the overall "Pseudo R-squared" and its p-value
ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null

## The p-value for the R^2
1 - pchisq(2*(ll.proposed - ll.null), df=(length(logistic$coefficients)-1))

## now we can plot the data
predicted.data <- data.frame(
  probability.of.monetary_gain=logistic$fitted.values,
  monetary_gain=data$monetary_gain)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.monetary_gain, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

## Lastly, we can plot the predicted probabilities for each sample having
## heart disease and color by whether or not they actually had heart disease
ggplot(data=predicted.data, aes(x=rank, y=probability.of.monetary_gain  )) +
  geom_point(aes(color=monetary_gain), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of having economic problems")

ggsave("/Users/choyuching/Downloads/logistical regression2.pdf")