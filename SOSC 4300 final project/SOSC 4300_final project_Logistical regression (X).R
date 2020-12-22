library(ggplot2)
library(cowplot)
library(randomForest)

data <- read.csv("/Users/choyuching/Downloads/Final (one sheet only)3.csv")
head(data)
str(data)

##Cleaning 
data$rank <- as.factor(data$rank)
str(data)

## Now determine how many rows have "NA" (aka "Missing data"). If it's just
## a few, we can remove them from the dataset, otherwise we should consider
## imputing the values with a Random Forest or some other imputation method.
nrow(data[is.na(data$sentence_details) | is.na(data$corrupted_amount),])
data[is.na(data$sentence_details) | is.na(data$corrupted_amount),]
## so 6 of the 303 rows of data have missing values. This isn't a large
## percentage (2%), so we can just remove them from the dataset
## NOTE: This is different from when we did machine learning with
## Random Forests. When we did that, we imputed values.
set.seed(500)
data.imputed <- rfImpute(rank ~ ., data = data, iter=6)

#####################################
##
## Now we can do some quality control by making sure all of the factor
## levels are represented by officials with and without economic problem (hd)
##
## NOTE: We also want to exclude variables that only have 1 or 2 samples in
## a category since +/- one or two samples can have a large effect on the
## odds/log(odds)
##
##
#####################################
xtabs(~ economic_problem + gender, data=data.imputed)
xtabs(~ economic_problem + status_type, data=data.imputed)
xtabs(~ economic_problem + year_born, data=data.imputed)
xtabs(~ economic_problem + native_province, data=data.imputed)
xtabs(~ economic_problem + corruption_location, data=data.imputed)
xtabs(~ economic_problem + local_official, data=data.imputed)
xtabs(~ economic_problem + rank, data=data.imputed)
xtabs(~ economic_problem + sentence_details, data=data.imputed)
xtabs(~ economic_problem + sector, data=data.imputed)
xtabs(~ economic_problem + corrupted_amount, data=data.imputed)
xtabs(~ economic_problem + connections_with_tigers, data=data.imputed)
#####################################

#####################################
##
## Now we are ready for some logistic regression. First we'll create a very
## simple model that uses sex to predict heart disease
##
#####################################

## let's start super simple and see if status_type (Tiger/Fly) is a good
## predictor...
## First, let's just look at the raw data...
xtabs(~ economic_problem + status_type, data=data.imputed)

###########
logistic <- glm(economic_problem ~ status_type, data=data.imputed, family="binomial")
summary(logistic)


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
  probability.of.economic_problem=logistic$fitted.values,
  status_type=data.imputed$status_type)

## We can plot the data...
ggplot(data=predicted.data, aes(x=status_type, y=probability.of.economic_problem)) +
  geom_point(aes(color=status_type), size=5) +
  xlab("Status_type") +
  ylab("Predicted probability of having economic_problem")

## Since there are only two probabilities (one for females and one for males),
## we can use a table to summarize the predicted probabilities.
xtabs(~ probability.of.economic_problem + status_type, data=predicted.data)


#####################################
##
## Now we will use all of the data available to predict economic_problem
##
#####################################

logistic <- glm(economic_problem ~ ., data=data.imputed, family="binomial")
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
  probability.of.economic_problem=logistic$fitted.values,
  economic_problem=data.imputed$economic_problem)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.economic_problem, decreasing=FALSE),]

predicted.data$rank <- 1:nrow(predicted.data)

## Lastly, we can plot the predicted probabilities for each sample having
## heart disease and color by whether or not they actually had heart disease
ggplot(data=predicted.data, aes(x=rank, y=probability.of.economic_problem  )) +
  geom_point(aes(color=economic_problem), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of having economic problems")

ggsave("/Users/choyuching/Downloads/logistical regression3.pdf")