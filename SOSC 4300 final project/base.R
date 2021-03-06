library(ggplot2)
library(cowplot)
library(randomForest)

data <- read.csv('/Users/choyuching/Desktop/SOSC 4300 final project/from 22:12 based on this.csv')
head(data)
str(data)

data$status_type<-as.factor(data$status_type)
data$year_born <-as.factor(data$year_born)
data$gender<-as.factor(data$gender)
data$native_province<-as.factor(data$native_province)
data$corruption_location<-as.factor(data$corruption_location)
data$local_official<-as.factor(data$local_official)
data$rankErin<-as.factor(data$rankErin)
data$sentence_details<-as.factor(data$sentence_details)
data$monetary_gain <-as.factor(data$monetary_gain)
data$corrupted_amount<-as.factor(data$corrupted_amount)
data$sector<-as.factor(data$sector)
data$connections_with_tigers<-as.factor(data$connections_with_tigers)
data$oversea<-as.factor(data$oversea)
data$head_vice<-as.factor(data$head_vice)

set.seed(100)
data.imputed <- rfImpute(rankErin ~ ., data = data, iter=4)
str(data.imputed)

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

model <- randomForest(monetary_gain ~ ., data=data.imputed, proximity=TRUE)
model
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Yes", "No"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"Yes"], 
          model$err.rate[,"No"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))
ggsave("/Users/choyuching/Downloads/RF_NAnotdeleted_model1(3).pdf")

## If we want to compare this random forest to others with different values for
## mtry (to control how many variables are considered at each step)...
oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(monetary_gain ~ ., data=data.imputed, mtry=i, ntree=500)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}

oob.values
## find the minimum error
min(oob.values)
## find the optimal value for mtry...
which(oob.values == min(oob.values))
## create a model for proximities using the best value for mtry
model <- randomForest(monetary_gain ~ ., 
                      data=data.imputed,
                      ntree=500, 
                      proximity=TRUE, 
                      mtry=which(oob.values == min(oob.values)))
model

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Yes", "No"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"Yes"], 
          model$err.rate[,"No"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))
ggsave("/Users/choyuching/Downloads/Base R pic1.pdf")

#MDS plot
distance.matrix <- as.dist(1-model$proximity)

mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

## calculate the percentage of variation that each MDS axis accounts for...
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

## now make plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=data.imputed$monetary_gain)



ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) + 
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")
ggsave("/Users/choyuching/Downloads/Base R pic2.pdf")