library(caTools)
library(dplyr)
library(ggplot2)
library(caret)
library(class)
library(corrplot)
glass <- read.csv(file.choose())
#Standardize the Data
standard.features <- scale(glass[,1:9])

#Join the standardized data with the target column
data <- cbind(standard.features,glass[10])
#Check if there are any missing values to impute. 
anyNA(data)
# Looks like the data is free from NA's
head(data)
##Data Visualization
corrplot(cor(data))
####Test and Train Data Split
set.seed(101)

sample <- sample.split(data$Type,SplitRatio = 0.70)

train <- subset(data,sample==TRUE)

test <- subset(data,sample==FALSE)
###KNN Model
predicted.type <- knn(train[1:9],test[1:9],train$Type,k=1)
#Error in prediction
error <- mean(predicted.type!=test$Type)
#Confusion Matrix
confusionMatrix(predicted.type,test$Type)
predicted.type <- NULL
error.rate <- NULL

for (i in 1:10) {
  predicted.type <- knn(train[1:9],test[1:9],train$Type,k=i)
  error.rate[i] <- mean(predicted.type!=test$Type)
  
}

knn.error <- as.data.frame(cbind(k=1:10,error.type =error.rate))
##Choosing K Value by Visualization
ggplot(knn.error,aes(k,error.type))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:10)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')
########Result
predicted.type <- knn(train[1:9],test[1:9],train$Type,k=3)
#Error in prediction
error <- mean(predicted.type!=test$Type)
#Confusion Matrix
confusionMatrix(predicted.type,test$Type)

