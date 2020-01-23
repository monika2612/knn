library(caTools)
library(dplyr)
library(ggplot2)
library(caret)
library(class)
library(corrplot)
zoo <- read.csv(file.choose())
#Standardize the Data
standard.features <- scale(zoo[,2:18])

#Join the standardized data with the target column
data <- cbind(standard.features,zoo[16])
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
predicted.type <- knn(train[2:18],test[2:18],train$type,k=1)
#Error in prediction
error <- mean(predicted.type!=test$type)
#Confusion Matrix
confusionMatrix(predicted.type,test$type)
predicted.type <- NULL
error.rate <- NULL

for (i in 1:10) {
  predicted.type <- knn(train[2:18],test[2:18],train$type,k=i)
  error.rate[i] <- mean(predicted.type!=test$type)
  
}

knn.error <- as.data.frame(cbind(k=2:18,error.type =error.rate))
##Choosing K Value by Visualization
ggplot(knn.error,aes(k,error.type))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=2:18)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')
########Result
predicted.type <- knn(train[2:18],test[2:18],train$type,k=3)
#Error in prediction
error <- mean(predicted.type!=test$type)
#Confusion Matrix
confusionMatrix(predicted.type,test$type)

