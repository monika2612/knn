# Read the dataset
glass <- read.csv(file.choose())
class(glass)
View(glass)
str(glass)
#table of type 
table(glass$Type)


# table or proportation of enteries in the datasets. What % of entry is Bengin and % of entry is Malignant
round(prop.table(table(glass$Type))*100,1)
summary(glass)
#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))
#Apply the normalization function to glass dataset
glass_n <- as.data.frame(lapply(glass[1:10], norm))
View(glass_n)

#create training and test datasets
glass_train <- glass_n[1:180,]
glass_test <- glass_n[181:214,]

#Get labels for training and test datasets

glass_train_labels <- glass[1:180,10]
View(glass_train_labels)
glass_test_labels <- glass[181:214,10]
View(glass_test_labels)

# Build a KNN model on taining dataset
library("class")
library("caret")
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=21)
class(glass_train)
class(glass_test)
## Now evualuation the model performance

# install package gmodels
install.packages("gmodels")
library("gmodels")

# Create cross table of predicted and actual
CrossTable( x =  glass_test_labels, y = glass_pred)
