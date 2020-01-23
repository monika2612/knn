# Read the dataset
zoo <- read.csv(file.choose())
class(zoo)
View(zoo)
str(zoo)
zoo1 <- zoo[,2:18]
str(zoo1)
#table of domestic

table(zoo$domestic)


# table or proportation of enteries in the datasets. What % of entry is Bengin and % of entry is Malignant
round(prop.table(table(zoo$domestic))*100,1)
summary(zoo)
#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))
#Apply the normalization function to zoo dataset
zoo_n <- as.data.frame(lapply(zoo[1:18], norm))
View(zoo_n)

#create training and test datasets
zoo_train <- zoo_n[1:80,]
zoo_test <- zoo_n[81:101,]

#Get labels for training and test datasets

zoo_train_labels <- zoo[1:80,16]
View(zoo_train_labels)
zoo_test_labels <- zoo[80:101,16]
View(zoo_test_labels)

# Build a KNN model on taining dataset
library("class")
library("caret")
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
zoo_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=21)
class(zoo_train)
class(zoo_test)
## Now evualuation the model performance

# install package gmodels
install.packages("gmodels")
library("gmodels")

# Create cross table of predicted and actual
CrossTable( x =  zoo_test_labels, y = zoo_pred)

