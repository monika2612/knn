# -*- coding: utf-8 -*-
"""
Created on Tue Dec 10 15:44:04 2019

@author: lenovo
"""

# Importing Libraries 
import pandas as pd
import numpy as np


zoo = pd.read_csv("file:///C:/Users/lenovo/Downloads/zoo.csv")

# Training and Test data using 
from sklearn.model_selection import train_test_split
train,test = train_test_split(zoo,test_size = 0.2) # 0.2 => 20 percent of entire data 




# KNN using sklearn 
# Importing Knn algorithm from sklearn.neighbors
from sklearn.neighbors import KNeighborsClassifier as KNC

# for 3 nearest neighbours 
neigh = KNC(n_neighbors= 3)

# Fitting with training data 
neigh.fit(train.iloc[:,1:18],train.iloc[:,1:18])

# train accuracy 
train_acc = np.mean(neigh.predict(train.iloc[:,1:18])==train.iloc[:,1:18]) 

# test accuracy
test_acc = np.mean(neigh.predict(test.iloc[:,1:18])==test.iloc[:,1:18]) # 100%


# for 5 nearest neighbours
neigh = KNC(n_neighbors=5)

# fitting with training data
neigh.fit(train.iloc[:,1:18],train.iloc[:,1:18])

# train accuracy 
train_acc = np.mean(neigh.predict(train.iloc[:,1:18])==train.iloc[:,1:18])

# test accuracy
test_acc = np.mean(neigh.predict(test.iloc[:,1:18])==test.iloc[:,1:18])

# creating empty list variable 
acc = []

# running KNN algorithm for 3 to 50 nearest neighbours(odd numbers) and 
# storing the accuracy values 
 
for i in range(3,50,2):
    neigh = KNC(n_neighbors=i)
    neigh.fit(train.iloc[:,1:18],train.iloc[:,1:18])
    train_acc = np.mean(neigh.predict(train.iloc[:,1:18])==train.iloc[:,1:18])
    test_acc = np.mean(neigh.predict(test.iloc[:,1:18])==test.iloc[:,1:18])
    acc.append([train_acc,test_acc])



import matplotlib.pyplot as plt # library to do visualizations 

# train accuracy plot 
plt.plot(np.arange(3,50,2),[i[0] for i in acc],"bo-")



# test accuracy plot
plt.plot(np.arange(3,50,2),[i[1] for i in acc],"ro-")
plt.legend(["train","test"])




