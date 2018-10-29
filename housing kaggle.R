# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 
#Kaggle Project 


library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

#importing file 
dftrain <- read.csv("../input/kc_house_data.csv", header = T)
#defining test size and train size 
train_size <- 20000


train <-head(dftrain,train_size)
test_size <-nrow(dftrain)-train_size
test<- tail(dftrain,test_size)
#check for  null values 
sapply(train, function(x) sum(is.na(x)))
head(train)
#calling dplyr library 
library(dplyr)
#removing unwanted variables 
ntrain <-select(train,-lat,-long,-id,-date,-yr_built,-yr_renovated,-zipcode,-bathrooms,-sqft_lot,-floors,-sqft_basement)
#finding correlations 
mat <-cor(ntrain)

#calling corrplot library 
library(corrplot)
corrplot(mat,method = 'shade')
#fitting the model 
lm.fit <-lm(price~. +bedrooms*waterfront+ sqft_living*sqft_above + grade*sqft_living + sqft_above*sqft_living + sqft_living15*sqft_living, data=ntrain)
#predicting the model 
p <- predict(lm.fit,test)
#adding it to df
actuals_preds <- data.frame(cbind(actuals=test$price, predicteds=p))

# Any results you write to the current directory are saved as output.