# Load important packages
library(tidyr)
library(randomForest)
library(AUCRF)
library(dplyr)
library(party)
library(rpart)
library(rpart.plot)
library(rattle)
library(ggplot2)
library(pdp)
library(ROCR)
library(MLmetrics)
library(caret)
library(e1071)



# First we read in the data and clean it
setwd("/Users/sbothwell/Desktop/DATAS/My_material/Random_Forest")
flights = read.csv("FlightDelays.csv", stringsAsFactors = TRUE)

# Remove unneeded columns
flights = flights[,-c(1,9)]

# Make all variables either numeric or factor
cols <- c(2,7)
flights[cols] <- lapply(flights[cols], numeric)



set.seed(2019) # set seed for reproducible results

## 75% of the sample size
smp_size <- floor(0.75 * nrow(flights))
train_ind <- sample(seq_len(nrow(flights)), size = smp_size)

# Testing and Training Data
train <- flights[train_ind, ]
test <- flights[-train_ind, ]

# Random Forest
model = randomForest(`Delayed30`~., data = train)


# make a function to print certain text in tree nodes
nodefun = function(x, labs, digits, varlen){
  paste("n =", x$frame$n, "  ", 
        "\nPct =", round(x$frame$n/x$frame$n[1]*100,2),"%")
}

# plot decision tree
tree = rpart(model, train, control=rpart.control(minsplit=2, minbucket=1, cp=0.001))
fancyRpartPlot(tree, palettes=c("Reds"), main="Decision Tree Graph", sub="",
               yesno = 2)

# Confusion Matrix
predictions = predict(model, test)
confusionMatrix(predictions, test$Delayed30)
