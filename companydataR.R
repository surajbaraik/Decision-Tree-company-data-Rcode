

install.packages("C50") # we neeed to install C50 package to use ak
install.packages("tree")
library(C50)
install.packages("rpart")
library(rpart.plot) # use prp() to make cleaner plot with caret
install.packages("ISLR")
library(ISLR)

company_data <- read.csv(file.choose())
View(company_data)
summary(company_data)
colnames(company_data)
# Splitting data into training and testing. As the species are in order 
# splitting the data based on species 
# Change Sales to a qualitative variable by splitting it on the median.
company_data$Sales <- ifelse(company_data$Sales <= median(company_data$Sales), 'Low', 'High')
company_data$Sales <- factor(company_data$Sales)
View(company_data)

company_data<-na.omit(company_data)

library(caret)
#Split data into train / validation
set.seed(111)
split <- createDataPartition(y=company_data$Sales, p=0.6, list=FALSE)
train <- company_data[split,]
test <- company_data[-split,]

# Building model on training data 

sales_train <- C5.0(Sales~.,data= train)
windows()
plot(sales_train) # Tree graph
# Training accuracy
pred_train <- predict(sales_train,train)

mean(train$Sales==pred_train) # 96.26% Accuracy


confusionMatrix(pred_train,train$Sales)
#This train model is giving us 96% accuracy with error rate of 95% we will check with test model

pred_test <- predict(sales_train,newdata=test) # predicting on test data
mean(pred_test==test$Sales) # 72.95% accuracy 

confusionMatrix(pred_test,test$Sales)
#This train model is giving us 72% accuracy with error rate of 73% 
#As model will have extremely low training error but a high testing error.
#this tells us that model is overfitting

library(gmodels)
# Cross tablez
CrossTable(test$Sales,pred_test)

#A test error rate of ~73.5% is pretty good! But we could potentially improve it with cross validation.

#Cross Validating
library(tree)

set.seed(12)
tree <- tree(Sales~.,data=train)
cv_sales_tree <- cv.tree(tree, FUN=prune.misclass)
plot(cv_sales_tree)

#Here we see that the the lowest / simplest misclassification error is for a 4 leaf model. We can now prune the tree to a 4 leaf model.

prune_sales_tree <- prune.misclass(tree, best=4)
prune.pred <- predict(prune_sales_tree, test, type='class')

plot(prune_sales_tree)
text(prune_sales_tree, pretty=0)

confusionMatrix(prune.pred, test$Sales)

#This doesnt really improve our classification as accuracy dips down to 69%
# We need to go with different splitting method or go with other variable for decision dependent atribute other than sales to improve accuracy
# with 1st model we are getting 72% is test accuracy which is also good we will go with that.
