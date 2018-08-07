install.packages("readr")
install.packages("ggplot2")
install.packages("magrittr")
install.packages("dplyr")
install.packages("reshape2")
install.packages("pander")
install.packages("h2o")
install.packages("caret")
install.packages("e1071")

library(e1071)
library(caret)
library(ggplot2)
#library(magrittr)
#library(dplyr)
#library(reshape2)
#library(pander)
#library(h2o)
library(caTools)
#library(knitr)
library(rpart)



setwd("C:/Users/Admin/Desktop/Grillo/Assignment 1 - Plan of Action")
bank <- read.csv("bank.csv")

colnames(bank)
dim(bank)
str(bank)

#Finding missing values
table(bank$job)
table(bank$marital)
table(bank$education)
table(bank$default)
table(bank$housing)
table(bank$loan)

#Replacing missing values by mode
levels(bank$job)[levels(bank$job)==""]="admin."
levels(bank$marital)[levels(bank$marital)==""]="married"
levels(bank$education)[levels(bank$education)==""]="university.degree"
levels(bank$default)[levels(bank$default)==""]="no"
levels(bank$housing)[levels(bank$housing)==""]="yes"
levels(bank$loan)[levels(bank$loan)==""]="no"

#ploting the graph and filtering age group
ggplot(bank, aes(x = age)) + geom_histogram(binwidth = 5, col = "white") + theme_bw()

nrow(bank[bank$age < 18, ])
nrow(bank[bank$age > 90, ])
bank <- subset(bank,age > 18)
bank <- subset(bank,age < 90)
dim(bank)

#age vs y
ggplot(bank) + geom_histogram(aes(x = age), binwidth = 0.1, col = "white") +
  facet_grid(y~., scales = "free") + scale_x_log10() + theme()

#job vs y
ggplot(bank) + geom_bar(aes(x = job), col = "white") +
  facet_grid(y~., scales = "free")  + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#marital vs y
ggplot(bank) + geom_bar(aes(x = marital), col = "white") +
  facet_grid(y~., scales = "free")  + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#education vs y
ggplot(bank) + geom_bar(aes(x = education), col = "white") +
  facet_grid(y~., scales = "free")  + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#default vs y
ggplot(bank) + geom_bar(aes(x = default), col = "white") +
  facet_grid(y~., scales = "free")  + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#housing vs y
ggplot(bank) + geom_bar(aes(x = housing), col = "white") +
  facet_grid(y~., scales = "free")  + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#loan vs y
ggplot(bank) + geom_bar(aes(x = loan), col = "white") +
  facet_grid(y~., scales = "free")  + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#contact
ggplot(bank) + geom_bar(aes(x = contact), col = "white") +
  facet_grid(y~., scales = "free")  + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#month
ggplot(bank) + geom_bar(aes(x = month), col = "white") +
  facet_grid(y~., scales = "free")  + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#days
ggplot(bank) + geom_bar(aes(x = day_of_week), col = "white") +
  facet_grid(y~., scales = "free")  + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#duration
ggplot(bank) + geom_histogram(aes(x = duration), binwidth = 0.1) +
  facet_grid(y~., scales = "free")  + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#previous
ggplot(bank) + geom_histogram(aes(x = previous), binwidth = 1) +
  facet_grid(y~., scales = "free")  + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#poutcomes
ggplot(bank) + geom_bar(aes(x = poutcome), col = "white") +
  facet_grid(y~., scales = "free")  + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Cons.price.idx
ggplot(bank) + geom_histogram(aes(x = cons.price.idx), binwidth = 1) +
  facet_grid(y~., scales = "free")  + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Splitting dataset
set.seed(123)
split = sample.split(bank$y, SplitRatio = 0.75)
training_set = subset(bank, split == TRUE)
test_set = subset(bank, split == FALSE)


#Model Building
#Linear Model
classifier = glm(formula = y ~ .,
                 family = binomial,
                 data = training_set)
summary(classifier)

pred=predict(classifier,type = 'response')
pred_y=ifelse(pred > 0.5, 1, 0)
cm=table(test_set,pred_y>0.5)
#confusionMatrix(pred_y,test_set$y)

#Decission tree
model=rpart(y~.,training_set,method = 'class')
summary(model)

pred1=predict(model,test_set,type = "class")
confusionMatrix(pred1,test_set$y)
View(pred1)
table(test_set$y)
#Accuracy= 91.47%


#Navi Bayes
nb<-naiveBayes(y~.,data = training_set)
pred2=predict(nb,test_set,type = "class")
confusionMatrix(pred2,test_set$y)
#Accuracy=86.88%

#SVM
svm_model = svm(formula = y ~ .,
                 data = training_set,
                 kernel = 'radial')
pred3=predict(svm_model,test_set,type = "class")
confusionMatrix(pred3,test_set$y)
#Accuracy=90.75%

svm_model = svm(formula = y ~ .,
                data = training_set,
                kernel = 'linear')
pred3=predict(svm_model,test_set,type = "class")
confusionMatrix(pred3,test_set$y)
#Accuracy=90.53%

#Random forest 
install.packages("randomForest")
library(randomForest)
forest=randomForest(y~.,data = training_set)
pred4=predict(forest,test_set,type = "class")
confusionMatrix(pred4,test_set$y)
summary(forest)
#accuracy=91.7%
