#Classification using Adaboost 

rm(list=ls(all=TRUE))
library(RCurl)

data=read.table(text = getURL("https://raw.githubusercontent.com/rajsiddarth/Adaboost/master/dataset.csv"), header=T, sep=',',
                col.names = c('ID', 'age', 'exp', 'inc', 
                              'zip', 'family', 'ccavg', 'edu', 
                              'mortgage', 'loan', 'securities', 
                              'cd', 'online', 'cc'))
#Removing the id, zip and experience

data=subset(data,select = -c(ID,zip,exp))

#Numeric attributes : age,inc,family,CCAvg,Mortgage
#Categorical: Education,Securities account,CD Account,Online,Credit card
#Target Variable: Personal Loan
num_data=data.frame(sapply(data[c('age','inc','family','ccavg')],function(x){as.numeric(x)}))
#Converting numeric data to similar range
library(vegan)
num_data=decostand(num_data,method = 'range')

#Categorical to numerical
library(dummies)
categ_attributes=c('edu','securities','cd','online')
categ_data=data.frame(sapply(data[categ_attributes],function(x){as.factor(x)}))
categ_data=dummy.data.frame(categ_data,sep="_")
loan=data$loan

#Final data
data=cbind(num_data,categ_data,loan)
str(data)

#Dividing into train and test
library(caTools)
index=sample.split(data$loan,SplitRatio = 0.7)
train=data[index,]
test=data[!index,]
ind_variable=setdiff(names(data),"loan")

#Building the classification model using Adaboost:
#install.packages("ada")
library(ada)
model = ada(x = train[ind_variable],y = train$loan,iter=20, loss="logistic")

# Accuracy.
# Predict on train data

pred_train = predict(model, train[,ind_variable])

# Building confusion matrix to find accuracy
cmatrix_train = table("actual"=train$loan,predicted= pred_train)
cmatrix_train
accuracy_train= sum(diag(cmatrix_train))/sum(cmatrix_train)

cat("accuracy on train data= ",round(accuracy_train,3)*100)

# Predict on test data
pred_test = predict(model, test[,ind_variable])

# Build confusion matrix and find accuracy
cmatrix_test = table(test$loan, pred_test)

accuracy_test= sum(diag(cmatrix_test))/sum(cmatrix_test)

cat("accuracy on test data= ",round(accuracy_test,3)*100)



    