######################################################
#  First Name  : Utsav
#  Last Name  : Italiya
#  Id   : 10475248
######################################################

#to clear entire environment and installation
rm(list = ls())
library(e1071)

df <- read.csv("F:/Sem1/CS513/lecture9/wisc_bc_ContinuousVar.csv",header=TRUE, sep=",")
df <- subset(df, select = -c(id))
df$diagnosis<-factor(df$diagnosis,  levels = c('M','B'),labels = c(1,2))
View(df)

#70% training and 30% testing data
idx<-sort(sample(nrow(df),as.integer(.70*nrow(df))))
training<-df[idx,]
testing<-df[-idx,]

#training SVM model
svm_model <- svm( diagnosis~ ., data =training  )
svm_pred <- predict(svm_model,  testing )

#confusion matrix and accuracy
confusion_matrix <-table(predict_svm = svm_pred, class = testing$diagnosis)
confusion_matrix
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(confusion_matrix)

