######################################################
#  First Name  : Utsav
#  Last Name  : Italiya
#  Id   : 10475248

#to clear entire environment
rm(list = ls())

#loaded data from csv file
df = read.csv("F:/Sem1/CS513/lecture3/breast-cancer-wisconsin.csv",header=TRUE, sep=",")

#converted data into dataframe
df <- data.frame(Sample = as.integer(c(df$Sample)), 
                 F1 = as.integer(c(df$F1)),
                 F2 = as.integer(c(df$F2)),
                 F3 = as.integer(c(df$F3)),
                 F4 = as.integer(c(df$F4)),
                 F5 = as.integer(c(df$F5)),
                 F6 = as.integer(c(df$F6)),
                 F7 = as.integer(c(df$F7)),
                 F8 = as.integer(c(df$F8)),
                 F9 = as.integer(c(df$F9)),
                 Class = as.integer(c(df$Class)))

#remove rows with NA values
df <- na.omit(df)

#category row is Class which has to be Factor form
#changing type of Class column to Factor
str(df)
df[,"Class"] <- factor(df[,"Class"], levels = c("2", "4"), labels = c("Banign", "Malignant"))
str(df)

#using 70% data for testing and 30% data for training classificaion model
idx <- sample(1:nrow(df), 0.7 * nrow(df))
training <- df[2:11][sample(seq_len(nrow(df[2:11])), size = idx),]  
testing <- df[2:11][-sample(seq_len(nrow(df[2:11])), size = idx),]

library(class)
#library(e1071) has naiveBayes package included
library(e1071)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

#training naive bayes model
trained_naive <- naiveBayes(Class ~.,data = training)
#predicting in trained data with use of testing data
predict_naive <- predict(trained_naive,testing)

#confusion matrix and accuracy measure
confusion_matrix <- table(predict_naive,testing$Class)
confusion_matrix
accuracy(confusion_matrix)
