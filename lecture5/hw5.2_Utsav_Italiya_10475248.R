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
summary(df)
str(df)
df[,"Class"] <- factor(df[,"Class"], levels = c("2", "4"), labels = c("Banign", "Malignant"))
str(df)

df<- df[2:11]
idx <- sample(1:nrow(df), 0.7 * nrow(df))

training <- df[sample(seq_len(nrow(df)), size = idx),]
testing <- df[-sample(seq_len(nrow(df)), size = idx),]

library(class)
library(rpart)
library(rpart.plot)

cart <- rpart(Class ~., data = training, method = "class");

prediction <- predict(cart, testing, type = "class")

rpart.plot(cart)
table(Actual=testing[,10],cart=prediction)

prediction2 <- predict(cart,testing)
prediction2_cat <- ifelse(prediction2[,1]<=.5,'Malignant','Banign')
table(Actual=testing[,10],cart=prediction2_cat)
wrong_predict <- sum(testing[,10]!=prediction2_cat)
error_rate <- wrong_predict/length(testing[,10])
error_rate
