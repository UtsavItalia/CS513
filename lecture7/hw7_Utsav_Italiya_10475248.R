######################################################
#  First Name  : Utsav
#  Last Name  : Italiya
#  Id   : 10475248

#to clear entire environment and installation
rm(list = ls())
library(neuralnet)

df = read.csv("F:/Sem1/CS513/lecture7/wisc_bc_ContinuousVar.csv",header=TRUE, sep=",")
View(df)
df$diagnosis<-ifelse(df$diagnosis == "M",2,1)

#70% training and 30% testing data
idx<-sort(sample(nrow(df),as.integer(.70*nrow(df))))
training<-df[idx,]
testing<-df[-idx,]

#ploting ANN
ann<- neuralnet( diagnosis~. ,training[,c(-1)], hidden=5,threshold=0.01)
plot(ann)

#prediction
prediction <-predict(ann , testing)
print(prediction)
pred_cat <- ifelse(prediction<1.5,1,2)
table(Actual = testing$diagnosis, Prediction = pred_cat)

#error rate
wrong<- (testing$diagnosis!=pred_cat)
error_rate<-sum(wrong)/length(wrong)
error_rate

#success rate
successrate <- 1 - error_rate
successrate

