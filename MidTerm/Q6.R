#################################################
#  First Name  : Utsav
#  Last Name  : Italiya
#  Id   : 10475248

rm(list = ls())

#read data from csv file
data <- read.csv("F:/Sem1/CS513/MidTerm/COVID19_v4.csv", header = TRUE, sep = ",")

#Summarizing data before handling NA or null values
summary(data)

#converting table into dataframe and Identifying bad values to replace with NA 
df <- data.frame(ID = as.integer(c(data$ID)), 
                 Age = as.integer(c(data$Age)),
                 Exposure = as.integer(c(data$Exposure)),
                 MaritalStatus = c(data$MaritalStatus),
                 Cases = as.integer(c(data$Cases)),
                 MonthAtHospital = as.integer(c(data$MonthAtHospital)),
                 Infected = c(data$Infected))

#Omiting rows with NA values
df <- na.omit(df)
View(df)

#normalizing data by replacing values in rows with string and categories
for(i in 1:nrow(df)){
  ifelse(df$MonthAtHospital[i] < 6, df$MonthAtHospital[i] <- 0, df$MonthAtHospital[i] <- 1)
  if(df$Age[i] < 35){
    df$Age[i] <- 0
  }
  if(df$Age[i] >= 35 && df$Age[i] <= 50){
    df$Age[i] <- 1
  }
  if(df$Age[i] > 50){
    df$Age[i] <- 2
  }
}

View(df)

#converting Infected row in factors
str(df)
df[,"Infected"] <- factor(df[,"Infected"], levels = c("Yes", "No"), labels = c("0", "1"))
str(df)

df<- df[2:7]
idx <- sample(1:nrow(df), 0.7 * nrow(df))

#30% testing and 70% training data 
training <- df[sample(seq_len(nrow(df)), size = idx),]
testing <- df[-sample(seq_len(nrow(df)), size = idx),]

library(class)
library(rpart)
library(rpart.plot)

cart <- rpart(Infected ~., data = training, method = "class");

prediction <- predict(cart, testing, type = "class")

rpart.plot(cart)
table(Actual=testing$Infected,cart=prediction)
wrong_predict <- sum(testing$Infected!=prediction)
error_rate <- wrong_predict/length(testing$Infected)
error_rate

