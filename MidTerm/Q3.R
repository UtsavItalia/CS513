#################################################
#  First Name  : Utsav
#  Last Name  : Italiya
#  Id   : 10475248

rm(list = ls())

#read data from csv file
data <- read.csv("F:/Sem1/CS513/MidTerm/COVID19_v4.csv", header = TRUE, sep = ",")

#Summarizing data before handling NA or null values
summary(data)
View(data)

#converting table into dataframe and Identifying bad values to replace with NA 
df <- data.frame(ID = as.integer(c(data$ID)), 
                 Age = as.integer(c(data$Age)),
                 Exposure = as.integer(c(data$Exposure)),
                 MaritalStatus = c(data$MaritalStatus),
                 Cases = as.integer(c(data$Cases)),
                 MonthAtHospital = as.integer(c(data$MonthAtHospital)),
                 Infected = c(data$Infected))

#replacing NA values with Round value of mode of that column
Mode <- which.max(tabulate(df$Age))
Mode
df$Age[is.na(df$Age)] <- Mode


#summarizing data after handling NA values in dataframe
View(df)
summary(df)


#Displaying the scatter plot of "Age", "Exposure" and "MonthAtHospital" one pair at a time
plot(df[c(2,3,6)])

#Show histogram box plot for columns age, monthAtHospital and Exposure
#############################################
par(mfrow = c(1,3))

boxplot(df$Age,
     main = "BoxPlot for Age",
     xlab = "",
     col = 'purple')
boxplot(df$MonthAtHospital,
     main = "BoxPlot for MonthAtHospital",
     xlab = "",
     col = 'red')
boxplot(df$Exposure,
     main = "BoxPlot for Exposure",
     xlab = "",
     col = 'pink')
#############################################

#Delete all the objects from your R- environment. 
#Reload the "breast-cancer-wisconsin.data.csv" from canvas into R.
#############################################
rm(list = ls())


