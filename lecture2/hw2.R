#################################################
#  Company    : Stevens 
#  Project    : HW_02_EDA
#  Purpose    : EDA
#  First Name  : Utsav
#  Last Name  : Italiya
#  Id			    : 10475248

rm(list = ls())

#read data from csv file
data <- read.csv("F:/Sem1/CS513/lecture2/breast-cancer-wisconsin.csv", header = TRUE, sep = ",")

#Summarizing data before handling NA or null values
summary(data)
View(data)

#converting table into dataframe and Identifying bad values to replace with NA 
df <- data.frame(Sample = as.integer(c(data$Sample)), 
                 F1 = as.integer(c(data$F1)),
                 F2 = as.integer(c(data$F2)),
                 F3 = as.integer(c(data$F3)),
                 F4 = as.integer(c(data$F4)),
                 F5 = as.integer(c(data$F5)),
                 F6 = as.integer(c(data$F6)),
                 F7 = as.integer(c(data$F7)),
                 F8 = as.integer(c(data$F8)),
                 F9 = as.integer(c(data$F9)),
                 Class = as.integer(c(data$Class)))

#replacing NA values with Round value of mean of that column
df$F6[is.na(df$F6)] <-  round(mean(df$F6, na.rm = TRUE))

#summarizing data after handling NA values in dataframe
View(df)
summary(df)

#creating frequency table of Class vs F6
table(df$Class,df$F6)

plot(df[,2:7])

par(mfrow = c(3,1))

hist(df$F7,
     xlim = c(0,3),
     main = "Histogram for F7",
     xlab = "")
hist(df$F8,
     xlim = c(0,3),
     main = "Histogram for F7",
     xlab = "")
hist(df$F9,
     xlim = c(0,3),
     main = "Histogram for F7",
     xlab = "")

summary(df)
View(df)

rm(list = ls())
detach("package:datasets", unload = TRUE)
dev.off()
cat("\014")

data <- read.csv("F:/Sem1/CS513/lecture2/breast-cancer-wisconsin.csv", header = TRUE, sep = ",")

is.na(data)
