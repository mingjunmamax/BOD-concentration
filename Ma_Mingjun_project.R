library(ISLR)
library(aod)
library(stats)
getOption("defaultPackages")
library(prob)
library("dplyr")
setwd("/Users/maxma/Desktop/555/CS555/Ma_Mingjun_Project")
data_t<- read.csv("train.csv")
set.seed(1234)
par(mfrow=c(1,3))
hist(data_t$target, main = "Distribution of Target BOD level")
hist(data_t$X1, main = "Distribution of BOD level in Station 1")
hist(data_t$X2, main = "Distribution of BOD level in Station 2")
par(mfrow=c(1,3))
boxplot(data_t$target, main = "Distribution of Target BOD level")
boxplot(data_t$X1, main = "Distribution of BOD level in Station 1")
boxplot(data_t$X2, main = "Distribution of BOD level in Station 2")

print(summary(data_t$X1))
print(summary(data_t$X2))
print(summary(data_t$X3))
print(summary(data_t$X4))
print(summary(data_t$X5))
print(summary(data_t$X6))
print(summary(data_t$X7))
print(summary(data_t$target))
data_t<-data_t[ ,2:4]
s1<-fivenum(data_t$X1, na.rm = TRUE)
s2<-fivenum(data_t$X2, na.rm = TRUE)
sy<-fivenum(data_t$target, na.rm = TRUE)
#find the index of outlier
outlier_index <- which(data_t$target>=sy[4] + 1.5*(sy[4]-sy[2]))
data_t$X1[is.na(data_t$X1)] = s1[3]
data_t$X2[is.na(data_t$X2)] = s2[3]
#removing outliers
#data_t = data_t[-outlier_index, ]
# train-test split during training 
# Source: https://www.statology.org/train-test-split-r/
split<-sample(c(TRUE, FALSE), nrow(data_t), replace= TRUE, prob=c(0.7,0.3))
index_split<- which(split==TRUE)
test_split<- which(split==FALSE)
train<- data_t[index_split, ]
test<- data_t[test_split, ]
#linear regression
m1<- lm(train$target~train$X1)
m2<- lm(train$target~train$X2)
m12<-lm(train$target~train$X1+train$X2)
summary(m1)
summary(m2)
summary(m12)
predict_regression <- m12[["coefficients"]][["(Intercept)"]]+ m12[["coefficients"]][["train$X1"]]*test$X1+m12[["coefficients"]][["train$X2"]]*test$X2

total_ss <- sum((test$target - mean(test$target))^2)
reg_ss<-sum((predict_regression - mean(test$target))^2)
res_ss <- sum((test$target-predict_regression)^2)
r2 <- reg_ss/total_ss
paste("The R-Squared Score by model with station 1 and station 2 is ", round(r2, 3))
par(mfrow=c(1,3))
plot(test$X1, predict_regression, axes=TRUE, frame.plot=TRUE, xlab='X1 Test', ylab= "Predicted Target")
abline(m12)
plot(test$X2, predict_regression, axes=TRUE, frame.plot=TRUE, xlab='X2 Test', ylab= "Predicted Target")
abline(m12)
plot(predict_regression-test$target,axes=TRUE, frame.plot=TRUE, xlab='Target', ylab= "Residual")
abline(h = 0)
#removing outliers
data_r = data_t[-outlier_index, ]
split2<-sample(c(TRUE, FALSE), nrow(data_r), replace= TRUE, prob=c(0.7,0.3))
index_split2<- which(split2==TRUE)
test_split2<- which(split2==FALSE)
train2<- data_t[index_split2, ]
test2<- data_t[test_split2, ]
#linear regression
m1_r<- lm(train2$target~train2$X1)
m2_r<- lm(train2$target~train2$X2)
m12_r<- lm(train2$target~train2$X1+train2$X2)
summary(m1_r)
summary(m2_r)
summary(m12_r)

predict_regression2 <- m12_r[["coefficients"]][["(Intercept)"]]+ m12_r[["coefficients"]][["train2$X1"]]*test2$X1 + m12_r[["coefficients"]][["train2$X2"]]*test2$X2
total_ss2 <- sum((test2$target - mean(test2$target))^2)
reg_ss2<-sum((predict_regression2 - mean(test2$target))^2)
res_ss2 <- sum((test2$target-predict_regression2)^2)
r2r <- reg_ss2/total_ss2
paste("The R-Squared Score by model with station 1 and station 2 while removing the outliers is ", round(r2r, 3))
# I found out that removing outliers does not help the accuracy. Therefore, I choose to stay with the result without removal of three outliers.




