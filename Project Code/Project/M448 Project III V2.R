################################
# The Course Project: Industry #
################################

#############
# Libraries #
#############

library(caret)
library(ggplot2)
library(stats)
library(MASS) # QDA & LDA
library(class) # KNN
library(leaps) # best subset
library(glmnet) # ridge regress & LASSO

#############
# Data Prep #
#############

Rnames = read.csv("Resume Names.csv", header = T, na.strings = "?", stringsAsFactors = T)
dim(Rnames)
names(Rnames)
summary(Rnames)

# DownSampled Dataset 

NewNames <- downSample(Rnames, Rnames$call)
summary(NewNames)
names(NewNames)

# New Dataset 

Names <- subset(NewNames, select = -c(X, name, city, Class))
names(Names)
summary(Names)
sum(is.na(Names)) #checking for missing values

#training & test data
train = sample(1:nrow(Names),0.8*nrow(Names))
test = -train
train.data = Names[train,]
test.data = Names[-train,]

#glm.fit
glm.fit = glm(call~., train.data, family = "binomial")
summary(glm.fit)

glm.fit1 = glm(call~ethnicity+experience+special+reqeduc+honors+reqcomp+industry, train.data, family = "binomial")
summary(glm.fit1)
glm.probs1 = predict(glm.fit1,test.data,type="response")
glm.pred1 = rep("no",nrow(test.data))
glm.pred1[glm.probs1 > 0.5] = "yes"
table(glm.pred1,test.data$call)
mean(glm.pred1 == test.data$call)

###################################
# Quadratic Discriminant Analysis #
###################################

qda.fit = qda(call~ethnicity+experience+special+reqeduc+honors+reqcomp+industry, data =  train.data)
qda.fit
qda.class = predict(qda.fit, test.data)$class
table(qda.class, test.data$call)
mean(qda.class == test.data$call)

################################
# Linear Discriminant Analysis #
################################

lda.fit = lda(call~ethnicity+experience+special+reqeduc+honors+reqcomp+industry, data = train.data) 
lda.fit
plot(lda.fit)
lda.class = predict(lda.fit, test.data)$class
table(lda.class, test.data$call)
mean(lda.class == test.data$call)

########################
# K-Nearest Neighbors ##
########################

train.X = data[train,]
test.X = data[-train,]
y.train = train.data$call
set.seed(1)
knn.pred = knn(train.X, test.X, y.train, k = 1)
table(knn.pred, test.data$call)
mean(knn.pred == test.data$call)

knn.pred = knn(train.X, test.X, y.train, k = 5)
table(knn.pred, test.data$call)
mean(knn.pred==test.data$call)

#########################
# Best Subset Selection #
#########################

regfit.full = regsubsets(call~ethnicity+experience+special+reqeduc+honors+reqcomp+industry, data = Names, nvmax = 20)
reg.summary = summary(regfit.full)
reg.summary

names(reg.summary)
reg.summary$rsq
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab="Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
which.max(reg.summary$adjr2)
points(9, reg.summary$adjr2[9], col = "purple", cex = 2, pch = 20)
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp",type='l')
which.min(reg.summary$cp)
points(6, reg.summary$cp[6], col = "purple", cex = 2, pch = 20)
which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = 'l')
points(3, reg.summary$bic[3], col = "purple", cex = 2, pch = 20)

############################
# Ridge Regression & Lasso #
############################

x = model.matrix(call~ethnicity+experience+special+reqeduc+honors+reqcomp+industry, Names)[,-1]
y = Names$call
dim(x)

grid = 10^seq(10, -2, length = 100) #grid of lambda
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid, family = "binomial")

y.test = test.data$call
y = Names$call
y = as.factor(as.character(y))
par(mfrow = c(2,2))

# Ridge Regress

cv.out = cv.glmnet(x[train,], y[train], alpha = 0, family = "binomial")
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
ridge.pred = predict(ridge.mod, s = bestlam, newx = x[test,], type = "class")
table(ridge.pred, y.test)
mean(ridge.pred == y.test)

out = glmnet(x, y, alpha = 0, lambda = grid, family = "binomial")
ridge.coef = predict(out, type = "coefficients", s = bestlam)[1:9,]
ridge.coef
ridge.coef[ridge.coef!=0]

# LASSO

cv.out = cv.glmnet(x[train,], y[train], alpha = 1, family = "binomial")
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
lasso.pred = predict(ridge.mod, s = bestlam, newx = x[test,], type = "class")
table(lasso.pred, y.test)
mean(lasso.pred == y.test)

out = glmnet(x, y, alpha = 1, lambda = grid, family = "binomial")
lasso.coef = predict(out, type = "coefficients", s = bestlam)[1:9,]
lasso.coef
lasso.coef[lasso.coef!=0]
