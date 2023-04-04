####################################
# Course Project: Applying Methods #
####################################

# calling libraries
library(caret)
library(ggplot2)
library(stats)
library(MASS) # QDA & LDA
library(class) # KNN
library(leaps) # best subset
library(glmnet) # ridge regress

# checking dataset
Rnames = read.csv("Resume Names.csv", header=T, na.strings="?", stringsAsFactors = T)
dim(Rnames)
names(Rnames)
summary(Rnames)

# new dataset
NewNames <- downSample(Rnames, Rnames$call)
summary(NewNames)
names(NewNames)

# updating dataset
Names <- subset(NewNames, select = -c(X, name, city, wanted, industry, Class))
names(Names)
summary(Names)
sum(is.na(Names)) #checking for missing values

#training & test data
train = sample(1:nrow(Names),0.8*nrow(Names))
test = -train
train.data = Names[train,]
test.data = Names[-train,]
data = model.matrix(call~ethnicity+experience+honors+holes+special+reqorg+gender+college,Names)[,-1]

#glm.fit
glm.fit = glm(call~., train.data, family = "binomial")
summary(glm.fit1)

glm.fit4 = glm(call~ethnicity+quality+jobs+honors+volunteer+holes+school+special+reqeduc+reqcomp, train.data, family = "binomial")
summary(glm.fit4)
glm.probs3 = predict(glm.fit4,test.data,type="response")
glm.pred3 = rep("no",nrow(test.data))
glm.pred3[glm.probs3 > 0.5] = "yes"
table(glm.pred3,test.data$call)
mean(glm.pred3 == test.data$call)

###################################
# Quadratic Discriminant Analysis #
###################################

qda.fit = qda(call~ethnicity+quality+jobs+honors+volunteer+holes+school+special+reqeduc+reqcomp, data =  train.data)
qda.fit
qda.class = predict(qda.fit, test.data)$class
table(qda.class, test.data$call)
mean(qda.class == test.data$call)

################################
# Linear Discriminant Analysis #
################################

lda.fit = lda(call~ethnicity+quality+jobs+honors+volunteer+holes+school+special+reqeduc+reqcomp, data = train.data) 
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

regfit.full = regsubsets(call~ethnicity+quality+jobs+honors+volunteer+holes+school+special+reqeduc+reqcomp, data = Names, nvmax = 20)
reg.summary = summary(regfit.full)
reg.summary

names(reg.summary)
reg.summary$rsq
par(mfrow = c(2,2))

plot(reg.summary$rss, xlab="Number of Variables", ylab = "RSS", type = "l")

plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
which.max(reg.summary$adjr2)
points(6, reg.summary$adjr2[6], col = "purple", cex = 2, pch = 20)

plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp",type='l')
which.min(reg.summary$cp)
points(6, reg.summary$cp[6], col = "purple", cex = 2, pch = 20)

which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = 'l')
points(3, reg.summary$bic[3], col = "purple", cex = 2, pch = 20)

############################
# Ridge Regression & Lasso #
############################

x = model.matrix(call~ethnicity+quality+jobs+honors+volunteer+holes+school+special+reqeduc+reqcomp, Names)[,-1]
y = Names$call
dim(x)

grid = 10^seq(10, -2, length = 100) #grid of lambda
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid, family = "binomial")

y.test = test.data$call
y = Names$call
y = as.factor(as.character(y))
par(mfrow = c(2,2))

#ridge
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

#lasso

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

#refit on the full data set, using the "best" lambda
out = glmnet(x, y, alpha = 0, family = "binomial")
predict(out, type = "coefficients", s = bestlam)[1:9,]

