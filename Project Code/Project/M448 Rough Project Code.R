####################################
# Course Project: Applying Methods #
####################################

# classification problem
# see KNN, (multiple) logistic regression, LDA

# calling libraries
library(caret)
library(ggplot2)
library(MASS) #QDA
library(leaps) #best subset
library(glmnet) #ridge regress
library(stats)

# calling dataset
#setwd("/Users/gsal/Downloads")
View(Rnames)
dim(Rnames)
names(Rnames)
summary(Rnames)

# tweaking dataset
Names <- downSample(Rnames, Rnames$call)
summary(Names)
names(Names)
View(Names)

# removing variables from dataset
NewNames <- subset(Names, select = -c(X, name, city, wanted, industry, Class))
names(NewNames)
View(NewNames)
summary(NewNames)
# checking for missing values
sum(is.na(NewNames))

train=sample(1:nrow(NewNames),0.8*nrow(NewNames))
test=-train
train.data=NewNames[train,]
test.data=NewNames[-train,]


glm.fit=glm(call~ethnicity+experience+special+college+reqeduc+reqorg,train.data,family="binomial")
glm.probs=predict(glm.fit,test.data,type="response")
glm.pred=rep("no",nrow(test.data)) # generate a 1250-dim vector with each element as "Down", name this vector glm.pred
glm.pred[glm.probs>.5]="yes"
table(glm.pred,test.data$call)
mean(glm.pred==test.data$call)
###################################
# Quadratic Discriminant Analysis #
###################################

# needs to be edited
# ELEPHANT ELEPHANT ELEPHANT
qda.fit = qda(call~ethnicity+experience+special+college+reqeduc+reqorg, data = NewNames, subset = train)
qda.fit
qda.class = predict(qda.fit,test.data)$class
table(qda.class,test.data$call)
mean(qda.class == test.data$call)

#########################
# Best Subset Selection #
#########################


regfit.full = regsubsets(call~ethnicity+experience+special+college+reqeduc+reqorg, data = NewNames, nvmax = 20) #linear dependencies
reg.summary = summary(regfit.full)
reg.summary

names(reg.summary)
reg.summary$rsq
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(6,reg.summary$adjr2[6], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(6,reg.summary$cp[6],col="red",cex=2,pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(3,reg.summary$bic[3],col="red",cex=2,pch=20)

####################
# Ridge Regression #
####################


x = model.matrix(call~ethnicity+experience+special+college+reqeduc+reqorg, NewNames)[,-1]
y = NewNames$call
dim(x)

grid = 10^seq(10, -2, length = 100) #grid of lambda
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid, family = "binomial")

# dim(coef(ridge.mod)) #save beta.hat for the 100 lambdas.
#
# ridge.mod$lambda[50]
# coef(ridge.mod)[,50]
# sqrt(sum(coef(ridge.mod)[-1,50]^2))
# ridge.mod$lambda[60]
# coef(ridge.mod)[,60] #lambda=705
# sqrt(sum(coef(ridge.mod)[-1,60]^2))
#
# predict(ridge.mod, s = 50, type = "coefficients")[1:20,] #lambda=50
# set.seed(1)
# train = sample(1:nrow(x), nrow(x)/2)
# test = (-train)

y.test = test.data$call
y=NewNames$call
y=as.factor(as.character(y))

#ridge
cv.out=cv.glmnet(x[train,],y[train],alpha=0,family="binomial")
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,],type="class")
table(ridge.pred,y.test)
mean(ridge.pred==y.test)

#lasso
lasso.mod = glmnet(x, y, alpha = 1, lambda = grid, family = "binomial")
cv.out=cv.glmnet(x[train,],y[train],alpha=1,family="binomial")
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,],type="class")
table(lasso.pred,y.test)
mean(lasso.pred==y.test)


#ridge.mod = glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12, family = "binomial") # thresh controls coordinate descent convergence

#ridge.pred = predict(ridge.mod, s = 4, newx = x[test,],type="class")


mean((ridge.predict-y.test)^2) # test mse

#arg not numeric or logical
mean((mean(y[train])-y.test)^2) # test mse using intercept model

ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])#close to intercept fit

#NA
mean((ridge.pred-y.test)^2)

ridge.pred=predict(ridge.mod,s=0,x=x[train,],y=y[train],newx=x[test,],exact=T)#linear regression fit
mean((ridge.pred-y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod,s=0,x=x[train,],y=y[train],exact=T,type="coefficients")[1:20,]

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)

#refit on the full data set, using the "best" lambda
out = glmnet(x, y, alpha = 0, family = "binomial")
predict(out, type = "coefficients", s = bestlam)[1:20,]
