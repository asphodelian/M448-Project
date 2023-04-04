#############################
#  Course Project: GLM fit  #
#############################

### Calling Libraries ###
library(caret)
library(ggplot2)

### Dataset Prep ###

Rnames = read.csv("Resume Names.csv", header=T, na.strings="?", stringsAsFactors = T)
dim(Rnames) 
names(Rnames)
summary(Rnames)

# Down sampled data
NewNames <- downSample(Rnames, Rnames$call)
summary(NewNames)
names(NewNames)

# Updating data set
Names <- subset(NewNames, select = -c(X, name, city, wanted, industry, Class))
names(Names)
summary(Names)
sum(is.na(Names)) #checking for missing values

# Splitting the data
train = sample(1:nrow(Names),0.8*nrow(Names))
test = -train
train.data = Names[train,]
test.data = Names[-train,]

### glm.fits ###

# Fit 0
glm.fit = glm(call~., train.data, family = "binomial")
summary(glm.fit)

# Fit 1
glm.fit1 = glm(call~ethnicity+experience+special+reqeduc, train.data, family = "binomial")
summary(glm.fit1)
glm.probs = predict(glm.fit1,test.data,type="response")
glm.pred = rep("no",nrow(test.data))
glm.pred[glm.probs > 0.5] = "yes"
table(glm.pred,test.data$call)

# Fit 2
glm.fit2 = glm(call~ethnicity+experience+honors+holes+special+reqorg, train.data, family = "binomial")
summary(glm.fit2)
glm.probs1 = predict(glm.fit2,test.data,type="response")
glm.pred1 = rep("no",nrow(test.data))
glm.pred1[glm.probs1 > 0.5] = "yes"
table(glm.pred1,test.data$call)

# Fit 3
glm.fit3 = glm(call~ethnicity+experience+special+college+reqeduc+reqorg, train.data, family = "binomial")
summary(glm.fit3)
glm.probs2 = predict(glm.fit3,test.data,type="response")
glm.pred2 = rep("no",nrow(test.data))
glm.pred2[glm.probs2 > 0.5] = "yes"
table(glm.pred2,test.data$call)

# Fit 4
glm.fit4 = glm(call~ethnicity+quality+jobs+honors+volunteer+holes+school+special+reqeduc+reqcomp, train.data, family = "binomial")
summary(glm.fit4)
glm.probs3 = predict(glm.fit4,test.data,type="response")
glm.pred3 = rep("no",nrow(test.data))
glm.pred3[glm.probs3 > 0.5] = "yes"
table(glm.pred3,test.data$call)

# Fit 5
glm.fit5 = glm(call~ethnicity+experience+special+reqeduc+honors+reqcomp, train.data, family = "binomial")
summary(glm.fit5)
glm.probs4 = predict(glm.fit5,test.data,type="response")
glm.pred4 = rep("no",nrow(test.data))
glm.pred4[glm.probs4 > 0.5] = "yes"
table(glm.pred4,test.data$call)

# Fit 6: checking other variables
glm.fit6= glm(call~gender+quality+jobs+volunteer+military+school+email+computer+college+minimum+equal+requirements+reqexp+reqcomm, train.data, family = "binomial")
summary(glm.fit6)

# Fit 7
glm.fit7 = glm(call~ethnicity+experience+special+reqeduc+honors+reqcomp+requirements, train.data, family = "binomial")
summary(glm.fit7)
glm.probs6 = predict(glm.fit7,test.data,type="response")
glm.pred6 = rep("no",nrow(test.data))
glm.pred6[glm.probs4 > 0.5] = "yes"
table(glm.pred6,test.data$call)

# Fit 8
glm.fit8 = glm(call~ethnicity+experience+special+reqeduc+honors, train.data, family = "binomial")
summary(glm.fit8)
glm.probs7 = predict(glm.fit8,test.data,type="response")
glm.pred7 = rep("no",nrow(test.data))
glm.pred7[glm.probs7 > 0.5] = "yes"
table(glm.pred7,test.data$call)

# Fit 9
glm.fit9 = glm(call~ethnicity+special+reqeduc+honors, train.data, family = "binomial")
summary(glm.fit9)
glm.probs8 = predict(glm.fit9,test.data,type="response")
glm.pred8 = rep("no",nrow(test.data))
glm.pred8[glm.probs7 > 0.5] = "yes"
table(glm.pred8,test.data$call)

# Fit 10
glm.fit10 = glm(call~ethnicity+experience+honors+holes+special+reqorg+gender+college, train.data, family = "binomial")
summary(glm.fit10)
glm.probs9 = predict(glm.fit10,test.data,type="response")
glm.pred9 = rep("no",nrow(test.data))
glm.pred9[glm.probs9 > 0.5] = "yes"
table(glm.pred9,test.data$call)

# Fit 11
glm.fit11 = glm(call~ethnicity+experience+honors+holes+special+reqorg+gender+college+school, train.data, family = "binomial")
summary(glm.fit11)
glm.probs10 = predict(glm.fit11,test.data,type="response")
glm.pred10 = rep("no",nrow(test.data))
glm.pred10[glm.probs10 > 0.5] = "yes"
table(glm.pred10,test.data$call)

# Fit 12
glm.fit12 = glm(call~ethnicity+experience+honors+holes+special+reqorg+gender+college+school+computer, train.data, family = "binomial")
summary(glm.fit12)
glm.probs11 = predict(glm.fit12,test.data,type="response")
glm.pred11 = rep("no",nrow(test.data))
glm.pred11[glm.probs11 > 0.5] = "yes"
table(glm.pred11,test.data$call)

# Means of Fit
mean(glm.pred == test.data$call)
mean(glm.pred1 == test.data$call)
mean(glm.pred2 == test.data$call)
mean(glm.pred3 == test.data$call)
mean(glm.pred4 == test.data$call)
mean(glm.pred6 == test.data$call)
mean(glm.pred7 == test.data$call)
mean(glm.pred8 == test.data$call)
mean(glm.pred9 == test.data$call)
mean(glm.pred10 == test.data$call)
mean(glm.pred11 == test.data$call)