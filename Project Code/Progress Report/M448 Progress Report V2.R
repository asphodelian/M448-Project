##########################################
#Course Project: Reading & Analyzing Data#
##########################################

#calling libraries
library(caret)
library(ggplot2)

#calling dataset
Rnames = read.csv("Resume Names.csv",header=T,na.strings="?",stringsAsFactors = T)
#checking out the specs
View(Rnames)
dim(Rnames)
names(Rnames)
summary(Rnames)

#industry variable
pie(table(Rnames$industry))

#ethnicity variable
colors = c("purple", "white")
ethnic <- c("African-American", "Caucasian")
pie(table(Rnames$ethnicity), col = colors, labels = ethnic)

#call variable
colors = c("red", "green")
pie(table(Rnames$call),col = colors)

#gender variable
colors = c("mediumvioletred", "midnightblue")
pie(table(Rnames$gender),col = colors)

#plots w/ x = call
ggplot(Rnames, aes(x = call, fill = ethnicity)) + geom_bar(position = "fill")
ggplot(Rnames, aes(x = call, fill = gender)) + geom_bar(position = "fill")
ggplot(Rnames, aes(x = call, fill = industry)) + geom_bar(position = "fill")
ggplot(Rnames, aes(x = call, fill = quality)) + geom_bar(position = "fill")
#plots w/ other x
ggplot(Rnames, aes(x = ethnicity, fill = gender)) + geom_bar(position = "fill")
ggplot(Rnames, aes(x = quality, fill = holes)) + geom_bar(position = "fill")
ggplot(Rnames, aes(x = quality, fill = call)) + geom_bar(position = "fill")
#plots w/ x = equal
ggplot(Rnames, aes(x = equal, fill = industry)) + geom_bar(position = "fill")
ggplot(Rnames, aes(x = equal, fill = call)) + geom_bar(position = "fill")
ggplot(Rnames, aes(x = equal, fill = gender)) + geom_bar(position = "fill")
ggplot(Rnames, aes(x = equal, fill = ethnicity)) + geom_bar(position = "fill")
#pairs
Names = subset(Rnames, select = -c(X, name, city, holes, email, equal, wanted, industry))
pairs(Names)

#downsample in callback
Cnames <- downSample(Rnames, Rnames$call)
Cnames = subset(Rnames, select = -c(X, name))
summary(Cnames)
colors = c("violetred", "slateblue2")
pie(table(Cnames$call),col = colors)
colors = c("springgreen3", "orange")
pie(table(Cnames$ethnicity),col = colors)

#downsample in gender, call and ethnicity
Names <- downSample(Rnames, Rnames$gender)
Names = subset(Names, select = -c(X, name))
summary(Names)
Names <- downSample(Names, Names$call)
Names <- downSample(Names, Names$ethnicity)
summary(Names)
