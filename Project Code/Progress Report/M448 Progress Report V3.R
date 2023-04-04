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
x <- table(Rnames$industry)
labels <- c("Business/Personal Services", "Finance/Insurance/Real Estate", "Health/Educational/Social Services", "Manufacturing", "Trade", "Transport/Communication", "Unknown")
colors = c("slateblue3", "royalblue", "goldenrod", "red3", "green4", "lightcoral", "oldlace")
pie(x, labels = x, col = colors)
legend("topright", labels, cex = 0.8, fill = colors)
#ethnicity variable
colors = c("turquoise4", "orange")
ethnic <- c("African-American", "Caucasian")
pie(table(Rnames$ethnicity), col = colors, labels = ethnic)
#call variable
colors = c("red", "green")
pie(table(Rnames$call),col = colors)
#gender
colors = c("violetred2", "steelblue3")
pie(table(Rnames$gender),col = colors)

#downsample in callback
Rnames <- downSample(Rnames, Rnames$call)
Rnames = subset(Rnames, select = -c(X, name))
summary(Rnames)
colors = c("violetred", "slateblue2")
pie(table(Rnames$call),col = colors)
colors = c("springgreen3", "orange")
pie(table(Rnames$ethnicity),col = colors)

#plots w/ x = call
ggplot(Rnames, aes(x = call, fill = ethnicity)) + geom_bar(position = "fill")
ggplot(Rnames, aes(x = call, fill = gender)) + geom_bar(position = "fill")
#gender variable
colors = c("mediumvioletred", "midnightblue")
pie(table(Rnames$gender),col = colors)
#plots w/ x = equal
ggplot(Rnames, aes(x = equal, fill = industry)) + geom_bar(position = "fill")
ggplot(Rnames, aes(x = equal, fill = call)) + geom_bar(position = "fill")
ggplot(Rnames, aes(x = equal, fill = gender)) + geom_bar(position = "fill")
ggplot(Rnames, aes(x = equal, fill = ethnicity)) + geom_bar(position = "fill")
#pairs
Names = subset(Rnames, select = -c(X, name, city, holes, email, equal, wanted, industry))
pairs(Names)
Req = subset(Rnames, select = -c(quality,city,jobs,honors,military,holes,email,minimum,equal,wanted,requirements,industry))
pairs(Req)

#misc plots
plot(Rnames$call,Rnames$experience)
ggplot(Rnames, aes(x = experience, fill = reqexp)) + geom_bar(position = "fill")
attach(Rnames)
plot(experience, call, main="Scatterplot Example",
     xlab="Experience ", ylab="Call", pch=19)
