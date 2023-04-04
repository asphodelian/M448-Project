############################################
# Course Project: Reading & Analyzing Data #
############################################

#calling libraries
library(caret)
library(ggplot2)

#calling dataset
Rnames = read.csv("Resume Names.csv", header=T, na.strings="?", stringsAsFactors = T)
dim(Rnames)
names(Rnames)
summary(Rnames)

#industry variable
x <- table(Rnames$industry)
labels <- c("Business/Personal Services", "Finance/Insurance/Real Estate", "Health/Educational/Social Services", "Manufacturing", "Trade", "Transport/Communication", "Unknown")
piepercent<- round(100*x/sum(x), 1)
colors = c("slateblue3", "royalblue", "goldenrod", "red3", "green4", "lightcoral", "oldlace")
pie(x, labels = piepercent, col = colors)
legend("topright", labels, cex = 0.7, fill = colors)

#wanted variablex <- table(Rnames$wanted)
labels <- c("Manager", "Office Support", "Other", "Retail Sales", "Secretary", "Supervisor")
piepercent<- round(100*x/sum(x), 1)
colors = c("seagreen4", "sienna4", "seashell1", "slateblue3", "palevioletred2", "orangered2")
pie(x, labels = piepercent, col = colors)
legend("topright", labels, cex = 1, fill = colors)


#ethnicity variable
colors = c("turquoise4", "orange")
ethnic <- c("African-American", "Caucasian")
pie(table(Rnames$ethnicity), col = colors, labels = ethnic)

#call variable
x <- table(Rnames$call)
labels <- c("No", "Yes")
piepercent<- round(100*x/sum(x), 1)
colors = c("firebrick3", "forestgreen")
pie(x,label = x, col = colors)
legend("topright", labels, cex = 1.5, fill = colors)

#gender
x <- table(Rnames$gender)
labels <- c("Female", "Male")
colors = c("violetred2", "steelblue3")
pie(x,label = x, col = colors)
legend("topright", labels, cex = 1.5, fill = colors)

# new dataset
NewNames <- downSample(Rnames, Rnames$call)
summary(NewNames)
names(NewNames)

# updating dataset
Names <- subset(NewNames, select = -c(X, name, city, Class))
names(Names)
summary(Names)

#################
# Names visuals #
#################

#call variable
x <- table(Names$call)
labels <- c("No", "Yes")
piepercent<- round(100*x/sum(x), 1)
colors = c("firebrick3", "forestgreen")
pie(x,label = labels, col = colors)
legend("topright", labels, cex = 1.5, fill = colors)

# plots
ggplot(Rnames, aes(x = call, fill = industry)) + geom_bar(position = "fill")
ggplot(Rnames, aes(x = call, fill = wanted)) + geom_bar(position = "fill")
ggplot(Rnames, aes(x = industry, fill = wanted)) + geom_bar(position = "fill")
ggplot(Rnames, aes(x = wanted, fill = industry)) + geom_bar(position = "fill")

ggplot(Names, aes(x = call, fill = industry)) + geom_bar(position = "fill")
ggplot(Names, aes(x = call, fill = wanted)) + geom_bar(position = "fill")
ggplot(Names, aes(x = industry, fill = wanted)) + geom_bar(position = "fill")
ggplot(Names, aes(x = wanted, fill = industry)) + geom_bar(position = "fill")
