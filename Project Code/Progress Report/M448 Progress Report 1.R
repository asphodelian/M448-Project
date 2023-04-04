##########################################
#Course Project: Reading & Analyzing Data#
##########################################

#calling libraries
library(caret)
library(ggplot2)

#calling dataset
Rnames = read.csv("Resume Names.csv",header=T,na.strings="?",stringsAsFactors = T)
dim(Rnames)
names(Rnames)

#plots
plot(Rnames$call,Rnames$experience)
pie(table(Rnames$industry))
pie(table(Rnames$ethnicity))

colors = c("red", "green")
pie(table(Rnames$call),col = colors)

