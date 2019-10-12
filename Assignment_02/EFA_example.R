
library(psych)
library(lessR)

setwd("D:/Projects/MSDS-Unsupervised-Learning/datasets")
HSB <- read_csv("HSB.csv")
mydata<-data.frame(HSB)

str(mydata)
head(mydata)
names(mydata)

x1<-mydata$sex
x2<-mydata$race
x3<-mydata$ses
x4<-mydata$sctyp
x5<-mydata$hsp
x6<-mydata$locus
x7<-mydata$concept
x8<-mydata$mot
x9<-mydata$car
x10<-mydata$rdg
x11<-mydata$wrtg
x12<-mydata$math 
x13<-mydata$sci
x14<-mydata$civ


mydata1<-cbind.data.frame(x6,x7,x8,x10,x11,x12,x13,x14)
str(mydata1)
head(mydata1)
names(mydata1)

hsb.cor <- cor(mydata1)
hsb.cor

Z<-eigen(hsb.cor)
Z$val
Z$vec

fa.parallel(hsb.cor, n.obs=600, fa="both", n.iter=100, show.legend=TRUE,main="Scree plot with parallel analysis")

fa1<-fa(mydata1, nfactors=3, rotate="none", fm="pa")
fa1

fa2<-fa(mydata1, nfactors=3, rotate="varimax", fm="pa")
fa2

fa3<-fa(mydata1, nfactors=2, rotate="varimax", fm="pa")
fa3

fa4<-fa(mydata1, nfactors=2, rotate="varimax", fm="ml")
fa4

fa5<-fa(mydata1, nfactors=1, rotate="varimax", fm="pa")
fa5


### Constructing the data matrix to include only the variables I want to work with. ###

mydata1<-cbind.data.frame(x6,x7,x8,x10,x11,x12,x13,x14)
x<-as.matrix(mydata1)

### Use the fa() function from the psych package to conduct a factor analysis on the x matrix just contructed. ###
### We are going to extract 3 factors, use varimax rotation, Principal Axis factoring. ###
### AND, we will obtain information for factor scores.  ###

fa2<-fa(x, nfactors=3, rotate="varimax", fm="pa", scores=TRUE)
fa2
weights(fa2)
