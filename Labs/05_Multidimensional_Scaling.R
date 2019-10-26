
mydata<-data.frame(FloridaLakes)
x1<-mydata$ID
x2<-mydata$Lake
x3<-mydata$Alkalinity
x4<-mydata$pH
x5<-mydata$Calcium
x6<-mydata$Chlorophyll
x7<-mydata$AvgMercury
x8<-mydata$NumSamples
x9<-mydata$MinMercury
x10<-mydata$MaxMercury
x11<-mydata$ThreeYrStdMercury

mydata1<-cbind.data.frame(x2,x3,x4,x5,x6,x7,x9,x10,x11)  
.rowNamesDF(mydata1,make.names=TRUE) <-mydata1$x2
row.names(mydata1)


# Classical MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name

d1 <- dist(mydata1) # euclidean distances between the rows
d1
fit <- cmdscale(d1, eig=TRUE, k=2) # k is the number of dim
fit # view results

# plot solution
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Dimension 1", ylab="Dimension 2",
     main="Metric MDS", type="n")
text(x, y, labels = row.names(mydata1), cex=.7)



# Nonmetric MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name

library(MASS)
d2 <- dist(mydata1) # euclidean distances between the rows
fit <- isoMDS(d2, k=2) # k is the number of dim
fit # view results

# plot solution
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Dimension 1", ylab="Dimension 2",
     main="Nonmetric MDS", type="n")
text(x, y, labels = row.names(mydata1), cex=.7)