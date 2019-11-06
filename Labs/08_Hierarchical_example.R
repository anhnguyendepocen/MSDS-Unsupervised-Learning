
set.seed(2)
x=matrix(rnorm(50*2),ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4
x

y<-dist(x)

hc.complete<-hclust(dist(x), method="complete")
hc.average<-hclust(dist(x), method="average")
hc.single<-hclust(dist(x), method="single")

par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab=" ",sub=" ",cex=0.9)
plot(hc.average,main="Average Linkage", xlab=" ",sub=" ",cex=0.9)
plot(hc.single,main="Single Linkage", xlab=" ",sub=" ",cex=0.9)

cutree(hc.complete, 2)

group<-cutree(hc.average, 2)
group


path.data <- "D:/Projects/MSDS-Unsupervised-Learning/datasets"

setwd(path.data)

mydata <- data.frame(read.csv("FloridaLakes.csv"))

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

mydata1<-cbind.data.frame(x3,x4,x5,x6,x7,x9,x10,x11)  

x<-as.matrix(mydata1)
y<-dist(x)
y

xsc<-scale(x)
y<-dist(xsc)

hc.complete<-hclust(y,method="complete")
hc.average<-hclust(y,method="average")
hc.single<-hclust(y,method="single")
                    

plot(hc.complete,main="Complete Linkage", xlab=" ",sub=" ", cex=0.9)
plot(hc.average,main="Average Linkage", xlab=" ",sub=" ", cex=0.9)
plot(hc.single,main="Single Linkage", xlab=" ", sub=" ",cex=0.9)

group<-cutree(hc.complete, 4)
mydata2<-cbind.data.frame(mydata1,group,x2)
mydata2
