set.seed(2)
x=matrix(rnorm(50*2),ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4
x

km.out<-kmeans(x,2,nstart=20)
km.out
km.out$cluster

plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2", xlab=" ", pch=20,cex=2)

set.seed(4)
km.out1=kmeans(x,3,nstart=20)
km.out1
plot(x, col=(km.out1$cluster+1), main="K-Means Clustering Results with K=2", xlab=" ", pch=20,cex=2)



mydata<-data.frame(HSB)

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


mydata1<-data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x10,x11,x12,x13,x14)
x<-as.matrix(mydata1)


set.seed(4)
km.out1=kmeans(x,3,nstart=50)
km.out1
cl_grp3<-km.out1$cluster
cl_grp3

x1<-x1-1
x4<-x4-1
mydata2<-data.frame(x1,x4,x6,x7,x8,x10,x11,x12,x13,x14)
x<-as.matrix(mydata2)


set.seed(5)
km.out2<-kmeans(x,3,nstart=50)
km.out2

cl_grp3<-km.out2$cluster
cl_grp3


set.seed(6)
km.out3<-kmeans(x,4,nstart=50)
km.out3
