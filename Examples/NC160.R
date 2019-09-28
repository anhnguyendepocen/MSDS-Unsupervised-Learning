library(ISLR)

nci.labs <- NCI60$labs
nci.data <- NCI60$data

dim(nci.data)

nci.labs[1:4]

table(nci.labs)

pr.out <- prcomp(nci.data, scale = T)

Cols <- function(vec) {
    cols = rainbow(length(unique(vec)))
    return(cols[as.numeric(as.factor(vec))])
}

par(mfrow = c(1,2))

plot(pr.out$x[, 1:2], 
     col = Cols(nci.labs),
     pch = 19,
     xlab = "Z1",
     ylab = "Z2")

plot(pr.out$x[, c(1,3)], 
     col = Cols(nci.labs),
     pch = 19,
     xlab = "Z1",
     ylab = "Z3")

summary(pr.out)

plot(pr.out)

# PCA Analysis

pve <- 100 * pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow = c(1,2))
plot(pve,
     type = "o",
     ylab = "PVE",
     xlab = "Principal Component",
     col = "blue")
plot(cumsum(pve),
     type = "o",
     ylab = "Cumulative PVE",
     xlab = "Principal Component",
     col = "brown3")

# Hierarchial Clustering

sd.data <- scale(nci.data)
data.dist <- dist(sd.data)

par(mfrow = c(1,3))

plot(hclust(data.dist, method = "complete"), 
     main = "Complete Linkage", 
     xlab = "", 
     sub = "")

plot(hclust(data.dist, method = "average"), 
     main = "Average Linkage", 
     xlab = "", 
     sub = "")

plot(hclust(data.dist, method = "single"), 
     main = "Single Linkage", 
     xlab = "", 
     sub = "")

hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters, nci.labs)

hc.out

set.seed(2)

km.out <- kmeans(sd.data, 4, nstart = 20)
km.clusters <- km.out$cluster
table(km.clusters, hc.clusters)

par(mfrow = c(1,1))
hc.out <- hclust(dist(pr.out$x[,1:5]))
plot(hc.out,
     labels = nci.labs,
     main = "Heir. Clust. on First Five Score Vectors")
table(cutree(hc.out, 4), nci.labs)

