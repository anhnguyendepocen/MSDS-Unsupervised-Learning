library(data.table)
library(readxl)
library(dplyr)
library(corrplot)
library(ggplot2)
library(ggthemes)
library(ggfortify)
library(RColorBrewer)
library(tidyverse)
library(Rtsne)

theme_set(theme_light())

# Theme Overrides
theme_update(plot.title = element_text(hjust = 0.5),
             axis.text.x = element_text(size = 10),
             axis.text.y = element_text(size = 10),
             axis.title = element_text(face = "bold", size = 12, colour = "steelblue4"),
             legend.position = "top", legend.title = element_blank())

path.data <- "D:/Projects/MSDS-Unsupervised-Learning/datasets"
setwd(path.data)

###########################
# Data load and prep
###########################

tsne_data <- as.data.table(read_csv("bodyfat.csv"))[, c("Age", "Density") := NULL]

head(tsne_data)


###########################
# Exploratory Data Analysis
# Data Prep
###########################

# review range of variables and ensure no N/As exist
summary(tsne_data)

# using rtsne
set.seed(1) # for reproducibility
tsne <- Rtsne(tsne_data, dims = 2, perplexity=30, verbose=TRUE, max_iter = 5000, learning = 200)

# visualizing
colors = rainbow(length(unique(tsne_data$agegroup)))
names(colors) = unique(tsne_data$agegroup)

par(mgp=c(2.5,1,0))

plot(tsne$Y, t='n', main="tSNE", xlab="tSNE dimension 1", ylab="tSNE dimension 2", cex.main=2, cex.lab=1.5)
text(tsne$Y, labels = tsne_data$agegroup, col = colors[tsne_data$agegroup])

tsne.results <- data.table(x = tsne$Y[,1], y = tsne$Y[,2], group = tsne_data$agegroup)

ggplot(tsne.results, aes(x, y, label = group, color = group)) +
  geom_text()

# train and plot using different parameters
tsne_plot <- function(perpl=30,iterations=500,learning=200){
  set.seed(1) # for reproducibility
  tsne <- Rtsne(tsne_data %>% select(-Density, -agegroup), dims = 2, perplexity=perpl, verbose=TRUE, max_iter=iterations, eta=learning)
  plot(tsne$Y, t='n', main = print(paste0("perplexity = ",perpl, ", max_iter = ",iterations, ", learning rate = ",learning)), xlab="tSNE dimension 1", ylab="tSNE dimension 2", cex.main=1, cex.lab=1.5)
  text(tsne$Y, labels = tsne_data$agegroup, col = colors[tsne_data$agegroup])
}

perplexity_values <- c(2,5,30,50,80)
sapply(perplexity_values, function(i){tsne_plot(perpl=i)})

learning_values <- c(20,200,2000)
sapply(learning_values,function(i){tsne_plot(learning=i)})


