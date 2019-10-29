library(data.table)
library(readxl)
library(dplyr)
library(corrplot)
library(ggplot2)
library(ggthemes)
library(ggfortify)
library(RColorBrewer)
library(formattable)
library(readxl)
library(MASS)

theme_set(theme_light())

# Theme Overrides
theme_update(axis.text.x = element_text(size = 10),
             axis.text.y = element_text(size = 10),
             plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "darkgreen"),
             axis.title = element_text(face = "bold", size = 12, colour = "steelblue4"),
             plot.subtitle = element_text(face = "bold", size = 8, colour = "darkred"),
             legend.title = element_text(size = 12, color = "darkred", face = "bold"),
             legend.position = "right", legend.title.align=0.5)

path.data <- "D:/Projects/MSDS-Unsupervised-Learning/datasets"

setwd(path.data)

#############
##### MDS
#############

recidivism <- read.csv("recidivism.csv")

ggplot(recidivism, aes(durat, y = ..density..)) +
  geom_histogram(aes(fill = ..count..)) +
  geom_density(col = "red", lwd = 1) +
  labs(title = "Duration Until Return")

ggscatmat(recidivism)

ggplot(recidivism, aes(tserved, durat)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(recidivism, aes(y = tserved, group = black)) +
  geom_boxplot()

ggplot(recidivism, aes(y = durat, group = black)) +
  geom_boxplot()

ggplot(recidivism, aes(y = tserved, group = alcohol)) +
  geom_boxplot()

ggplot(recidivism, aes(y = durat, group = alcohol)) +
  geom_boxplot()

ggplot(recidivism, aes(y = durat, group = drugs)) +
  geom_boxplot(aes(fill = drugs)) +
  facet_grid(. ~ alcohol) +
  labs(title = "Duration by Drugs / Alcohol Use") +
  theme(legend.position = "top")

ggplot(recidivism, aes(y = durat, group = property)) +
  geom_boxplot(aes(fill = property)) +
  facet_grid(. ~ person) +
  labs(title = "Duration by Person / Property Crimes") +
  theme(legend.position = "top")

ggplot(recidivism, aes(y = durat, group = felon)) +
  geom_boxplot(aes(fill = felon)) +
  facet_grid(. ~ workprg ) +
  labs(title = "Duration by Felony / Work Program") +
  theme(legend.position = "top")

p1 <- ggplot(recidivism, aes(durat, rules)) +
  geom_point() + 
  geom_smooth()

p2 <- ggplot(recidivism, aes(durat, priors)) +
  geom_point() + 
  geom_smooth()

p3 <- ggplot(recidivism, aes(durat, educ)) +
  geom_point() + 
  geom_smooth()

grid.arrange(p1, p2, p3, nrow = 3)


p1 <- ggplot(recidivism, aes(durat, age)) +
  geom_point() + 
  geom_smooth()

p2 <- ggplot(recidivism, aes(durat, tserved)) +
  geom_point() + 
  geom_smooth()

p3 <- ggplot(recidivism, aes(durat, follow)) +
  geom_point() + 
  geom_smooth()

grid.arrange(p1, p2, p3, nrow = 3)

# 2.)

recid.dist <- dist(recidivism)

fit <- cmdscale(recid.dist, eig=TRUE, k=2) # k is the number of dim
fit # view results

par(mfrow = c(1,1))

ggplot(data.table(x = fit$points[, 1], y = fit$points[, 2]), aes(x, y)) +
  geom_point(aes(color = recidivism$durat)) +
  geom_hline(yintercept = 0, col = "darkred") +
  geom_vline(xintercept = 0, col = "darkred") +
  labs(title = "Duration (months) by Dimension", subtitle = "Metric MDS", 
       x = "Dimension 1", y = "Dimension 2", color = "Duration")

fit2 <- isoMDS(recid.dist, k=2) # k is the number of dim
fit2 # view results

ggplot(data.table(x = fit2$points[, 1], y = fit2$points[, 2]), aes(x, y)) +
  geom_point(aes(color = recidivism$durat)) +
  geom_hline(yintercept = 0, col = "darkred") +
  geom_vline(xintercept = 0, col = "darkred") +
  labs(title = "Duration (months) by Dimension", subtitle = "Nonmetric MDS",
       x = "Dimension 1", y = "Dimension 2", color = "Duration")

#############
##### Self-Organizing Maps
#############

data.college <- read.csv("college_acceptance.csv")

ggplot(data.college, aes(rank, y = ..density..)) +
  geom_histogram(aes(fill = ..count..)) +
  geom_density(col = "red", lwd = 1) +
  labs(title = "College Ranks")
