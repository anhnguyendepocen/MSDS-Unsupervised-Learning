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

theme_set(theme_light())

# Theme Overrides
theme_update(plot.title = element_text(hjust = 0.5),
             axis.text.x = element_text(size = 10),
             axis.text.y = element_text(size = 10),
             axis.title = element_text(face = "bold", size = 12, colour = "steelblue4"),
             legend.position = "top", legend.title = element_blank())

path.data <- "D:/Projects/MSDS-Unsupervised-Learning/datasets"

setwd(path.data)

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


