# 1. LOADING LIBRARIES
library(ISLR)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)

# 2. IMPORTING DATA
setwd('C:/Users/LENOVO/Documents/year3/ML Projects')
data <- read.csv('EWCS_2016.csv')
dim(data)
# 7813   11
summary(data)
# 3. DATA CLEANING
# Removing errors
data[,][data[, ,] == -999] = NA
cond=complete.cases(data)
data = data[cond,]

summary(data)

# 4. PCA
# Check the mean and variance of variables
apply(data, 2, mean)
apply(data, 2, var)

# Step 1: Scale data
data1 = scale(data)

# Step 2: Perform PCA
pr.out <- prcomp(data1, scale=TRUE)

pr.out$rotation = - pr.out$rotation

biplot(pr.out, scale=0)
par(mar=c(1,1,1,1))

# Total variance explained by all PCs
pr.out$sdev
pr.var <- pr.out$sdev^2
pr.var
pve <- pr.var / sum(pr.var)
pve

# Plot the PVE and cumulative PVE
par(mar = c(5,5,3,1))

par(mfrow = c(1,2))
plot(pve, xlab = 'Principle Component',
          ylab = 'Proportion of Variance explained', ylim = c(0,1),
          type = 'b')
plot(cumsum(pve), xlab = 'Principle Component',
                  ylab = 'Cumulative Proportion of Variance explained',
                  ylim = c(0,1), type = 'b')
plot(pr.out, type = 'l')
biplot(pr.out, scale=0)
# Matrix completion
X <- data.matrix(data1)
pcob <- prcomp(X)
summary(pcob)

# Extract PC scores

data2 <- data.frame(cbind(data1, pr.out$x[,1:2]))
head(data2)

# Plot with ggplot
ggplot(data2, aes(PC1, PC2, fill=Q90a))+
    stat_ellipse(geom = 'polygon', col = 'black', alpha = 0.5)+
    geom_point(shape = 21, col = 'black')
head(data2)

biplot(pr.out, data = data,
       loadings = TRUE, col = c('blue', 'red'),
       loadings.label = TRUE, loadings.label.size = 3)

# 5. CLUSTERING
# Step 1: Determine k-value using different methods
fviz_nbclust(data2, kmeans, method = 'wss')
fviz_nbclust(data2, kmeans, method = 'silhouette')

## k=2

# Step 2: Cluster the data 
k = 2
kmeans_data = kmeans(data2, centers = k, nstart = 50)
fviz_cluster(kmeans_data, data = data2)

# Identify number of obs in clusters
k1.clusters = kmeans_data$cluster
table(k1.clusters)

sub.group = k1.clusters
data %>%
  mutate(Cluster = sub.group) %>%
  group_by(Cluster) %>%
  summarise_all("mean") %>%
  data.frame

# Create a new dataframe with the cluster variable
data_results = data.frame(data$Q2a, data$Q2b, data$Q87a, data$Q87b, 
                          data$Q87c, data$Q87d, data$Q87e, data$Q90a, data$Q90b,
                          data$Q90c, data$Q90f, kmeans_data$cluster)

# Create subsets for clusters
cluster1 = subset(data_results, kmeans_data$cluster == 1)
cluster2 = subset(data_results, kmeans_data$cluster == 2)

# Perform Chi-square tests for Q90f 
# Q90f
M = as.matrix(table(cluster1$data.Q90f))
p.null = as.vector(prop.table(table(cluster2$data.Q90f)))
chisq.test(M, p=p.null)
##data:  M
##X-squared = 1655.7, df = 4, p-value < 2.2e-16














