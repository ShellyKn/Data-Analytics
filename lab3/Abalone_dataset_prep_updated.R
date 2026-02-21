################################################
#### Evaluating Classification & CLustering ####
################################################

library("caret")
library(GGally)
library(psych)
library("class")
library("cluster")

## read data
abalone <- read.csv("/Users/Shanelle/School/Spring26/Data_analytics/lab3/abalone.data", header=FALSE)

## rename columns
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' ) 

## derive age group based in number of rings
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

## take copy removing sex and rings
abalone.sub <- abalone[,c(2:8,10)]

## convert class labels to strings
abalone.sub$age.group <- as.character(abalone.sub$age.group)

## convert back to factor
abalone.sub$age.group <- as.factor(abalone.sub$age.group)

## split train/test
train.indexes <- sample(4177,0.7*4177)

train <- abalone.sub[train.indexes,]
test <- abalone.sub[-train.indexes,]

## separate x (features) & y (class labels)
X <- train[,1:7] 
Y <- train[,8]

## features subset
# train <- train[,5:8]
# test <- test[,5:8]

## feature boxplots
boxplot(X, main="abalone features")

## class label distributions
plot(Y)


## feature-class plots
featurePlot(x=X, y=Y, plot="ellipse")
featurePlot(x=X, y=Y, plot="box")

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=X, y=Y, plot="density", scales=scales)

## psych scatterplot matrix
pairs.panels(X,gap = 0,bg = c("pink", "green", "blue")[Y],pch=21)

## GGally 
ggpairs(train, ggplot2::aes(colour = Y))


# Exercise 1: 
X_train_sub1 <- scale(train[,1:3])
X_test_sub1 <- scale(test[,1:3])
X_train_sub2 <- scale(train[,4:7])
X_test_sub2 <- scale(test[,4:7])
knn_1 <- knn(train = X_train_sub1,test = X_test_sub1, cl=train$age.group, k = 7)
knn_2 <- knn(train = X_train_sub2,test = X_test_sub2, cl=train$age.group, k = 7)
table1 <- table(Predicted = knn_1, Actual=test$age.group)
table2 <- table(Predicted = knn_2, Actual=test$age.group)

cat("kNN Contingency Table: Dimensions")
print(table1)
cat("kNN Contingency Table: Weights")
print(table2)

acc1 <- sum(diag(table1)) / nrow(test)
acc2 <- sum(diag(table2)) / nrow(test)
cat("Accuracy Subset 1:", acc1)
cat("Accuracy Subset 2:", acc2)

if (acc1>acc2) {
  best_train <-X_train_sub1
  best_test <- X_test_sub1
} else {
  best_train <- X_train_sub2
  best_test <- X_test_sub2
}

k_values <- seq(1,60, by =2)
k_accuracies <- numeric(length(k_values))

for(i in seq_along(k_values)) {
  val <- k_values[i]
  pred <- knn(train = best_train, test=best_test, cl=train$age.group,k=val)
  k_accuracies[i] <- sum(diag(table(pred,test$age.group))) / nrow(test)
}

optimal_k_index <- which.max(k_accuracies)
optimal_k <- k_values[optimal_k_index]
cat("Optimal k for the better model:", optimal_k)
cat("Accuracy:", max(k_accuracies))


# Exercise 2:
library(factoextra)
if (acc1 > acc2) {
  best_cols <- 1:3
} else {
  best_cols <- 4:7
}

cluster_data <- scale(abalone.sub[, best_cols])
k.list <- c(1, 2, 3, 4, 5, 6, 7, 8)
set.seed(42)
si.list.km <- c()

for (k in k.list) {
  km_model <- kmeans(cluster_data, centers = k, nstart = 25)
  if (k > 1) {
    si <- silhouette(km_model$cluster, dist(cluster_data))
    avg.si <- mean(si[, 3])  
    si.list.km <- c(si.list.km, avg.si)
  }
}

opt_k_kmeans <- k.list[-1][which.max(si.list.km)]
cat("Optimal k for k-means:", opt_k_kmeans)

f_kmeans <- kmeans(cluster_data, centers = opt_k_kmeans, nstart = 25)

sil_km <- silhouette(f_kmeans$cluster, dist(cluster_data))
print(fviz_silhouette(sil_km) + labs(title = paste("k-Means Silhouette k =", opt_k_kmeans)))

si.list.pam <- c()
for (k in k.list) {
  if (k > 1) {
    pam_model <- pam(cluster_data, k)
    si <- silhouette(pam_model$cluster, dist(cluster_data))
    avg.si <- mean(si[, 3])  
    si.list.pam <- c(si.list.pam, avg.si)
  }
}

opt_k_pam <- k.list[-1][which.max(si.list.pam)]
cat("Optimal k for PAM:", opt_k_pam, "\n")

f_pam <- pam(cluster_data, k = opt_k_pam)
sil_pam <- silhouette(f_pam$cluster, dist(cluster_data))
print(fviz_silhouette(sil_pam) + labs(title = paste("PAM Silhouette k=", opt_k_pam)))
## EOF ##
