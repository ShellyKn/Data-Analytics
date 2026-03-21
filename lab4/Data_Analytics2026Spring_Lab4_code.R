##########################################
### Principal Component Analysis (PCA) ###
##########################################

## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)

## set working directory so that files can be referenced without the full path
setwd("/Users/Shanelle/School/Spring26/Data_analytics/lab4/")

## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)
wine$Type <- as.factor(wine$Type)


## visualize variables
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)
ggpairs(wine, ggplot2::aes(colour = Type))

###

X <- wine[,-1]
Y <- wine$Type

###
pca_result_exploratory <- princomp(X, cor = TRUE)
autoplot(pca_result_exploratory, data = wine, colour = 'Type', main = "PCA of Wine Dataset")

pc1_loadings <- abs(pca_result_exploratory$loadings[, 1])
sorted_contributors <- sort(pc1_loadings, decreasing=TRUE)
print("Top variables contributing to the 1st PC:")
print(head(sorted_contributors, 4))

set.seed(123)
sample_index <- sample(seq_len(nrow(wine)), size = 0.8 * nrow(wine))
train_Y <- Y[sample_index]
test_Y <- Y[-sample_index]

X_scaled <- scale(X)
subset_vars <- c("Flavanoids", "Hue", "Color Intensity", "Nonflavanoid Phenols")

train_X_sub <- X_scaled[sample_index, subset_vars]
test_X_sub <- X_scaled[-sample_index, subset_vars]

knn_model_original <- knn(train = train_X_sub, test = test_X_sub, cl = train_Y, k = 5)

# Compute PCA
train_X <- X[sample_index, ]
test_X <- X[-sample_index, ]
pca_result_train <- princomp(train_X, cor=TRUE)
train_X_pc <- pca_result_train$scores[, 1:2]
test_X_pc <- predict(pca_result_train, newdata = test_X)[, 1:2]
knn_model_pcs <- knn(train = train_X_pc, test = test_X_pc, cl = train_Y, k = 5)

calc_metrics <- function(conf_matrix) {
  n_classes <- nrow(conf_matrix)
  precision <- numeric(n_classes)
  recall <- numeric(n_classes)
  f1 <- numeric(n_classes)
  
  for(i in 1:n_classes) {
    tp <- conf_matrix[i, i]
    fp <- sum(conf_matrix[i, ]) - tp
    fn <- sum(conf_matrix[, i]) - tp
    
    precision[i] <- ifelse((tp + fp) == 0, 0, tp / (tp + fp))
    recall[i] <- ifelse((tp + fn) == 0, 0, tp / (tp + fn))
    f1[i] <- ifelse((precision[i] + recall[i]) == 0, 0, 2 * (precision[i] * recall[i]) / (precision[i] + recall[i]))
  }
  
  return(c(Precision = mean(precision), Recall = mean(recall),F1 = mean(f1)))
}

conf_matrix_orig <- table(Predicted=knn_model_original, Actual = test_Y)
conf_matrix_pcs <- table(Predicted=knn_model_pcs, Actual=test_Y)

# Model 1 Results
cat("\nModel 1: Original Subset\n")
print(conf_matrix_orig)
print(calc_metrics(conf_matrix_orig))

# Model 2 Results
cat("\nModel 2: First 2 PCs\n")
print(conf_matrix_pcs)
print(calc_metrics(conf_matrix_pcs))

