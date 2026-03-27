## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)

setwd("/Users/Shanelle/School/Spring26/Data_analytics/lab5/")

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
train_data <- data.frame(Type = train_Y, train_X_sub)
test_data <- data.frame(Type = test_Y, test_X_sub)

#Train models
set.seed(123)
tune_linear <- tune.svm(Type ~ ., data = train_data, kernel = "linear", 
                        cost = c(0.01, 0.1, 1, 10, 100))
best_svm_linear <- tune_linear$best.model
set.seed(123)
tune_radial <- tune.svm(Type ~ ., data = train_data, kernel = "radial", 
                        cost = c(0.1, 1, 10, 100), 
                        gamma = c(0.01, 0.1, 1, 10))
best_svm_radial <- tune_radial$best.model
set.seed(123)
knn_model_original <- knn(train = train_X_sub, test = test_X_sub, cl = train_Y, k = 5)

#Model Evals
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
  
  return(c(Precision = mean(precision, na.rm=TRUE), Recall = mean(recall, na.rm=TRUE), F1 = mean(f1, na.rm=TRUE)))
}

#Predictions for SVMs
pred_svm_linear <- predict(best_svm_linear, test_data)
pred_svm_radial <- predict(best_svm_radial, test_data)

#Confusion Matrices
conf_matrix_linear <- table(Predicted = pred_svm_linear, Actual = test_Y)
conf_matrix_radial <- table(Predicted = pred_svm_radial, Actual = test_Y)
conf_matrix_knn <- table(Predicted = knn_model_original, Actual = test_Y)

#Performance Comparison
cat("Model 1: SVM (Linear Kernel)\n")
print(paste("Optimal Cost:", tune_linear$best.parameters$cost))
print(conf_matrix_linear)
print(calc_metrics(conf_matrix_linear))

cat("Model 2: SVM (Radial Kernel)\n")
print(paste("Optimal Cost:", tune_radial$best.parameters$cost, 
            "| Optimal Gamma:", tune_radial$best.parameters$gamma))
print(conf_matrix_radial)
print(calc_metrics(conf_matrix_radial))

cat("Model 3: kNN (k=5)\n")
print(conf_matrix_knn)
print(calc_metrics(conf_matrix_knn))

