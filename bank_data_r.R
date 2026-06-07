library(tidyverse)
library(randomForest)
library(caret)
library(pROC)
library(data.table)

data <- read_csv("C:/Users/micah/Projects/Kaggle_Projects/playground-series-s5e8/train.csv")
setDT(data)

head(data)
dim(data) #(750000, 18)     

sum(is.na(data)) #No null values

data[, id := NULL] #Getting rid of id column

X <- data %>% select(-y)
Y <- data$y

X_dum <- model.matrix(~ . -1, data = X) %>% as.data.frame()

#Splitting into training, testing
set.seed(36)
train_index <- sample(seq_len(nrow(X_dum)), size = 0.80 * nrow(X_dum))

x_train <- X_dum[train_index, ]
x_test <- X_dum[-train_index, ]
y_train <- Y[train_index]
y_test <- Y[-train_index]

model_rf <- randomForest(x = x_train, y = as.factor(y_train), ntree = 100) #Need to make it a factor to run in R. 100 trees to save memory

preds <- predict(model_rf, x_test)
accuracy <- mean(preds == y_test)
print(accuracy)


Y_pred_proba <- predict(model_rf, x_test, type = "prob")[, 2]
auc_score <- roc(y_test, Y_pred_proba)$auc
cat("AUC Score:", round(auc_score, 4), "\n")


roc_object <- roc(y_test, Y_pred_proba)

ggroc(roc_object) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "ROC Curve",
    x = "False Positive Rate",
    y = "True Positive Rate",
    caption = paste("AUC =", round(auc_score, 4))
  ) +
  theme_minimal()
