library(caret)
library(randomForest)
library(gbm)
library(xgboost)
library(doParallel)
registerDoParallel(4)   # Registrer a parallel backend for train
getDoParWorkers()

# Some toy data
data(Boston, package="MASS")
d <- Boston
# set target variable
# for survival analysis, requirement is that 0 = censored and 1 = death / event
d$label <- d$medv
d$medv <- NULL

# split data into train & test
set.seed(1110)
train_ind <- createDataPartition(y = d$label, p = .70, list = FALSE)
train <- d[train_ind,]
test <- d[-train_ind,]

# VALIDATE model performance using several metrics -----
# Parameters:
#   - model : an object returned by caret train() method
#   - test : test data, must contain a target / label variable that is called, unsurprinsly, label
#   - output_dir : where to save plots
validate <- function(method_name = '', model, test, output_dir) {
  suffix <- sprintf("%s_%s.pdf", Sys.Date(), method_name)
  print('Variable importance')
  print(varImp(model))

  # TODO: output variable importance to a file
  # Confusion matrix (decision threshold = 0.5)
  pred <- predict(model, test)
  print('Confusion matrix')
  print(confusionMatrix(pred, test$label))

  # ROC
  pred <- predict(model, test, type="prob")
  print('Few example predictions')
  print(head(pred))
  
  numeric_label <- ifelse(test$label == 'class1', 1, 0)
  pdf(file = sprintf("%s/roc_%s", output_dir, suffix))
  res <- roc(response = numeric_label, predictor = pred[, 'class1'],
   auc=TRUE, plot=TRUE)
  title(sprintf('AUC = %.2f', res$auc))
  print('Area under the curve: ')
  print(res$auc)
  dev.off()

  # Cumulative gains
  res <- gains(actual=numeric_label, predicted=pred[, 'class1'], optimal=TRUE)
  #plot_ly(x = res$depth, y = res$cume.pct.of.total, type='scatter', mode='lines')
  ggplot(data.frame(x = res$depth, y = res$cume.pct.of.total), aes(x, y)) +
      geom_point() + geom_line() + 
      xlab("% of test instances (decreasing in score)") +
      ylab("Cumulative positives detected")
  ggsave(sprintf("%s/cumulative_gain_%s", output_dir, suffix))
  ggplot(data.frame(x = res$depth, y = res$lift), aes(x, y)) +
    geom_point() + geom_line() + 
    xlab("% of test instances (decreasing in score)") +
    ylab("Cumulative lift")
  ggsave(sprintf("%s/cumulative_lift_%s", output_dir, suffix))
  pred
}
# END-----

# RANDOM FOREST -----
# Compare CV and OOB validation error
ctrl <- trainControl(method = "cv",   # cv / repeatedcv / oob
                     number = 2,      # 2 folds if have enough data, 10 folds if data small
                     summaryFunction = twoClassSummary, # Use AUC to pick the best model
                     classProbs = TRUE,
                     allowParallel = TRUE)
rf_model <- train(label ~ ., data = train,
                              method = "rf",
                              metric = "ROC",
                              ntree = 500,
                              trControl = ctrl,
                              verboseIter = TRUE)
# Plot mtry vs. validation error
plot(rf_model)
# Plot errors vs. number of trees
errors <- data.frame(rf_model$finalModel$err.rate)
errors$num_trees <- 1:nrow(errors)
errors <- gather(errors, what, error, -num_trees)
ggplot(errors, aes(x = num_trees, y = error, color = what)) + geom_line(aes(group = what))

# GBM -----

# Set class name for caret training
train$label <- ifelse(train$label == 1, 'class1', 'class0')
test$label <- ifelse(test$label == 1, 'class1', 'class0')

# Set up training control
ctrl <- trainControl(method = "cv",   # cv / repeatedcv / oob
                     number = 2,      # 2 folds if have enough data, 10 folds if data small
                     summaryFunction = twoClassSummary, # Use AUC to pick the best model
                     classProbs = TRUE,
                     allowParallel = TRUE)

# Use the expand.grid to specify the search space 
# Note that the default search grid selects multiple values of each tuning parameter
# Quick test
grid <- expand.grid(interaction.depth = c(2), # Depth of variable interactions
                    n.trees = c(50),         # Num trees
                    shrinkage = c(0.01),    # Learning rate
                    n.minobsinnode = 10)
# Real training
grid <- expand.grid(interaction.depth = c(1, 2, 3, 4), # Depth of variable interactions
                    n.trees = c(100, 200, 500),         # Num trees
                    shrinkage = c(0.01, 0.1),    # Learning rate
                    n.minobsinnode = 10)

# Train model
gbm_model <- train(label ~ ., data = train,
                              method = "gbm",
                              metric = "ROC",
                              trControl = ctrl,
                              tuneGrid = grid,
                              verboseIter = TRUE)
print('Best hyperparameters:')
gbm_model$bestTune
plot(gbm_model)      # Plot the performance of the training models
res <- gbm_model$results
res
validate('gbm', gbm_model, test, 'plots')

# XGBOOST -----

# See validate.R for a few useful methods for validation
