library(caret)
library(randomForest)
library(gbm)
library(xgboost)
library(doParallel)

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

# SET UP -----
# Use parallel backend 
registerDoParallel(4)
getDoParWorkers()

# Common setting for train control for all models
ctrl <- trainControl(method = "cv",   # cv / repeatedcv / oob
                     number = 2,      # 2 folds if have enough data, 10 folds if data small
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE, # required for AUC matric
                     allowParallel = TRUE)

#-----

# VARIABLE IMPORTANCE -----
imp <- varImp(xgb_model)
plot(imp, top = 20)
importance <- imp$importance # data frame with row indices = feature names
#-----

# RANDOM FOREST -----
train$label <- ifelse(train$label == 1, 'class1', 'class0')
test$label <- ifelse(test$label == 1, 'class1', 'class0')

# Compare CV and OOB validation error
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

# -----

# GBM -------------

# Set class name for caret training
train$label <- ifelse(train$label == 1, 'class1', 'class0')
test$label <- ifelse(test$label == 1, 'class1', 'class0')

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

#------

# XGBOOST -------------------
 
# Train xgboost
grid <- expand.grid(nrounds = 500, #the maximum number of iterations
                        eta = c(0.01, 0.1), # shrinkage / learning rate
                        max_depth = c(2, 4, 6), # max tree depth
                        #  Minimum loss reduction required to make a further partition on a leaf (larger is more conservative)
                        gamma = 0, # default
                        # similar to min observations per node
                        min_child_weight = 1, # default
                        # proportion of columns to sub-sample
                        colsample_bytree = 1, # default
                        )
xgb_model <-train(label ~ .,
                 data = train,
                 method = "xgbTree",
                 metric = "ROC",
                 trControl = ctrl,
                 tuneGrid = grid)
 
 
xgb_model$bestTune
plot(xgb_model)      # Plot the performance of the training models
res <- xgb_model$results
res
validate('xgboost', xgb_model, test, 'plots')
