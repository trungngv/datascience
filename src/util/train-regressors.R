library(caret)
library(randomForest)
library(gbm)
library(xgboost)
library(doMC)
library(plyr)
library(dplyr)
library(corrplot)
# Use parallel backend
registerDoMC(cores = 4)

# Some toy data
data(Boston, package="MASS")
d <- Boston
# set target variable
# for survival analysis, requirement is that 0 = censored and 1 = death / event
d$target <- d$medv
d$medv <- NULL

# split data into train & test
set.seed(1110)
train_ind <- createDataPartition(y = d$target, p = .70, list = FALSE)
train <- d[train_ind,]
test <- d[-train_ind,]

# knn model
ctrl <- trainControl(method = "cv", number = 5)
model_knn <- train(target ~ ., data = train,
                   method = "knn",
                   trControl = ctrl,
                   preProcess = c("center", "scale"),
                   tuneLength = 10)
val_knn <- predict(model_knn, test)
(rmse_knn <- RMSE(val_knn, test$target))
write_rds(model_knn, 'models/knn')
print(model_knn)
plot(model_knn)
plot(varImp(model_knn))

# xgboost model
ctrl <- trainControl(method = "cv", number = 5, search = "random")
model_xgb <- train(target ~ ., data = train,
                   method = "xgbTree",
                   trControl = ctrl,
                   tuneLength = 10)
val_xgb <- predict(model_xgb, test)
(rmse_xgb <- RMSE(val_xgb, test$target))
write_rds(model_xgb, 'models/xgb')
print(model_xgb)
plot(varImp(model_xgb))

# LM model
model_linear <- lm(target ~ deposit_amount_initial + deposit_count_inital + turnover_initial +
                     bet_count_initial + turnover_reg_initial + bet_count_reg_initial, data = train)
val_lm <- predict(model_linear, test)
(rmse_lm <- RMSE(val_lm, test$target))
write_rds(model_linear, 'models/linear')
summary(model_linear)

# L2-regularised linear model
ctrl <- trainControl(method = "cv", number = 5, search = "random")
# Input to glmnet must be in X, Y format
model_l2reg <- train(train[, features, with = FALSE], train$target,
                     method = "glmnet",
                     trControl = ctrl,
                     metric = "RMSE",
                     preProcess = c("center", "scale"),
                     tuneLength = 10)
val_l2reg <- predict(model_l2reg, test[, features, with = FALSE])
(rmse_l2reg <- RMSE(val_l2reg, test$target))
write_rds(model_l2reg, 'models/l2reg')
plot(model_l2reg)
print(model_l2reg)
plot(varImp(model_l2reg))

# Comparing predictions by models
ggplot(data.frame(x = val_xgb, y = test$target), aes(x, y)) + geom_jitter() + geom_smooth() + xlim(0, 10) + ylim(0, 10)
ggplot(data.frame(x = val_knn, y = test$target), aes(x, y)) + geom_jitter() + geom_smooth() + xlim(0, 10) + ylim(0, 10)
ggplot(data.frame(x = val_lm, y = test$target), aes(x, y)) + geom_jitter() + geom_smooth() + xlim(0, 10) + ylim(0, 10)
ggplot(data.frame(x = val_l2reg, y = test$target), aes(x, y)) + geom_jitter() + geom_smooth() + xlim(0, 10) + ylim(0, 10)
