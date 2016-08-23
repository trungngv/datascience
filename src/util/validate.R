# VALIDATE model performance using several metrics -----
# Parameters:
#   - model : an object returned by caret train() method
#   - test : test data, must contain a target / label variable that is called, unsurprinsly, label
#   - output_dir : where to save plots
validate <- function(method_name = '', model, test, output_dir) {
  suffix <- sprintf("%s_%s.pdf", Sys.Date(), method_name)
  imp <- varImp(model)
  pdf(file = sprintf("%s/variable_importance_%s", output_dir, suffix))
  plot(imp, top = 30, main = 'Top 30 features')
  dev.off()
  print(imp)

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
  dev.off()
  print('Area under the curve: ')
  print(res$auc)

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
