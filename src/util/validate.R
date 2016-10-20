# Utility method for sensinble validation of a classifier. There will be kinds of outputs: text and figures.
# All text-based results are stored in output_dir/prefix_summary_date.txt, and all figures are stored in 
# output_dir/figures/prefix_..._date.txt.
# Parameters:
#   - output_dir : where to store output
#   - prefix : a prefix in all files, typically method name, for distinguishing different methods, if applicable
#   - model : a model trained with caret package
#   - test : a test set
validate <- function(output_dir, prefix, model, test) {
  figure_dir <- sprintf('%s/figures', output_dir)
  summary_file <- sprintf('%s/%s_summary.txt', output_dir, prefix)
  var_importance_file <- sprintf("%s/figures/%s_var_importance.pdf", output_dir, prefix)
  roc_file <- sprintf("%s/figures/%s_roc.pdf", output_dir, prefix)
  cum_detection_file <- sprintf("%s/figures/%s_cumulative_detection.pdf", output_dir, prefix)
  lift_file <- sprintf("%s/figures/%s_lift.pdf", output_dir, prefix)
  dir.create(figure_dir, recursive = TRUE)
  
  imp <- varImp(model)
  #pdf(file = var_importance_file)
  #plot(imp, top = 30, main = 'Top 30 features')
  #dev.off()

  sink(summary_file)
  print('Variable importance: ')
  print(imp, top = 100)
  
  # Confusion matrix (decision threshold = 0.5)
  pred <- predict(model, test)
  print('Confusion matrix')
  print(confusionMatrix(pred, test$label))
  
  # ROC
  pred <- predict(model, test, type="prob")
  print('Few example predictions')
  print(head(pred))
  
  numeric_label <- ifelse(test$label == 'class1', 1, 0)
  pdf(file = roc_file)
  res <- roc(response = numeric_label, predictor = pred[, 'class1'],
             auc=TRUE, plot=TRUE)
  title(sprintf('AUC = %.2f', res$auc))
  dev.off()
  print('Area under the curve: ')
  print(res$auc)
  
  # Cumulative positives detection and lift
  res <- gains(actual=numeric_label, predicted=pred[, 'class1'], optimal=TRUE)
  Npos <- nrow(test[label == 'class1'])
  optimal <- res$cume.obs/Npos
  df <- data.frame(depth = res$depth,
                   cume_pct_positive = res$cume.pct.of.total,
                   optimal_cume_pct_positive =  ifelse(optimal < 1, optimal, 1))
  print('Cumulative percentage of total positives vs. optimal')
  print(df)
  sink()
  # Reshape data for ploting
  df <- df %>% gather(key, value, -depth) %>% data.table()
  df$key <- factor(df$key, levels = c("optimal_cume_pct_positive", "cume_pct_positive"),
                   labels = c("Optimal model", "Learn model"))
  ggplot(df, aes(x = depth, y = value, color = key)) +
    geom_point() + geom_line(aes(group = key)) + 
    ylim(c(0, 1)) +
    xlab("% of test instances (decreasing in score)") +
    ylab("Cumulative positives detected") +
    scale_color_discrete(name = "")
  ggsave(cum_detection_file)
  ggplot(data.frame(x = res$depth, y = res$lift), aes(x, y)) +
    geom_point() + geom_line() + 
    xlab("% of test instances (decreasing in score)") +
    ylab("Lift")
  ggsave(lift_file)
  pred
}
