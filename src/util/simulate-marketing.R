# Simulates targeted marketing based on the test set and their customer scores (given by a model).
# It shows the relationship between the number of audience to target, corresponding cut-off threshold,
# and model performance given the cut-off. This can be used to select the desired audience size,
# for example, if I restrict that accuracy must be > 0.7, what should the audience size be?
#
# Parameters:
# - score : customer scores by a model
# - label : customer label
# - positive_class : value of positive class (default = 'class1')
# - threshold = cut-off threshold to select customer audience (0 to 1)
# - target proportion = the top percentage of population to target (0 to 1)
#   Note that only one of threshold or target proportion can be set; the other parameter is obtained from
#   the parameter that is given.
#
# Returns:
# - total_population: population size
# - threshold: cut-off threshold to select target audience
# - target_proportion: size of target audience (num_target / total_population)
# - num_target: size of target audience
# - true_positives: number of true positives in the selected audience
# - accuracy: true_positives / num_target
# - recall: true_positives / number of positives in population
# - optimal_recall: max recall that can be obtained for the selected audience (= num_target / number of positives in popultation)
#
# Example marketing use-case:
#   I have a total of 150,000 customers and have marketing budget to target 10,000. This means my target
# proportion is 10,000 / 150,000 = 0.0666 or 6.7%. I can pass this number to the function as
# `simulate_marketing(prob, label, 'class1', threshold = NULL, target_proportion = 0.066)` which may give
# some output like:
# total_population    threshold target_proportion num_target true_positives  accuracy    recall optimal_recall
#           256919    0.8923627             0.058      14904          10489 0.7037708 0.2810407      0.3993355
#
# Total population and num_target are numbers wrt the test set and can be ignored if I just want
# estimation based on my marketing audience size. The method shows that the cut-off threshold for score
# is 0.89 and the accuracy is 0.70 and recall is 0.28. Note that this recall may be deceptively low
# but that is because the total positive instances in my test set is high. The optimal_recall says
# that, even if all of my selected audience is actually positive (accuracy = 1), I would only
# cover 40% of all positive instances.
#
# I can improve the recall by increasing the number of customers to target, but that will decrease my
# accuracy as seen by calling `simulate_marketing(xgb_prob, test$label, 'class1', threshold = NULL, target_proportion = 0.15)`
#total_population threshold target_proportion num_target true_positives  accuracy    recall optimal_recall
#          256919 0.7723944              0.15      38538          21540 0.5589288 0.5771395              1
#
# Ultimately, the decision is a trade-off between budget (number of customers to target), precision, and recall.
simulate_marketing <- function(score, label, positive_class = 'class1', threshold = 0.9, target_proportion = NULL) {
  if (!is.null(threshold) & !is.null(target_proportion)) {
    stop("only one of threshold or target_proportion can be set");
  }
  if (length(score) != length(label)) {
    stop("score and label must be vectors of the same length")
  }
  res <- list(total_population = length(label),
              threshold = threshold,
              target_proportion = target_proportion)
  if (!is.null(threshold)) {
    res$target_proportion <- sum(score >= threshold) / res$total_population
  } else {
    res$threshold <- as.numeric(quantile(score, 1 - target_proportion))
    threshold <- res$threshold
  }
  res$num_target <- sum(score >= threshold)
  res$true_positives <- sum(label[score >= threshold] == positive_class)
  res$accuracy <- res$true_positives / res$num_target
  res$recall <- res$true_positives / sum(label == positive_class)
  res$optimal_recall <- min(1, res$num_target / sum(label == positive_class))
  data.frame(res)
}
