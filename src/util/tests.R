# hypothesis testings for TWO SAMPLES

# t-test for two sample means

#---------------------------------------
# NORMAL t-test, including outliers
# If different variances, Welch t-test will be used

# SIGNIFICANT
t.test(1:10, y = c(7:20)) # p = 0.00001

# NOT SIGNIFICANT at alpha = 0.05 (outlier => bigger variance => harder to detect effect)
# If alpha = 0.15 (e.g. in marketing?), then maybe significant
t.test(1:10, y = c(7:20, 200)) # p = .1245 

# SIGNIFICANT
# But if we have larger sample size: mean more 'stable', variance reduces, so outlier's effect can be reduced
# t-test can be robust to outlier, if enough samples
t.test(rep(1:10, 5), y = c(rep(7:20, 5), 200))

# Use Mann-Whitney test when non-normality may not be satisfied
# In this case, result is SIGNIFICANT (cf t-test)
wilcox.test(1:10, y = c(7:20, 200), conf.int = T)
# With more smaples, even more robust
wilcox.test(rep(1:10, 5), y = c(rep(7:20, 5), 200), conf.int=T)

#-----------------------------------------
# Two proportions test
# Input 1:
#    x is (#successes_in_group1, #successes_in_group2, ..., #successes_in_groupn)
#    n is (#trials1, #trials2, ...)
prop.test(x = c(10, 20), n = c(100, 102))
# Input 2: a matrix 
#            #successes #trials
# group1
# group 2
prop.test(matrix(c(10, 20, 90, 82), nrow=2, ncol=2))

