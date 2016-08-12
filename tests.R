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

#---------------------------------------
# Log-normal distribution
# Should we use geometric mean = median < arithmetic mean?
#

#---------------------------------------
# CAMPAIGN ANALYTICS
# Subject - customer or account / unit of diversion
# Unit of measurement / unit of analytics
#  - A metric
#  - aggregated customer activity (not good due to different #days)
#  - daily customer activity

all_d <- fread('~/projects/analytics_datascience/tmp/data/test_campaign/trungtest.txt')
d <- all_d[campaignname == '160617 Data Science - TAB Reactivation']
d <- all_d[campaignname ==  '160603 Data Science TAB Reactivation']
d <- all_d[campaignname == '160610 Data Science - TAB Reactivation - Email']

# visual inspection for activities per day
dd <- d[order(date), list(sum_turnover = sum(dailyturnover),
                          avg_turnover = mean(dailyturnover),
                          sum_bonusbet_ct = sum(daily_bonusbet_ct),
                          avg_bonusbet_ct = mean(daily_bonusbet_ct),
                          avg_sms_sent = mean(daily_sms_sent_ct),
                          avg_email_sent = mean(daily_email_sent_ct)),
        by = c('date', 'controlflag')]
ggplot(dd, aes(x = date, y = avg_turnover, color = controlflag)) + geom_point(aes(group = controlflag)) +
  geom_line(aes(group = controlflag)) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# not needed
d$campaignname <- NULL
d$campaignstartdate <- NULL
d$controlflag <- as.factor(d$controlflag)
d$campaignperiods <- as.factor(d$campaignperiods)

# aggregate at customer level
dc <- d[, list(turnover = sum(dailyturnover),
               bet_count = sum(daily_bet_ct),
               active_day_count = sum(ifelse(daily_bet_ct > 0, 1, 0)),
               bonusbet_turnover = sum(daily_bonusbet_turnover),
               bonusbet_count = sum(daily_bonusbet_ct),
               deposit_amount = sum(daily_deposit_value),
               deposit_count = sum(daily_deposit_ct),
               withdrawal_amount = sum(daily_withdrawal_value),
               withdrawal_count = sum(daily_withdrawal_ct),
               created_bonus_bet = sum(daily_created_bonus_bet_ct),
               email_sent = sum(daily_email_sent_ct),
               sms_sent = sum(daily_sms_sent_ct)),
        by = c('customer_number', 'controlflag', 'campaignperiods')]

# COMPARABILITY check: p < 0.05 => DIFFERENCE, p > 0.05 => SAME
# Equivalent result for UNIT of MEASUREMENT (daily or total)
# control - pre vs. send - pre, aggregation per customer
wilcox.test(dc[controlflag == 'Control' & campaignperiods == 'Pre Period', bet_count],
            dc[controlflag == 'Send Group' & campaignperiods == 'Pre Period', bet_count])
wilcox.test(dc[controlflag == 'Control' & campaignperiods == 'Pre Period', turnover],
            dc[controlflag == 'Send Group' & campaignperiods == 'Pre Period', turnover])
wilcox.test(dc[controlflag == 'Control' & campaignperiods == 'Pre Period', deposit_amount],
            dc[controlflag == 'Send Group' & campaignperiods == 'Pre Period', deposit_amount])
wilcox.test(dc[controlflag == 'Control' & campaignperiods == 'Pre Period', deposit_count],
            dc[controlflag == 'Send Group' & campaignperiods == 'Pre Period', deposit_count])

t.test(dc[controlflag == 'Control' & campaignperiods == 'Pre Period', bet_count],
       dc[controlflag == 'Send Group' & campaignperiods == 'Pre Period', bet_count])
t.test(dc[controlflag == 'Control' & campaignperiods == 'Pre Period', turnover],
       dc[controlflag == 'Send Group' & campaignperiods == 'Pre Period', turnover])

# visual check (may show some difference, but needs stat test to verify significance)
ggplot(dc[campaignperiods == 'Pre Period'], aes(x = controlflag, y = log(turnover))) + geom_boxplot()
ggplot(dc[campaignperiods == 'Pre Period'], aes(x = controlflag, y = bonusbet_count)) + geom_boxplot()

# SANITY CHECK 1: control group does not receive more promotions in post (or less)
# control - pre vs. control - post, daily bonus bet count
# In this case we can use DAILY measurement because they are independent (at least sms & email received)
# bonus_bet_count may not satisfy independence but should still be ok
ggplot(dc[controlflag == 'Control'], aes(x = campaignperiods, y = email_sent)) + geom_boxplot()

# Not as critical as #sms and #email sent
t.test(d[controlflag == 'Control' & campaignperiods == 'Post Period', daily_bonusbet_ct],
       d[controlflag == 'Control' & campaignperiods == 'Pre Period', daily_bonusbet_ct])
wilcox.test(d[controlflag == 'Control' & campaignperiods == 'Post Period', daily_bonusbet_ct],
       d[controlflag == 'Control' & campaignperiods == 'Pre Period', daily_bonusbet_ct])
t.test(d[controlflag == 'Control' & campaignperiods == 'Post Period', daily_email_sent_ct],
       d[controlflag == 'Control' & campaignperiods == 'Pre Period', daily_email_sent_ct])
t.test(d[controlflag == 'Control' & campaignperiods == 'Post Period', daily_sms_sent_ct],
       d[controlflag == 'Control' & campaignperiods == 'Pre Period', daily_sms_sent_ct])

t.test(dc[controlflag == 'Control' & campaignperiods == 'Post Period', bonusbet_count],
       dc[controlflag == 'Control' & campaignperiods == 'Pre Period', bonusbet_count])

t.test(dc[controlflag == 'Control' & campaignperiods == 'Post Period', email_sent],
       dc[controlflag == 'Control' & campaignperiods == 'Pre Period', email_sent])
pre <- dc[controlflag == 'Control' & campaignperiods == 'Pre Period', email_sent]
post <- dc[controlflag == 'Control' & campaignperiods == 'Post Period', email_sent]
t.test(post, pre)
t.test(dc[controlflag == 'Control' & campaignperiods == 'Post Period', email_sent],
       dc[controlflag == 'Control' & campaignperiods == 'Pre Period', email_sent])
pre <- dc[controlflag == 'Control' & campaignperiods == 'Pre Period', sms_sent]
post <- dc[controlflag == 'Control' & campaignperiods == 'Post Period', sms_sent]
t.test(post, pre)
t.test(dc[controlflag == 'Control' & campaignperiods == 'Post Period', sms_sent],
       dc[controlflag == 'Control' & campaignperiods == 'Pre Period', sms_sent])

# OPTIONAL SANITY CHECK 2: send group DO receive more promotions
# (not as critical as check 1, especially for campaigns other than re-activation)
# send - pre .vs send - post, daily bonus bet count
t.test(d[controlflag == 'Send Group' & campaignperiods == 'Post Period', daily_bonusbet_ct],
       d[controlflag == 'Send Group' & campaignperiods == 'Pre Period', daily_bonusbet_ct],
       alternative = "greater")

# EFFECTIVENESS test: send group has greater turnover than test group
# Unit of measurement: daily customer activity or total customer activity?
# - Daily activity is not sensitive enough if there's cross-contamination
# I.e. inactive cust still inactive; some active cust behave in the same way, ON A DAILY BASIS
# - Do daily samples satisfy independence assumption of observations? 
# Not really, unless we assume a customer's activity on a day is independent of his activity in previous days...
# Thus BETTER to use total customer activity

# Robust metrics:
# - for churn / reactivation, not stretching customer => expect avg turnover or deposit amount to be same
# - for other test (e.g. cross-sell) => #active customers may be same
# - Thus, should measure both test for activeness / count_of_active_days / bet_count & turn_over

#---------------------
# Boxplot to compare two distributions, when there are outliers and heavy tail

# Total plots, better
ggplot(dc[campaignperiods == 'Post Period'], aes(x = controlflag, y = deposit_amount)) + geom_boxplot()
ggplot(dc[campaignperiods == 'Post Period'], aes(x = controlflag, y = deposit_count)) + geom_boxplot()
ggplot(dc[campaignperiods == 'Post Period'], aes(x = controlflag, y = log(turnover))) + geom_boxplot()
ggplot(dc[campaignperiods == 'Post Period'], aes(x = controlflag, y = bet_count)) + geom_boxplot()
ggplot(dc[campaignperiods == 'Post Period'], aes(x = controlflag, y = active_day_count)) + geom_boxplot()

#----------------------------------------
# CUSTOMER AGGREGATION tests - IS there a change in individual customer behaviour?
t.test(dc[controlflag == 'Send Group' & campaignperiods == 'Post Period', turnover],
       dc[controlflag == 'Control' & campaignperiods == 'Post Period', turnover])
t.test(dc[controlflag == 'Send Group' & campaignperiods == 'Post Period', bet_count],
       dc[controlflag == 'Control' & campaignperiods == 'Post Period', bet_count])
t.test(dc[controlflag == 'Send Group' & campaignperiods == 'Post Period', deposit_amount],
       dc[controlflag == 'Control' & campaignperiods == 'Post Period', deposit_amount])
t.test(dc[controlflag == 'Send Group' & campaignperiods == 'Post Period', deposit_count],
       dc[controlflag == 'Control' & campaignperiods == 'Post Period', deposit_count])
t.test(dc[controlflag == 'Send Group' & campaignperiods == 'Post Period', active_day_count],
       dc[controlflag == 'Control' & campaignperiods == 'Post Period', active_day_count])

# Total, Mann-Whitney test
wilcox.test(dc[controlflag == 'Send Group' & campaignperiods == 'Post Period', turnover],
            dc[controlflag == 'Control' & campaignperiods == 'Post Period', turnover])
wilcox.test(dc[controlflag == 'Send Group' & campaignperiods == 'Post Period', bet_count],
            dc[controlflag == 'Control' & campaignperiods == 'Post Period', bet_count])
wilcox.test(dc[controlflag == 'Send Group' & campaignperiods == 'Post Period', deposit_amount],
            dc[controlflag == 'Control' & campaignperiods == 'Post Period', deposit_amount], conf.int = T)
wilcox.test(dc[controlflag == 'Send Group' & campaignperiods == 'Post Period', deposit_count],
            dc[controlflag == 'Control' & campaignperiods == 'Post Period', deposit_count])
wilcox.test(dc[controlflag == 'Send Group' & campaignperiods == 'Post Period', active_day_count],
       dc[controlflag == 'Control' & campaignperiods == 'Post Period', active_day_count])

#---------------------------------------
# Proportion test
# Did the campaign result in INCREMENTAL bettor / improve retention or reactivation in the period?
# i.e. is there a difference in 'activeness', or probability to be active?

# Compute tables for activeness
# Test group typically have higher daily bonus bet (expected), not necessarily daily bet as they haven't spent the bonus yet
da <- d[, list(is_active = any(daily_bet_ct > 0 | daily_bonusbet_ct > 0)), by = c('customer_number', 'controlflag', 'campaignperiods')]
da <- d[, list(is_active = any(daily_bonusbet_ct > 0)), by = c('customer_number', 'controlflag', 'campaignperiods')]
da <- d[, list(is_active = any(daily_bet_ct > 0)), by = c('customer_number', 'controlflag', 'campaignperiods')]
da <- d[, list(is_active = any(daily_deposit_ct > 0)), by = c('customer_number', 'controlflag', 'campaignperiods')]

# Pre-period
pre_period <- table(da[campaignperiods == 'Pre Period', list(controlflag, is_active)])
pre_period
# prop.test requires #successes first, then #failures
prop.test(pre_period[, c('TRUE', 'FALSE')])
# Post-period
post_period <- table(da[campaignperiods == 'Post Period', list(controlflag, is_active)])
post_period
# prop1 is CONTROL, prop2 is TEST / SEND
prop.test(post_period[, c('TRUE', 'FALSE')])

