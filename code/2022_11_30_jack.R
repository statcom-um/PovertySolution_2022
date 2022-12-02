#an exploration of late payments and how to handle them

prop_late_csv <- read.csv(file = "C:/Users/Jack Li/Downloads/2022_11_22.csv")

prop_late_csv_orig <- read.csv(file = "C:/Users/Jack Li/Downloads/2022_03_21.csv")

prop_late_csv$name <- rep("", 1155)
for (i in 1:1155) {
  prop_late_csv$name[i] <- prop_late_csv_orig$name[which(prop_late_csv_orig$tid == prop_late_csv$tid[i])]
}

prop_late_csv$proportion_late <- prop_late_csv$late_trans/prop_late_csv$total_trans #grabbing the proportion of late payments
#there are results over 1 - we have to adjust for that.

# prop_late_csv$propL_adjusted <- prop_late_csv$proportion_late
# for (i in 1:1257) {
#   same_household_max <- max(prop_late_csv$proportion_late[prop_late_csv$name == prop_late_csv$name[i] & prop_late_csv$proportion_late <= 1])
#   if (prop_late_csv$proportion_late[i] > 1) {
#     prop_late_csv$propL_adjusted[i] <- same_household_max
#     print(paste("Changed", i, "from", prop_late_csv$proportion_late[i], "to", same_household_max))
#   }
#} #a crude form of adjustment that replaces any proportions over 1 with the highest proportion of late payments <=1 from the same building and does nothing else to the proportions.
  #DON'T NEED IT ANYMORE - EVERYTHING IS ALREADY UNDER 1

#Lower Wilson score intervals
  #Wilson scores are a type of binomial confidence interval that adjust for both the observed sample size and the 
  #we can take the lower bound of the 95% confidence Wilson interval - the lower bound allows for the benefit of the doubt for those with very few recorded payments, even if they have a decently high proportion of them.
#in other words, it treats 1 month late, 2 months total more leniently than 15 months late, 30 total (the lower interval for the former is 0.025, the latter is 0.331).
  #this interval can be tuned by the confidence level - if alpha = 0.05 leads to CIs that you think are too generous with their lower bound, let me know and I can make the alpha parameter make a little "more sense" - I started with alpha = 0.05 as the default 95% CI, but setting alpha = 50% instead turns 1 month late, 2 months total with a lower bound of ~0.35, for example, which may be more sensible.

library(Hmisc)
prop_late_csv$wilson_prop_late <- rep(0, 1155)
for (i in 1:1155) {
  x <- prop_late_csv$late_trans[i]
  n <- prop_late_csv$total_trans[i]
  if (x > n) n = x #if it has x late payments, there need to be at least x payments total.
  prop_late_csv$wilson_prop_late[i] <- binconf(x, n, alpha= 0.5, method = "wilson")[2] #lower bound of interval
  #for a select amount of observations, the wilson interval goes VERY slightly below 0 due to some precision weirdness - I bumped those up to 0 since negative proportions are theoretically impossible to accomplish.
  if (prop_late_csv$wilson_prop_late[i] < 0 ) {
    prop_late_csv$wilson_prop_late[i] = 0
  }
}

library(ggplot2)

#Mean, median, IQR, SD per building
#boxplots, violin plots
lateproportion_summary_stats <- tapply(prop_late_csv$proportion_late, prop_late_csv$name, summary)
latewilson_summary_stats <- tapply(prop_late_csv$wilson_prop_late, prop_late_csv$name, summary)
saveRDS(lateproportion_summary_stats, file = "C:/Users/Jack Li/Downloads/raw_late_proportion_summary_stats.RDS")
saveRDS(latewilson_summary_stats, file = "C:/Users/Jack Li/Downloads/wilson_late_proportion_summary_stats.RDS")
library(table1)

table1(~ age + factor(sex) + income + num_dep + rent + proportion_late + wilson_prop_late| name, data = prop_late_csv)
library(flextable)
library(dplyr)
tbl1 <- table1(~ age + factor(sex) + income + num_dep + rent + proportion_late + wilson_prop_late| name, data = prop_late_csv)
write.csv(tbl1, file = "C:/Users/Jack Li/Downloads/STATCOM_prop_late_summary_statistics.csv")

windows()
ggplot(data = prop_late_csv, aes(x = name, y = proportion_late)) + geom_violin() +theme_classic(base_size = 10) + geom_boxplot(width = 0.1) + ylim(0,1)

windows()
ggplot(data = prop_late_csv, aes(x = name, y = wilson_prop_late)) + geom_violin() +theme_classic(base_size = 10) + geom_boxplot(width = 0.1) + ylim(0, 1) #a rudimentary violin plot of results when using Wilson scores.

#you'll notice in the summary statistics and in the violin plots that using the Wilson score has the tendency to shrink most proportions to zero (by nature of using the lower bound instead of the point estimate) - this could lead to some trickiness with interpretation as we are using a lower bound of a confidence interval as our response variable, rather than a point estimate. Nonetheless, if we plan to categorize tenants by quartiles and/or in a way that is relative to each other, this should not be a major issue.

#I am more partial to the Wilson score implementation since it can take into account different sample sizes in each point estimate, and I think I can adjust it easily to be more useful, but I do understand how shifting the estimates down is troublesome and how it may be a bit harder to explain - let me know what you think.