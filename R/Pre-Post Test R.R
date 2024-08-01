##########################
### Imports ####
########################## 
library(tidyverse)
library(readxl)
library(skimr)
library(ggthemes)
library(scales)
library(forecast)
library(zoo)
library(car)
library(rstatix)
library(pwr)
options(scipen = 5)

# -------

##########################
### Load Data ####
########################## 

f <- "file.csv"

#read data
pp <- read_csv(f)


# -------

## #define test store ###
test <- "20"
control <- c(6, 69, 8, 28, 27)

##########################
### Visualizations ####
########################## 

#filter data
pp2 <- pp %>% 
  filter(group_col %in% c(test) | Store %in% control)

# Visualization
ggplot(pp2) +
  geom_density(aes( x= target_col,
                    group = group_col,
                    fill = group_col),
               alpha=0.5)

# Boxplot
ggplot(pp2) +
  geom_boxplot(aes( x= group_col, y= target_col,
                    fill = group_col),
               alpha=0.5)



# -------

##########################
### Stats Tests ####
########################## 

# Levene’s Test for Equal Variance
"   First, we must test whether the variance in differenced values are equal 
     between two groups. We can use Levene’s Test for Variance Equality. "

## Ho : Variances are equal
leveneTest(target_col ~ factor(group_col), data = pp2)

#---
# Var Equal
# Anova for equal variances
summary(aov(target_col ~ group_col, data = pp2))

# Var Not Equal
library(onewaytests)
welch.test(target_col ~ group_col, data = pp2)

#---

# Tukey for equal variances
tukey_hsd(pp2, target_col ~ group_col, conf.level = 0.95)


# Games-Howell test result (for unequal variances)
games_howell_test(target_col ~ group_col, data = pp2, conf.level = 0.95)

#---

# -------


###############################################
## AB Tests with Pre Post Samples using NORMAL DIST
# Showing Group vs Group
###############################################

"
In this simulation, we will use the normal distribution to create a distribution of the
variable being tested. We can rely on the Central Limit Theorem to be able to use the normal
distribution, given that the distribution of the means from a bootstrap converges to normality.
"
# Define Samples A and B
A <- "Control"
B <- "203T"
# Data
prepost <- pp2 %>% 
  filter(group_col %in% B | Store %in% control) %>% 
  group_by(group_col, after) %>% 
  summarise(mu = mean(target_col),
            std =sd(target_col),
            N = n()) %>% 
  ungroup()


# parameters from A and B [ PRE(0) | POST(1) ]
a_mean <- prepost %>% filter(group_col == A & after == 0) %>% select(mu) %>% unlist() %>% unname()
a_std <- prepost %>% filter(group_col == A & after == 0) %>% select(std) %>% unlist() %>% unname()
na <- prepost %>% filter(group_col == A & after == 0) %>% select(N) %>% unlist() %>% unname()
b_mean <- prepost %>% filter(group_col == B & after == 0) %>% select(mu) %>% unlist() %>% unname()
b_std <- prepost %>% filter(group_col == B & after == 0) %>% select(std) %>% unlist() %>% unname()
nb <- prepost %>% filter(group_col == B & after == 0) %>% select(N) %>% unlist() %>% unname()


# parameters from A and B [ PRE(0) | POST(1) ]
a_mean1 <- prepost %>% filter(group_col == A & after == 1) %>% select(mu) %>% unlist() %>% unname()
a_std1 <- prepost %>% filter(group_col == A & after == 1) %>% select(std) %>% unlist() %>% unname()
na1 <- prepost %>% filter(group_col == A & after == 1) %>% select(N) %>% unlist() %>% unname()
b_mean1 <- prepost %>% filter(group_col == B & after == 1) %>% select(mu) %>% unlist() %>% unname()
b_std1 <- prepost %>% filter(group_col == B & after == 1) %>% select(std) %>% unlist() %>% unname()
nb1 <- prepost %>% filter(group_col == B & after == 1) %>% select(N) %>% unlist() %>% unname()


# Means difference
diff_ab = b_mean - a_mean
diff_ab1 = b_mean1 - a_mean1

# Calculate Standard Error
se <- sqrt( ((a_std**2)/na) + ((b_std**2)/nb) )
se1 <- sqrt( ((a_std1**2)/na1) + ((b_std1**2)/nb1) )

# # Data Pre-Post 
# ab_dist <- bind_rows(
#   data.frame(group=A, vals=rnorm(10000, a_mean, a_std)),
#   data.frame(group=B, vals=rnorm(10000, b_mean, b_std))
# )

# # Visualization
# ggplot(ab_dist) +
#   geom_density(aes( x= vals,
#                     group = group,
#                     fill = group),
#                alpha=0.5)


# Visualization Difference in Difference (Pre vs Post)
bind_rows(
  data.frame(group="Pre", vals=rnorm(10000, diff_ab, se)),
  data.frame(group="Post", vals=rnorm(10000, diff_ab1, se1))
) %>% 
  ggplot() +
  geom_density( aes(x=vals, group=group, fill=group), alpha=0.5 ) +
  scale_x_continuous(labels= comma)

# Difference in Difference is significant? Pre vs Post
t.test(rnorm(10000, diff_ab, se), 
       rnorm(10000, diff_ab1, se1), 
       alternative = "two.sided", var.equal = T)

# Probability Difference more extreme than the difference between A and B
point_estimate = diff_ab # my test

# Calculate the z-score
z <- (point_estimate - 0) / se

# Probability Difference more extreme than the difference between A and B
# If Z < 0, then lower.tail = T. Else, use lower.tail= F
probability <- pnorm(z, lower.tail = z<0)
2*(probability)


# Find the z-scores for the 2.5th and 97.5th percentiles
lower_bound <- qnorm(0.025, diff_ab, se)
upper_bound <- qnorm(0.975, diff_ab, se)

cat("The range that contains 95% of the data is from", lower_bound*100, "% to", upper_bound*100, "%\n")

# T Test: Is the Pre period statistically significant?
t.test(rnorm(10000, a_mean, a_std), 
       rnorm(10000, b_mean, b_std), 
       alternative = "two.sided", var.equal = T)

### POST
# Probability Difference more extreme than the difference between A and B
point_estimate = diff_ab1 # my test

# Calculate the z-score
# "Transform our test result into a z score by dividing by the standard error"
z <- (point_estimate - 0) / se1

# Probability Difference more extreme than the difference between A and B
# If Z < 0, then lower.tail = T. Else, use lower.tail= F
probability <- pnorm(z, lower.tail = z<0)
2*(probability)


# Find the z-scores for the 2.5th and 97.5th percentiles
lower_bound <- qnorm(0.025, diff_ab1, se1)
upper_bound <- qnorm(0.975, diff_ab1, se1)

cat("The range that contains 95% of the data is from", lower_bound*100, "% to", upper_bound*100, "%\n")

# T Test: Is the Post period statistically significant?
t.test(rnorm(10000, a_mean1, a_std1), 
       rnorm(10000, b_mean1, b_std1), 
       alternative = "two.sided", var.equal = T)

#------------------------------------------------------