library(tidyverse)
setwd('C:\\Users\\Weber\\Desktop\\¤j¾Ç\\¬F¤j110¤U\\R\\HW6')
fbdata <- read.csv("hw6-fb.csv", encoding = "UTF-8")
load("abtest.Rdata")
## check your data1
str(fbdata)

fbdata$visit_date <- as.Date(fbdata$visit_date)
fbdata$condition <- as.factor(fbdata$condition)
fbdata$gender <- as.factor(fbdata$gender)
fbdata <- fbdata[,-1]
str(fbdata)
## descriptive statistics
summary(fbdata)
## check the differnce 
### test difference
fbdata %>%
  group_by(condition) %>%
  summarise(mean_clicked_like = mean(clicked_like))

### gender differece
fbdata %>%
  group_by(gender) %>%
  summarise(mean_clicked_like = mean(clicked_like))

## Hypothesis Test:
### t-test for two sample mean
### Ha:  mu_1 - mu_0 >0
t.test(fbdata[fbdata$condition == "tips", ]$clicked_like,
       fbdata[fbdata$condition == "tools", ]$clicked_like,
       alternative = "greater")
#### reject H0, if p-value is less than the significance level 0.05

### test for more than two samples:
### ANOVA
aov.model <- aov(
  clicked_like ~ condition + gender, fbdata)

summary(aov.model)

## check the interaction term 
interaction.model <- aov( clicked_like ~ condition*gender , fbdata)
summary(interaction.model)

## final model
aov.model <- aov( clicked_like ~ condition, fbdata)

summary(aov.model)

## post-hoc test
TukeyHSD(aov.model, "condition")

## check daily differece
daily.like <- fbdata %>%
  group_by(visit_date, condition) %>%
  summarise(like_amount = mean(clicked_like))

ggplot(daily.like, aes(x = visit_date, y = like_amount, colour = as.factor(condition))) + 
  geom_point() + geom_line() +
  xlab("Date") + ylab("Like Amount") +
  ggtitle("Time Series Plot of Like Amount: Condition versus Control") +
  theme_bw()

# website A: proportion of purchased
subset_A <- fbdata %>% filter(condition == "tips" & clicked_like == 1)
like_A <- nrow(subset_A)
visitors_A <- nrow(fbdata %>% filter(condition == "tips"))
phat_A <-  (purchased_A/visitors_A)  

# website B: proportion of purchased
subset_B <- fbdata %>% filter(condition == "tools" & clicked_like == 1)
like_B <- nrow(subset_B)
visitors_B <- nrow(fbdata %>% filter(condition == "tools"))
phat_B <-  (purchased_B/visitors_B)  

#
uplift <- (phat_A - phat_B)/ phat_B * 100
uplift  #140.73%
#A is better than B by 140.73%. 

p_pool <- (like_A + like_B)/(visitors_A + visitors_B) #pooled proportion

SE_pool<- sqrt(p_pool*(1-p_pool) * ((1/visitors_A) + (1/visitors_B)))

d_hat <- phat_A - phat_B #Point Estimate or Difference in proportion

z_score <- d_hat/SE_pool

p_value <- pnorm(q = -z_score, mean = 0, sd = 1) * 2


ci <- c(d_hat - qnorm(0.975)*SE_pool, d_hat + qnorm(0.975)*SE_pool) #[0.0899, 0.1045]


#compute SE and CI for test website A and B separately

se_hat_A <- sqrt(phat_A*(1-phat_A)/visitors_A) 
ci_A <- c(phat_A - qnorm(0.975)*se_hat_A, phat_A + qnorm(0.975)*se_hat_A) 
se_hat_B <- sqrt(phat_B*(1-phat_B)/visitors_B) 
ci_B <- c(phat_B - qnorm(0.975)*se_hat_B, phat_B + qnorm(0.975)*se_hat_B) 

#install.packages("pwr")
library(pwr)

#Run a 2-sampled test
prop.test(c(like_A , like_B), c(visitors_A,visitors_B))

#chi squared test
chisq.test(fbdata$clicked_like,fbdata$condition)
