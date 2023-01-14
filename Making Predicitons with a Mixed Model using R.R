### Making Predictions with a Mixed Model using R

## Load Packages & Data ----------------------------------------------
library(tidyverse)
library(lme4)

theme_set(theme_light())

dat <- sleepstudy

## Simple Linear Regression ----------------------------------------------
# plot of the data
dat %>%
  ggplot(aes(x = Days, y = Reaction)) +
  geom_point(size = 3,
              shape = 21,
              fill = "grey",
              color = "black") +
  geom_smooth(method = "lm",
              color = "red",
              size = 1.2)

# regression
fit_ols <- lm(Reaction ~ Days, data = dat)
summary(fit_ols)

# predict the expected reaction time after 5 days of sleep deprivation

predict(fit_ols, newdata = data.frame(Days = 5))

# by hand

251.4 + 10.5 * 5

# 90% Confidence Interval
predict(fit_ols, newdata = data.frame(Days = 5), interval = 'confidence', level = 0.90)

# 90% Prediction Interval
predict(fit_ols, newdata = data.frame(Days = 5), interval = 'predict', level = 0.90)

# Use se.fit argument to get the standard error
predict(fit_ols, newdata = data.frame(Days =5), interval = 'confidence', level = 0.90, se.fit = TRUE)

# Build the 90% confidence interval with the standard error
predict(fit_ols, newdata = data.frame(Days = 5)) + qt(p = c(0.05, 0.95), df = nrow(dat) - 2) * predict(fit_ols, newdata = data.frame(Days =5), interval = 'confidence', level = 0.90, se.fit = TRUE)$se.fit

## Mixed Model ----------------------------------------------
# plot individuals
dat %>%
  ggplot(aes(x = Days, y = Reaction)) +
  geom_line(size = 1) +
  geom_point(shape = 21,
             size = 2,
             color = "black",
             fill = "white") +
  geom_smooth(method = "lm",
              se = FALSE) +
  facet_wrap(~Subject) +
  scale_x_continuous(labels = seq(from = 0, to = 9, by = 3),
                     breaks = seq(from = 0, to = 9, by = 3)) +
  labs(x = "Days of Sleep Deprivation",
       y = "Average Reaction Time",
       title = "Reaction Time ~ Days of Sleep Deprivation")


# mixed model
fit_lme <- lmer(Reaction ~ Days + (1|Subject), data = dat)
summary(fit_lme)

# look at the random effects 
ranef(fit_lme)
coef(fit_lme)

## Making predictions without accounting for the random effects

predict(fit_lme, newdata = data.frame(Days = 5), 
        re.form = NA)

## Making Predictions while accounting for the random effects (Subject - 308)

predict(fit_lme, newdata = data.frame(Days = 5, Subject = 308), 
        re.form = ~(1|Subject))

## Making Predictions while accounting for the random effects but allow the option to include new levels
# NOTE: This uses only the fixed effects only since the model doesn't know about the new subjects
predict(fit_lme, newdata = data.frame(Days = 5, Subject = 800), 
        re.form = ~(1|Subject),
        allow.new.levels = TRUE)

# Make predictions using fixed effect only and then random effects and plot the results
dat %>%
  mutate(pred_fixef = predict(fit_lme, newdata = ., re.form = NA),
         pred_ranef = predict(fit_lme, newdata = ., re.form = ~(1|Subject))) %>%
  ggplot(aes(x = Days, y = Reaction)) +
  geom_point(shape = 21,
             size = 2,
             color = "black",
             fill = "grey") +
  geom_line(aes(y = pred_fixef),
            color = "blue",
            size = 1.2) +
  geom_line(aes(y = pred_ranef),
            color = "green",
            size = 1.2) +
  facet_wrap(~Subject) +
  scale_x_continuous(labels = seq(from = 0, to = 9, by = 3),
                     breaks = seq(from = 0, to = 9, by = 3)) +
  labs(x = "Days of Sleep Deprivation",
       y = "Average Reaction Time",
       title = "Reaction Time ~ Days of Sleep Deprivation",
       subtitle = "Blue = Fixed Effects Prediction | Green = Random Effects Prediction")



## Bootstrapped Confidence Intervals with bootMer() ---------------------------------------
boot_ci <- bootMer(fit_lme,
                   nsim = 100,
                   FUN = function(x) { predict(x, newdata = data.frame(Days = 5), re.form = NA) })

boot_ci

# See what the boot_ci element contains
names(boot_ci)

# "t" contains the 100 bootstrapped resamples
head(boot_ci$t)

# plot and quantile intervals
hist(boot_ci$t)
quantile(boot_ci$t)
quantile(boot_ci$t, probs = c(0.05, 0.95))

# Standard Deviation of the bootstrapped resamples
sd(boot_ci$t)

# 90% confidence interval
mean(boot_ci$t) + qnorm(p = c(0.05, 0.95)) * sd(boot_ci$t)

## Prediction Intervals with the merTools package ----------------------------------

library(merTools)

# Need to add a Subject to the new data frame. Use a Subject ID not in the original data
predictInterval(fit_lme, 
               newdata = data.frame(Days = 5, Subject = 800), 
               n.sims = 100,
               returnSims = TRUE, 
               seed = 123, 
               level = 0.90)

# Now use a Subject ID of someone in the data. Predict 5 days of sleep deprivation for Subject 308
predictInterval(fit_lme, 
                newdata = data.frame(Days = 5, Subject = 308), 
                n.sims = 100,
                returnSims = TRUE, 
                seed = 123, 
                level = 0.90)


# Prediction interval for the entire dataset
pred_ints <- predictInterval(fit_lme, 
                newdata = dat, 
                n.sims = 100,
                returnSims = TRUE, 
                seed = 123, 
                level = 0.90)

dat_new <- cbind(dat, pred_ints)

dat_new %>%
  head()

# plot predictions
dat_new %>%
  ggplot(aes(x = Days, y = Reaction)) +
  geom_point(shape = 21,
             size = 2,
             color = "black",
             fill = "grey") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
            fill = 'light grey',
            alpha = 0.4) +
  geom_line(aes(y = fit),
            color = 'red',
            size = 1.2) +
  facet_wrap(~Subject) +
  scale_x_continuous(labels = seq(from = 0, to = 9, by = 3),
                     breaks = seq(from = 0, to = 9, by = 3)) +
  labs(x = "Days of Sleep Deprivation",
       y = "Average Reaction Time",
       title = "Reaction Time ~ Days of Sleep Deprivation")
