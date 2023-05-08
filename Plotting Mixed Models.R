##### Plotting Mixed Models #####

## Load packages
library(tidyverse)
library(lme4)
library(lattice)
library(patchwork)

theme_set(theme_bw())

## load data
dat <- sleepstudy

dat %>%
  head()

## Fit mixed model
fit_lmer <- lmer(Reaction ~ Days + (1 + Days|Subject), data = dat)
summary(fit_lmer)

# look at the random effects
random_effects <- ranef(fit_lmer) %>%
  pluck(1) %>%
  rownames_to_column() %>%
  rename(Subject = rowname, Intercept = "(Intercept)") 

random_effects %>%
  knitr::kable()

## plot random effects
dotplot(ranef(fit_lmer))

## Make one in ggplot2
subject_intercept_sd <- 24.7
subject_days_sd <- 5.92

int_plt <- random_effects %>%
  mutate(Subject = as.factor(Subject)) %>%
  ggplot(aes(x = Intercept, y = reorder(Subject, Intercept))) +
  geom_errorbar(aes(xmin = Intercept - subject_intercept_sd,
                    xmax = Intercept + subject_intercept_sd),
                width = 0,
                size = 1) +
  geom_point(size = 3,
             shape = 21,
             color = "black",
             fill = "white") +
  geom_vline(xintercept = 0, 
             color = "red",
             size = 1,
             linetype = "dashed") +
  scale_x_continuous(breaks = seq(-60, 60, 20)) +
  labs(x = "Intercept",
       y = "Subject ID",
       title = "Random Intercepts")

slope_plt <- random_effects %>%
  mutate(Subject = as.factor(Subject)) %>%
  ggplot(aes(x = Days, y = reorder(Subject, Days))) +
  geom_errorbar(aes(xmin = Days - subject_days_sd,
                    xmax = Days + subject_days_sd),
                width = 0,
                size = 1) +
  geom_point(size = 3,
             shape = 21,
             color = "black",
             fill = "white") +
  geom_vline(xintercept = 0, 
             color = "red",
             size = 1,
             linetype = "dashed") +
  xlim(-60, 60) +
  labs(x = "Slope",
       y = "Subject ID",
       title = "Random Slopes")

slope_plt / int_plt

## Plot Residual
hist(resid(fit_lmer))
plot(fit_lmer)

## Plotting our own residual ~ fitted
lmer_fitted <- predict(fit_lmer, newdata = dat, re.form = ~(1 + Days|Subject))
lmer_resid <- dat$Reaction - lmer_fitted

plot(x = lmer_fitted,
     y = lmer_resid,
     pch = 19,
     main = "Resid ~ Fitted",
     xlab = "Fitted",
     ylab = "Residuals")
abline(h = 0,
       col = "red",
       lwd = 3,
       lty = 2)


### Plotting the time series on new data
# training set
dat_train <- dat %>%
  group_by(Subject) %>%
  slice(head(row_number(), 6)) %>%
  ungroup()

# testing set
dat_test <- dat %>%
  group_by(Subject) %>%
  slice(tail(row_number(), 4)) %>%
  ungroup()

## Fit mixed model
fit_lmer2 <- lmer(Reaction ~ Days + (1 + Days|Subject), data = dat_train)
summary(fit_lmer2)

# Predict on training set
train_preds  <- merTools::predictInterval(fit_lmer2,
                                              newdata = dat_train,
                                              n.sims = 100,
                                              returnSims = TRUE,
                                              seed = 657,
                                              level = 0.9) %>%
  as.data.frame()

dat_train <- dat_train %>% bind_cols(train_preds)

dat_train$group <- "train"

# Predict on test set with 90% prediction intervals
test_preds  <- merTools::predictInterval(fit_lmer2,
                               newdata = dat_test,
                               n.sims = 100,
                               returnSims = TRUE,
                               seed = 657,
                               level = 0.9) %>%
  as.data.frame()

dat_test <- dat_test %>% bind_cols(test_preds)

dat_test$group <- "test"

## Combine the data together
combined_dat <- bind_rows(dat_train,
          dat_test) %>%
  arrange(Subject, Days)

## Plot the time series of predictions and observed data
combined_dat %>%
  mutate(group = factor(group, levels = c("train", "test"))) %>%
  ggplot(aes(x = Days, y = Reaction)) +
  geom_ribbon(aes(ymin = lwr,
                  ymax = upr),
              fill = "light grey",
              alpha = 0.8) +
  geom_line(aes(y = fit),
            col = "red",
            size = 1) +
  geom_point(aes(fill = group),
             size = 3,
             shape = 21) +
  geom_line() +
  facet_wrap(~Subject) +
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(face = "bold", color = "white"),
        legend.position = "top") +
  labs(x = "Days",
       y = "Reaction Time",
       title = "Reaction Time based on Days of Sleep Deprivation")
