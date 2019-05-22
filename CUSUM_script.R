library(COUNT)
library(dplyr)
library(splines)

data(medpar)

medpar$type<-factor(medpar$type)

medpar <- data.frame(medpar)
medpar$died = as.integer(medpar$died)
medpar$los = as.integer(medpar$los)

summary(medpar$los)

hist(medpar$los)

# Regression model

model1 <- glm(died ~ age80 + poly(los,3) +
                factor(type)
              + ns(los, 25)

              , data=medpar, family="binomial")
summary(model1)

ModelMetrics::auc(model1)

medpar$predicted <-predict(model1, type="response")



# Person-specific
medpar %>%
   mutate(pt_lk = (predicted ^ died) * ((1-predicted)^(1-died)),
         pt_odds = (pt_lk/(1-pt_lk)),
         wt = ifelse(died == 1, log(2 / ((1-predicted) + (2*predicted))), log(1 / ((1-predicted) + (2*predicted))))
)



library(HEDfunctions)

usethis::use_git(message = "initial toy script")
usethis::use_github()

