## Regression with binary outcomes
## ═════════════════════════════════

## Logistic regression
## ───────────────────────

##   This far we have used the `lm' function to fit our regression models.
##   `lm' is great, but limited–in particular it only fits models for
##   continuous dependent variables. For categorical dependent variables we
##   can use the `glm()' function.

##   For these models we will use a different dataset, drawn from the
##   National Health Interview Survey. From the [CDC website]:

##         The National Health Interview Survey (NHIS) has monitored
##         the health of the nation since 1957. NHIS data on a broad
##         range of health topics are collected through personal
##         household interviews. For over 50 years, the U.S. Census
##         Bureau has been the data collection agent for the National
##         Health Interview Survey. Survey results have been
##         instrumental in providing data to track health status,
##         health care access, and progress toward achieving national
##         health objectives.

##   Load the National Health Interview Survey data: 

setwd("~/Desktop/Logistic Regression Exercises/logistic_regression")
getwd()
NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels

##   [CDC website] http://www.cdc.gov/nchs/nhis.htm

## Logistic regression example
## ───────────────────────────────

##   Let's predict the probability of being diagnosed with hypertension
##   based on age, sex, sleep, and bmi

str(NH11$hypev) # check stucture of hypev
levels(NH11$hypev) # check levels of hypev
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
              data=NH11, family="binomial")
coef(summary(hyp.out))

## Logistic regression coefficients
## ────────────────────────────────────

##   Generalized linear models use link functions, so raw coefficients are
##   difficult to interpret. For example, the age coefficient of .06 in the
##   previous model tells us that for every one unit increase in age, the
##   log odds of hypertension diagnosis increases by 0.06. Since most of us
##   are not used to thinking in log odds this is not too helpful!

##   One solution is to transform the coefficients to make them easier to
##   interpret

hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab  #?

## Generating predicted values
## ───────────────────────────────

##   In addition to transforming the log-odds produced by `glm' to odds, we
##   can use the `predict()' function to make direct statements about the
##   predictors in our model. For example, we can ask "How much more likely
##   is a 63 year old female to have hypertension compared to a 33 year old
##   female?".

# Create a dataset with predictors set at desired levels
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
# predict hypertension at those levels
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))  # different from just putting predict. Look at the difference.

##   This tells us that a 33 year old female has a 13% probability of
##   having been diagnosed with hypertension, while and 63 year old female
##   has a 48% probability of having been diagnosed.

## Packages for  computing and graphing predicted values
## ─────────────────────────────────────────────────────────

##   Instead of doing all this ourselves, we can use the effects package to
##   compute quantities of interest for us (cf. the Zelig package).

install.packages("effects")
library(effects)
plot(allEffects(hyp.out))

## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).

        summary(glm(everwrk ~ age_p + r_maritl, data = NH11, family = "binomial"))
        
##   2. Predict the probability of working for each level of marital
##      status.
        
        summary(glm(everwrk ~ r_maritl, data = NH11, family = binomial))
        
        levels(NH11$r_maritl) # observe the various levels of marital status

        # Let's create a binary variable each of the marital statuses:
        underFourteen <- NH11$r_maritl
        underFourteen <- sub("0 Under 14 years", 1, underFourteen)
        underFourteen[underFourteen != 1] <- 0
        underFourteen <- factor(underFourteen)
        
        marriedSpInHouse <- NH11$r_maritl
        marriedSpInHouse <- sub("1 Married - spouse in household", 1, marriedSpInHouse)
        marriedSpInHouse[marriedSpInHouse != 1] <- 0
        marriedSpInHouse <- factor(marriedSpInHouse)
        
        marriedSpNotInHouse <- NH11$r_maritl
        marriedSpNotInHouse <- sub("2 Married - spouse not in household", 1, marriedSpNotInHouse)
        marriedSpNotInHouse[marriedSpNotInHouse != 1] <- 0
        marriedSpNotInHouse <- factor(marriedSpNotInHouse)
        
        marriedSpInHouseUk <- NH11$r_maritl
        marriedSpInHouseUk <- sub("3 Married - spouse in household unknown", 1 , marriedSpInHouseUk)
        marriedSpInHouseUk[marriedSpInHouseUk != 1] <- 0
        marriedSpInHouseUk <- factor(marriedSpInHouseUk)
        
        widowed <- NH11$r_maritl
        widowed <- sub("4 Widowed", 1, widowed)
        widowed[widowed != 1] <- 0
        widowed <- factor(widowed)
        
        divorced <- NH11$r_maritl
        divorced <- sub("5 Divorced", 1, divorced)
        divorced[divorced != 1] <- 0
        divorced <- factor(divorced)
        
        separated <- NH11$r_maritl
        separated <- sub("6 Separated", 1, separated)
        separated[separated != 1] <- 0
        separated <- factor(separated)
        
        neverMarried <- NH11$r_maritl
        neverMarried <- sub("7 Never married", 1, neverMarried)
        neverMarried[neverMarried != 1] <- 0
        neverMarried <- factor(neverMarried)
        
        liveWPartner <- NH11$r_maritl
        liveWPartner <- sub("8 Living with partner", 1, liveWPartner)
        liveWPartner[liveWPartner != 1] <- 0
        liveWPartner <- factor(liveWPartner)
        
        ukMaritalStatus <- NH11$r_maritl
        ukMaritalStatus <- sub("9 Unknown marital status", 1, ukMaritalStatus)
        ukMaritalStatus[ukMaritalStatus != 1] <- 0
        ukMaritalStatus <- factor(ukMaritalStatus)
        
        # Edit the data frame to include the new binary variables and save it to a new object. 
        library(dplyr)
        
        NH11_edit <- NH11 %>%
          select(everwrk, r_maritl) %>%
          mutate(underFourteen, marriedSpInHouse, marriedSpNotInHouse, marriedSpInHouseUk, 
                  widowed, divorced, separated, neverMarried, liveWPartner, ukMaritalStatus)
        
        # Factor the everwrk to just "Yes" and "No" because we only care about those situations.
        NH11_edit$everwrk <- factor(NH11_edit$everwrk, levels=c("2 No", "1 Yes"))
        NH11_edit <- na.omit(NH11_edit) # maybe na.omit(NH11_edit$everwrk) Remove NA values so that everyone will have either worked or not worked.
        NH11_edit$r_maritl <- factor(NH11_edit$r_maritl) # If we factor, it removes two level: those under fourteen and married spouse unknown
                                                         # because those values do not exist.
        # Remove underFourteen and marriedSpInHouseUk:
        NH11_edit <- NH11_edit %>%
          select(everwrk, r_maritl, marriedSpInHouse, marriedSpNotInHouse, 
                 widowed, divorced, separated, neverMarried, liveWPartner, ukMaritalStatus)
        
        # Predict probability of working for each marital status.
        mod1 <- glm(everwrk~ marriedSpInHouse, data = NH11_edit, family = binomial)
        pred1 <- predict(mod1, type = "response")
        table(NH11_edit$everwrk, pred1 >= 0.88)
        (4834+1263)/(14040) # Accuracy of 43%
        
        mod2 <- glm(everwrk~ marriedSpNotInHouse, data = NH11_edit, family = binomial)
        pred2 <- predict(mod2, type = "response")
        table(NH11_edit$everwrk, pred2 >= 0.87)
        (1861+186)/(14040) # Accuracy of around 15%
        
        mod3 <- glm(everwrk~ widowed, data = NH11_edit, family = binomial)
        pred3 <- predict(mod3, type = "response")
        table(NH11_edit$everwrk, pred3 >= 0.87)
        (2214+1583)/(14040) # Accuracy of 27%
        
        mod4 <- glm(everwrk~ divorced, data = NH11_edit, family = binomial)
        pred4 <- predict(mod4, type = "response")
        table(NH11_edit$everwrk, pred4 >= 0.94)
        (1806+1786)/(14040) # Accuracy of 26%
        
        mod5 <- glm(everwrk~ separated, data = NH11_edit, family = binomial)
        pred5 <- predict(mod5, type = "response")
        table(NH11_edit$everwrk, pred5 >= 0.88)
        (412+1832)/(14040) # Accuracy of 16%
        
        mod6 <- glm(everwrk~ neverMarried, data = NH11_edit, family = binomial)
        pred6 <- predict(mod6, type = "response")
        table(NH11_edit$everwrk, pred6 >= 0.89)
        (10012+702)/(14040) # Accuracy of 76%
        
        mod7 <- glm(everwrk~ liveWPartner, data = NH11_edit, family = binomial)
        pred7 <- predict(mod7, type = "response")
        table(NH11_edit$everwrk, pred7 >= 0.87)
        (531+1817)/(14040) # Accuracy of 17%
        
        mod8 <- glm(everwrk~ ukMaritalStatus, data = NH11_edit, family = binomial)
        pred8 <- predict(mod8, type = "response")
        table(NH11_edit$everwrk, pred8 >= 0.86)
        (12124+5)/(14040) # Accuracy of 86%
        
##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.
