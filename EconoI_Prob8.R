## Econometria 1 - Problema 8

# Testing Assumptions of Linear Regression

library(dplyr)
library(haven)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(plotrix)
library(het.test)
basecon1 <- read_dta("~/Google Drive/FGV EESP/3o SEMESTRE/ECONOMETRIA I/Data/basecon1.dta")
basecon1_sample <- basecon1_old[sample(1:1000000, 100000),]

library(haven)
basecon1_old <- read_dta("G:/My Drive/FGV EESP/3o Semestre/Econo I/Data/basecon1_old.dta")
View(basecon1_old)

# Assumption 2: E(ei|x)=0

reg_educ <- lm(basecon1_sample$LWKLYWGE ~ basecon1_sample$EDUC)
mean(reg_educ$residuals)
plot(reg_educ)
# Assumption 3: Homoscedasticity
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(reg_educ)
title("EDUC")
summary(reg_educ)$sigma
educ <- basecon1_sample$EDUC

# Adding regressors: AGE
reg_educ_age <- lm(basecon1_sample$LWKLYWGE ~ basecon1_sample$EDUC + basecon1_sample$AGE)
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(reg_educ_age)
summary(reg_educ_age)
title("EDUC, AGE")

# Adding regressors: AGE & RACE
reg_educ_age_race <- lm(basecon1_sample$LWKLYWGE ~ basecon1_sample$EDUC + basecon1_sample$AGE + basecon1_sample$RACE)
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(reg_educ_age_race)
summary(reg_educ_age_race)
title("EDUC, AGE & RACE")

# Adding regressors: AGE, RACE & REGION (boolean)
reg_educ_age_race_region <- lm(basecon1_sample$LWKLYWGE ~ basecon1_sample$EDUC + basecon1_sample$AGE + 
                                 basecon1_sample$RACE + basecon1_sample$ENOCENT + basecon1_sample$ESOCENT + 
                                 basecon1_sample$MIDATL + basecon1_sample$MT + basecon1_sample$NEWENG + 
                                 basecon1_sample$SOATL + basecon1_sample$WNOCENT + basecon1_sample$WSOCENT)
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(reg_educ_age_race_region)
summary(reg_educ_age_race_region)
title("EDUC, AGE, RACE & REGION")

basecon1_sample$AGEQQ <- (basecon1_sample$AGE)^2

reg_beamer <- lm(LWKLYWGE ~ EDUC + AGE + AGEQQ + RACE, data = basecon1_sample)
summary(reg_beamer)
stargazer::stargazer(reg_beamer)

reg_simples <- lm(LWKLYWGE ~ EDUC, data = basecon1_sample)
abline(reg_simples)

with(basecon1_sample,plot(EDUC,LWKLYWGE))
abline(reg_simples)

ggplot(data = basecon1_sample, aes(x = EDUC, y = LWKLYWGE)) + geom_point() + geom_smooth(method = "lm") + theme_few()

stargazer::stargazer(reg_simples)

reg_sat <- lm(LWKLYWGE ~ EDUC + AGE + AGEQQ + RACE + SAT, data = basecon1_sample)



basecon1_sample$SAT

