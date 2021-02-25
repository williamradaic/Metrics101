library(haven)
library(dplyr)
library(haven)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(plotrix)
library(het.test)
library(lmtest)
library(bstats)
library(MASS)
library(sfsmisc)
library(robust)
library(gtsummary)

basecon1 <- file.choose()

# Econometria 1 - Problema 18

# OLS Model

AGEQQ <- I(basecon1$AGE^2)
ols_reg <- lm(LWKLYWGE ~ EDUC + AGE + AGEQQ + MARRIED + 
                EDUC*MARRIED + AGE*MARRIED + AGEQQ*MARRIED, data = basecon1)

res2 <- (ols_reg$residuals)^2

# Residuals plot - R-base
plot(basecon1$EDUC, res2)

## ggplot - residuals
ggplot(data = basecon1, aes(y = res, x = EDUC)) + geom_point(col = 'blue') + 
  geom_abline(slope = 0)

# The Breusch-Pagan Test

ols_ssr <- lm(res2 ~ EDUC + AGE + AGEQQ + MARRIED + 
                EDUC*MARRIED + AGE*MARRIED + AGEQQ*MARRIED, data = basecon1)

summary(ols_ssr)

# F-statistic: 425.6 on 7 and 1063626 DF,  p-value: < 2.2e-16 -> reject Ho: the data are not homoscedastic

bptest(ols_reg) # BP = 2970.8, df = 7, p-value < 2.2e-16


# Problema 19 - WLS & Robust regression

# Correcting std error

sandwich(ols_reg)



##  = psi.bisquare -- command to assign weighting function
  ## Converged in 5 iterations

f.robftest(robust_reg) ## Teste conjunto (B=0)

f.robftest(robust_reg, var = "EDUC") # Testando apenas bEDUC = 0

## WLS model

EDUCQQ <- I(basecon1$EDUC)^2

ols_ssr$coefficients

X_wls <- data.frame(basecon1$EDUC, basecon1$AGE, AGEQQ, basecon1$MARRIED, 
                    basecon1$EDUC*basecon1$MARRIED, basecon1$AGE*basecon1$MARRIED, (AGEQQ*basecon1$MARRIED))

wls_reg <- lm(LWKLYWGE ~ EDUC + AGE + AGEQQ + MARRIED + 
                EDUC*MARRIED + AGE*MARRIED + AGEQQ*MARRIED, data = basecon1) # ??????

summary(wls_reg)

?rlm
basecon1


# Econometria 1 - Problema 22
# Proxy variables 

basecon1_old <- read_dta("G:/My Drive/FGV EESP/3o SEMESTRE/Econo I/Data/basecon1_old.dta")

proxy_reg <- lm(LWKLYWGE ~ EDUC + AGE + AGEQQ + MARRIED + 
                EDUC*MARRIED + AGE*MARRIED + AGEQQ*MARRIED + SAT, data = basecon1_old)

summary(proxy_reg)

gtsummary::tbl_regression(proxy_reg)

stargazer(proxy_reg, single.row=TRUE)

