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

cor(data.frame(basecon1_old$EDUC, basecon1_old$AGE, basecon1_old$MARRIED, basecon1_old$SAT))

proxy_reg_old <- lm(LWKLYWGE ~ EDUC + AGE + AGEQQ + MARRIED + 
                EDUC*MARRIED + AGE*MARRIED + AGEQQ*MARRIED + ENOCENT + ESOCENT + 
                  MIDATL + MT + NEWENG + 
                  SOATL + WNOCENT + WSOCENT, data = basecon1_old)

summary(proxy_reg_old)

gtsummary::tbl_regression(proxy_reg)

stargazer(proxy_reg, single.row=TRUE)

# Problema 26 - Seemingly Unrelated Regression
library(systemfit)

wage_eq <- Tutorial26$annearn ~ Tutorial26$union + Tutorial26$exper + Tutorial26$educ + Tutorial26$age + Tutorial26$married +
            Tutorial26$age + (Tutorial26$age)^2 

vac_eq <- Tutorial26$vacdays ~ Tutorial26$union + Tutorial26$tenure + Tutorial26$tenuresq +  
  Tutorial26$exper + Tutorial26$educ + Tutorial26$age + Tutorial26$married 

sick_eq <- Tutorial26$sicklve ~ Tutorial26$union + Tutorial26$tenure + Tutorial26$tenuresq +  
  Tutorial26$exper + Tutorial26$educ + Tutorial26$age + Tutorial26$married 

insur_eq <- Tutorial26$insur ~ Tutorial26$union + Tutorial26$tenure + Tutorial26$tenuresq +  
  Tutorial26$exper + Tutorial26$educ + Tutorial26$age + Tutorial26$married 

pension_eq <- Tutorial26$pension ~ Tutorial26$union + Tutorial26$tenure + Tutorial26$tenuresq +  
  Tutorial26$exper + Tutorial26$educ + Tutorial26$age + Tutorial26$married 

sys <- c(wage_eq, vac_eq, sick_eq, insur_eq, pension_eq)

# SUR
sur_sys <- summary(systemfit(sys, method="SUR"))

ols_sys <- summary(systemfit(sys, method="OLS"))

ols_sys

summary(lm(vac_eq))


# Problema 28 - SEM
library(haven)
data28 <- read_dta("G:/My Drive/FGV EESP/3o SEMESTRE/Econo I/Data/Tutorias27-28.DTA")
library(sandwich)
library(systemfit)

supply <- data28$hours ~ data28$lwage + data28$educ + data28$age + data28$kidslt6 + data28$kidsge6 + 
          data28$faminc + data28$huswage

demand <- data28$lwage ~ data28$hours + data28$educ + data28$exper + data28$expersq

instr1 <- ~ data28$age + data28$kidslt6 + data28$kidsge6 + 
  data28$faminc + data28$huswage + data28$exper + data28$educ + data28$expersq
instr2 <- ~ data28$exper + data28$expersq + data28$age + data28$kidslt6 + data28$kidsge6 + 
  data28$faminc + data28$huswage + data28$educ

instr <- ~ data28$exper + data28$expersq + data28$age + data28$kidslt6 + data28$kidsge6 + 
  data28$faminc + data28$huswage + data28$educ

 
sxd <- c(supply, demand)

twosls_sxd <- systemfit(sxd, method = "2SLS", inst = instr)
summary(twosls_sxd)          

threesls_sxd <- systemfit(sxd, method = "3SLS", inst = instr)
summary(threesls_sxd)        
                      
                      