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
basecon1_sample <- basecon1[sample(1:1000000, 100000),]
# Assumption 2: E(ei|x)=0

reg_educ <- lm(basecon1_sample$LWKLYWGE ~ basecon1_sample$EDUC)
mean(reg_educ$residuals)

# Assumption 3: Homoscedasticity
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(reg_educ)
summary(reg_educ)$sigma
educ <- basecon1_sample$EDUC

# Adding regressors: AGE
reg_educ_age <- lm(basecon1_sample$LWKLYWGE ~ basecon1_sample$EDUC + basecon1_sample$AGE)
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(reg_educ_age)
summary(reg_educ_age)

# Adding regressors: AGE & RACE
reg_educ_age_race <- lm(basecon1_sample$LWKLYWGE ~ basecon1_sample$EDUC + basecon1_sample$AGE + basecon1_sample$RACE)
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(reg_educ_age_race)
summary(reg_educ_age_race)


# Adding regressors: AGE, RACE & REGION (boolean)
reg_educ_age_race_region <- lm(basecon1_sample$LWKLYWGE ~ basecon1_sample$EDUC + basecon1_sample$AGE + 
                                 basecon1_sample$RACE + basecon1_sample$ENOCENT + basecon1_sample$ESOCENT + 
                                 basecon1_sample$MIDATL + basecon1_sample$MT + basecon1_sample$NEWENG + 
                                 basecon1_sample$SOATL + basecon1_sample$WNOCENT + basecon1_sample$WSOCENT)
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(reg_educ_age_race_region)
summary(reg_educ_age_race_region)




## Econometria 1 - Problema 13 - Multicolinearidade e teste de significância

# Utilizando as variáveis explicativas contidas no modelo exposto no Problema 9, temos: 

reg_p13 <- lm(basecon1$LWKLYWGE ~ basecon1$EDUC + basecon1$AGE + I(basecon1$AGE^2))
summary(reg_p13)

library(corpcor)
cor2pcor(cov(data_ggp)) # tabela de correlação entre variáveis explicativas

library(GGally)
data_ggp <- data.frame(basecon1$LWKLYWGE, basecon1$EDUC, basecon1$AGE)
ggpairs(data_ggp[sample(1:10000, 10000),]) # plot entre v. explicativas


reg_educ_age_race_region <- lm(basecon1$LWKLYWGE ~ basecon1$EDUC + basecon1$AGE + I(basecon1$AGE^2) + basecon1$MARRIED +
                                 basecon1$RACE + basecon1$ENOCENT + basecon1$ESOCENT + 
                                 basecon1$MIDATL + basecon1$MT + basecon1$NEWENG + 
                                 basecon1$SOATL + basecon1$WNOCENT + basecon1$WSOCENT)


summary(reg_educ_age_race_region)

# Variance inflation factor (VIF)
car::vif(reg_educ_age_race_region)


## Econometria 1 - Problema 14

# F-test

library(car)
basecon1 <- read_dta("G:/My Drive/FGV EESP/3o SEMESTRE/Econo I/Data/basecon1.dta")

AGEQQ <- I(basecon1_S$AGE^2)
reg14 <- lm(LWKLYWGE ~ EDUC + AGE + AGEQ + MARRIED +
              RACE + ENOCENT + ESOCENT + 
              MIDATL + MT + NEWENG + 
              SOATL + WNOCENT + WSOCENT, data = basecon1)

# Teste de hipótese 1: nenhum efeito de EDUC, AGE e RACE.
car::linearHypothesis(reg14, c("EDUC=0", "AGE=0", "AGEQ=0", "RACE=0"))

# Teste de hipótese 2: Efeito de RACE = 2xAGE.
car::linearHypothesis(reg14, c("RACE=2*AGE"))

## Econometria 1 - Problema 15

# F-test

library(car)
basecon1 <- read_dta("G:/My Drive/FGV EESP/3o SEMESTRE/Econo I/Data/basecon1.dta")

AGEQ <- I(basecon1$AGE^2)
reg15_1 <- lm(LWKLYWGE ~ EDUC + AGE + AGEQQ + MARRIED + EDUC*MARRIED + AGE*MARRIED + AGEQQ*MARRIED, data = basecon1)

basecon1_S <- subset(basecon1, MARRIED==1)
AGEQQ_S <- I(basecon1_S$AGE^2)

reg15_S <- lm(LWKLYWGE ~ EDUC + AGE + AGEQQ_S, data = basecon1_S)
summary(reg15_S)

# Teste de hipótese - F test: 
car::linearHypothesis(reg15_1, c("MARRIED=0", "EDUC:MARRIED=0", "AGE:MARRIED=0", "AGEQQ:MARRIED=0"))

car::linearHypothesis(reg14, c("ENOCENT=0", "ESOCENT=0", 
                                   "MIDATL=0", "MT=0", "NEWENG=0", 
                                   "SOATL=0", "WNOCENT=0", "WSOCENT=0"))


simple_reg <- lm(basecon1$LWKLYWGE ~ basecon1$EDUC)
summary(simple_reg)
