# Problema 28 - SEM

data28 <- read_dta("G:/My Drive/FGV EESP/3o SEMESTRE/Econo I/Data/Tutorias27-28.DTA")

library(sandwich)
library(systemfit)
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

# Defining Supply and Demand equations

supply <- data28$hours ~ data28$lwage + data28$educ + data28$age + data28$kidslt6 + data28$kidsge6 + 
  data28$faminc + data28$huswage

demand <- data28$lwage ~ data28$hours + data28$educ + data28$exper + data28$expersq

# Defining instruments 

instr1 <- ~ data28$educ + data28$age + data28$kidslt6 + data28$kidsge6 + 
  data28$faminc + data28$huswage + data28$exper # + data28$expersq
instr2 <- ~  data28$educ + data28$exper + data28$expersq + data28$age + data28$kidslt6 + data28$kidsge6 + 
  data28$faminc + data28$huswage

sxd <- c(supply, demand)

# 2SLS
twosls_sxd <- systemfit(sxd, method = "2SLS", inst = list(instr1, instr2))
summary(twosls_sxd)          

# 3SLS
threesls_sxd <- systemfit(sxd, method = "3SLS", inst = list(instr1, instr2))
summary(threesls_sxd)        
