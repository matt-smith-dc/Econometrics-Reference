
library(dplyr)
library(datasets)
library(rpart)
library(mlbench)


#############################################################################
############################ Basic OLS
data("BostonHousing")
help("BostonHousing")

summary(BostonHousing)
summary(log(BostonHousing$medv))

lm.model <- lm(medv ~ crim + zn, data=BostonHousing)

#We see negative effect of crime
#Positive effect of zoning proportion
summary(lm.model)

#1% increase in crime rate leads to a 2.3% drop in home value
#1% increase in zoning proportion leads to 0.47% increase in home value
summary(lm(log(medv*1000) ~ crim + zn, data=BostonHousing))

#We see potential bias in previous OLS model by omitting room count
summary(lm(log(medv*1000) ~ crim + zn + rm, data=BostonHousing))

#The correlations suggest OVB in 1st two regressions
cor(BostonHousing$crim, BostonHousing$rm)
cor(BostonHousing$zn, BostonHousing$rm)


#############################################################################
############################# Clustered Standard Errors

#install.packages(c('miceadds', 'webuse'))
library(miceadds)
library(webuse)

nlswork.orig <- webuse('nlswork')


m1 <- lm(ln_wage ~ age + tenure + union + tenure:union + idcode, data=nlswork)
m2 <- lm.cluster(ln_wage ~ age + tenure + union + tenure:union + idcode,
                 cluster = 'idcode',
                 data = nlswork)
reg.summary <- summary(m1)
clstr.summary <- summary(m2)

#Standard OLS SEs are too small (compared to clustered)
# but are still significant even clustered
reg.summary$coefficients
clstr.summary


#############################################################################
############################# Fixed Effects
install.packages('plm')
library(plm)

data(Crime)
#Effect of log industry wages no longer significant
summary(lm(crmrte ~ avgsen + lpolpc + lwser + lpctymle, data=Crime))
summary(plm(crmrte ~ avgsen + lpolpc + lwser + lpctymle, data=Crime, method="within"))
