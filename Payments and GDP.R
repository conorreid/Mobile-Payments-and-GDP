#Conor Reid
#Looking at the link between mobile payment users and GDP growth
#first, let's import the data
Payments.and.GDP <- read.csv("~/R Stuff/Mobile Payments and GDP/Payments and GDP.csv")
#now, let's graph it
plot(Payments.and.GDP$GDP.Growth, Payments.and.GDP$Mobile.Payment.Users)
#looks like there might be some sort of linear relationship, we'll see
#let's do a preliminary basic regression
OLSmodel <- lm(GDP.Growth ~ Mobile.Payment.Users, data=Payments.and.GDP)
summary(OLSmodel)
abline(OLSmodel)
#a fairly high R squared, and with a small p-value it's definitely significant
#however, this is panel data, so a fixed effect or random effect model is better
#let's make a fixed effect model first
#to do that, however, we have to import a new library
library(plm)
#now let's construct our model
FEmodel <- plm(GDP.Growth ~ Mobile.Payment.Users, data=Payments.and.GDP, index=c("Country", "Year"), model="within")
summary(FEmodel)
#accounting for fixed effects, the model collapses under its own weight
#it looks like the fixed effects accounted for all the variation
#just to be sure that that model was approriate, let's do an F-test for FE
pFtest(FEmodel, OLSmodel)
#looks like there are significant fixed effects
#this model is useless
#hooray!