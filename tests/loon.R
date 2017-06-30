library(lme4ord)

## loon survey data ... (from where?
loon <- read.csv(system.file("extraData", "loon.csv", package = "lme4ord"))

## basic plot
loon <- within(loon, prop <- round(lakes * (percent/100)) / lakes)
with(loon, plot(year, prop, type = "o", las = 1, bty="l"))
summary(m <- glm(prop ~ year, binomial, loon, lakes))

corObj <- nlme:::Initialize(nlme:::corAR1(0, form = ~ year), loon)
(pform <- strucGlmer(prop ~ scale(year) +
                     nlmeCorStruct(1, corObj = corObj, sig = 1),
                     loon, numModularSteps = 1))

## set up deviance function: *MODIFIES pform!*
dfun <- strucMkDevfun(pform, family = binomial, weights = loon$lakes)
dfun(pars(pform))

## where does the family information enter the strucGlmer fit??
(gm <- strucGlmer(pform, weights = loon$lakes))

image(crossprod(relCovFact(gm)))
plot(plogis(fitted(gm)), loon$prop)
abline(a = 0, b = 1)
sigma(gm)
sigma(pform)




