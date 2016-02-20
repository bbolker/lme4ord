library(lme4ord)
library(multitable)
library(lme4)
library(ape)

nSites <- 10
nSpec <- 100
form <- y ~ x * z +
    (z | sites) + 
    edge(1     | species, phy = phy) +
    edge(0 + x | species, phy = phy)
dataList <- dims_to_vars(data.list(y = 1 * (matrix(rnorm(nSites * nSpec), nSites, nSpec) > 0),
                                   x = rnorm(nSites),
                                   z = rnorm(nSpec),
                                   dimids = c("sites", "species")))
phy <- compute.brlen(rtree(n = nSpec), method = "Grafen", power = 0.8)
phy$tip.label <- dimnames(dataList)$species
dataFrame <- as.data.frame(dataList)

parsedForm <- strucParseFormula(form, dataFrame, addArgs = list(phy = phy))

beta <- rnorm(ncol(parsedForm$fixed))
u <- rnorm(nrow(parsedForm$Lambdat))
dataFrame$y <- with(parsedForm, {
    fe <- as.numeric(fixed %*% beta)
    re <- as.numeric(t(Lambdat * Zt) %*% u)
    mu <- plogis(fe + re)
    rbinom(nSites * nSpec, 1, mu)
})

(gm <- strucGlmer(form, dataFrame, binomial, list(phy = phy)))
beta
