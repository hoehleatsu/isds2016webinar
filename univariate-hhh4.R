######################################################################
## R code for the univariate analysis of the 19 September 2016 ISDS
## Webinar on 'Multivariate count time series modelling for
## surveillance data'. Extracted with Stangle from slide file
## jss.Rnw.
##
## Authors:
##  Leonhard Held (U of Zurich, Switzerland)
##  Michael Höhle (Stockholm U, Sweden)
##
## Note:
## The code for the multivariate analysis is directly available
## as package vignette 'hhh4: Endemic-epidemic modeling of areal count
## time series' available as
## https://cran.r-project.org/web/packages/surveillance/vignettes/hhh4_spacetime.pdf
## Alternatively, the code can be viewed directly as file
##
##                     hhh4_spacetime.Rnw
##
## from the package repository, i.e. as https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/vignettes/hhh4_spacetime.Rnw?root=surveillance
##
######################################################################

###################################################
### code chunk number 1: jss.Rnw:8-10
###################################################
options(width=90,prompt="R> ",xtable.comment=FALSE)
library("surveillance")


###################################################
### code chunk number 2: measlesWeserEms_components
###################################################
## extract components from the measlesWeserEms sts object
data("measlesWeserEms")
#Form the aggregated series over all counties
counts.total <- aggregate(measlesWeserEms, by="unit")


###################################################
### code chunk number 4: jss.Rnw:99-106
###################################################
#Base model: time constant \nu_t, time constant \lambda
measlesModel_uni_0 <- list(end = list(f = ~1),
                           ar = list(f = ~1),
                           family = "NegBin1")
#Fit model using the hhh4 function and study output
measlesFit_uni_0 <- hhh4(stsObj = counts.total, control = measlesModel_uni_0)
summary(measlesFit_uni_0, idx2Exp = TRUE, maxEV = TRUE)


###################################################
### code chunk number 5: jss.Rnw:108-111
###################################################
#end=~1, epi=~0,
measlesFit_uni_end1epi0 <- update(measlesFit_uni_0, ar = list(f= ~0))
summary(measlesFit_uni_end1epi0, idx2Exp = TRUE, maxEV = TRUE)


###################################################
### code chunk number 6: jss.Rnw:113-118
###################################################
#end=~1+S, epi=~0,
measlesFit_uni_end1Sepi0 <- update(measlesFit_uni_0, end = list(f = addSeason2formula(~1 , period = counts.total@freq)), ar = list(f= ~0))
#end=~1+S, epi=~1,
measlesFit_uni_end1Sepi1 <- update(measlesFit_uni_0, end = list(f = addSeason2formula(~1 , period = counts.total@freq)))
print(summary(measlesFit_uni_end1Sepi1, idx2Exp = TRUE, amplitudeShift = TRUE, maxEV = TRUE))


###################################################
### code chunk number 7: jss.Rnw:127-130
###################################################
#Select colors for the three components
mycol <- c("blue", "orange", "red")#endmic, autoregressive, neighbour
plot(measlesFit_uni_end1epi0, type="fitted", names="end= ~1, ar= ~0", col=mycol)


###################################################
### code chunk number 8: jss.Rnw:132-133
###################################################
plot(measlesFit_uni_end1Sepi0, type="fitted", names="end= ~1+S, ar= ~0", col=mycol)


###################################################
### code chunk number 9: jss.Rnw:136-137
###################################################
plot(measlesFit_uni_0, type="fitted", names="end= ~1, ar= ~1", col=mycol)


###################################################
### code chunk number 10: jss.Rnw:139-140
###################################################
plot(measlesFit_uni_end1Sepi1, type="fitted", names="end= ~1+S, ar= ~1", col=mycol)


###################################################
### code chunk number 11: jss.Rnw:145-148
###################################################
#Model: end=~1+S,ar=~1+S
measlesFit_uni_end1Sepi1S <- update(measlesFit_uni_end1Sepi1,
  ar = list(f = addSeason2formula(~1 , period = counts.total@freq)))


###################################################
### code chunk number 12: jss.Rnw:151-152
###################################################
plot(measlesFit_uni_end1Sepi1S, type="fitted", names="end= ~1+S, ar= ~1+S", col=mycol)


###################################################
### code chunk number 13: jss.Rnw:155-157 (eval = FALSE)
###################################################
## #Show fitted seasonality components
## plot(measlesFit_uni_end1Sepi1S, type = "season", components = c("end","ar"))


###################################################
### code chunk number 14: jss.Rnw:160-165
###################################################
#Compare models using AIC (lowest is best)
AIC(measlesFit_uni_end1epi0,
    measlesFit_uni_end1Sepi0,
    measlesFit_uni_0,
    measlesFit_uni_end1Sepi1S)


