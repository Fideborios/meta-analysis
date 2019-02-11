# set your working directory using setwd
setwd("D:/CURSUS/Meta analyse/Practica")
library(foreign)
library(metafor)
library(meta)
#############################################
##            EXERCISE III                 ##
#############################################


############################################
##   Import data and make calculations    ##
############################################

chol = read.dta("Data/cholesterol.dta")

# Take a look at the data 
View(chol)
head(chol) # prints first 6 rows of data including headers
summary(chol)

############################################
##   Perform the analysis - FE Model      ##
############################################
res1 = rma(yi=logor, vi=varlogor, data=chol , method="FE", measure="OR")
res1 # point estimate 0.8246 (0.7720, 0.8807)

predict(res1, transf=exp, digits=4) # back-transformation 

############################################
##    Plot the result - Forestplot        ##
############################################

forest(res1, atransf=exp,  slab = chol$trialname, showweights=TRUE)
weights(res1) # study contribution 

############################################
##   Perform the analysis - RE Model      ##
############################################

res2 = rma(yi=logor, vi=varlogor, data=chol , method="REML", measure="OR")
res2 # point estimate 0.8033 (0.7233, 0.9033)

predict(res2, transf=exp, digits=4)

weights(res2)

## Model summary /output 

# tau^2 (estimated amount of total heterogeneity): 0.0313 (SE = 0.0204)
# tau (square root of estimated tau^2 value):      0.1770
# I^2 (total heterogeneity / total variability):   47.24%
# H^2 (total variability / sampling variability):  1.90


# I^2: percentage of "unexplained" variance; 30-60% is considered moderate heterogeneity



############################################
##   Calculate prediction interval        ##
############################################

# point estimate ? 1.96*tau
# exp(-0.2128 ? 1.96*0.1770) = (0.5714, 1.1435)

predict(res2, transf=exp, digits=4) # (0.5616, 1.1635)

############################################
##   Forestplot with prediction interval  ##
############################################


forest(res2,  atransf=exp,
       slab = chol$trialname, addcred=TRUE, col=c("blue", "red"))

############################################
##      Moderator analysis                ##
############################################

res3 = rma(y=logor, vi=varlogor, mods = ~ factor(ihdentry) - 1,  data=chol)
res3

############################################
##      Meta-regression                   ##
############################################

res4 = rma(y=logor, vi=varlogor, mods = ~ cholreduc , data=chol)
res4

predict(res4, transf=exp, digits=4)

forest(res4,  atransf=exp,
       slab = chol$trialname)

## Model output

#             estimate      se     zval    pval    ci.lb    ci.ub    
# intrcpt      0.1467  0.1237   1.1862  0.2356  -0.0957   0.3892    
# cholreduc   -0.5057  0.1651  -3.0626  0.0022  -0.8293  -0.1821  **

# Visualize the effect of cholesterol reduction 

### calculate point sizes by rescaling the standard errors
wi = 1/sqrt(chol$varlogor)
size = 0.5 + 3.0 * (wi - min(wi))/(max(wi) - min(wi))

preds = predict(res4, transf=exp, newmods=c(0:2), digits=4)


### plot the odds ratios against absolute latitude
plot(chol$cholreduc, exp(chol$logor), pch=19, cex=size, 
     xlab="Cholesterol reduction", ylab="Odds Ratio",
     las=1, bty="l", log="y")

### add predicted values (and corresponding CI bounds)
lines(0:2, preds$pred)
lines(0:2, preds$ci.lb, lty="dashed")
lines(0:2, preds$ci.ub, lty="dashed")

### dotted line at OR=1 (no difference between groups)
abline(h=1, lty="dotted")

ids <- c(12,22,23,28)
pos <- c(3,3,1,1)
text(chol$cholreduc[ids], exp(chol$logor)[ids], ids, cex=0.9, pos=pos)


############################################
##      Meta-regression continued         ##
############################################

res5 = rma(y=logor, vi=varlogor, mods = ~1 , data=chol)
res5 # regression using only the intercept 
# pseudo R^2 value

# Include different type of intervention as covariate
res6 = rma(y=logor, vi=varlogor, mods = ~ factor(intervention) -1  , subset = chol$intervention != "Surgery" , data=chol)
res6

# Included both intervention and cholesterol reduction as covariates 
res7 = rma(y=logor, vi=varlogor, mods = ~ factor(intervention) -1 + cholreduc  , subset = chol$intervention != "Surgery" , data=chol)
res7


############################################
##           Funnelplot                   ##
############################################

# Fit first a RE model without any covariates
res2 = rma(yi=logor, vi=varlogor, data=chol , method="REML", measure="OR")

### set up 2x2 array for plotting
par(mfrow=c(2,2))

# draw funnel plots
funnel(res2, main="Standard Error")
funnel(res2, yaxis="vi", main="Sampling Variance")
funnel(res2, yaxis="seinv", main="Inverse Standard Error")
funnel(res2, yaxis="vinv", main="Inverse Sampling Variance")

dev.off()

# Similar to forcenull in STATA
funnel(res2, main="Standard Error", refline=0)


############################################
##        Egger's test                    ##
############################################

res2 = rma(yi=logor, vi=varlogor, data=chol , method="REML", measure="OR")

regtest(res2)

