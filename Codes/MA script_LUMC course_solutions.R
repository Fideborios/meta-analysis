#########################################
##      Install and load packages      ##
#########################################

install.packages(c("foreign", "metafor", "meta"))

## load libraries
library(foreign)
library(metafor)
library(meta)


# set your working directory
setwd("F:/Meta-analysis course LUMC")


#############################################
##              EXERCISE I                 ##
#############################################


############################################
##   Import data and make calculations    ##
############################################

diuretic = read.dta("diuretic.dta")

# Take a look at the data 
View(diuretic)
head(diuretic) # prints first 6 rows of data including headers


# calculate effect sizes - MANUALLY
# 1. calculate number of mothers without pre-eclampsia
hc = with(diuretic,nc-pec)
ht = with(diuretic,nt-pet)

# 2. calculate odds ratios, log ORs and SEs
or = with(diuretic, (pet/ht)/(pec/hc))
logor = log(or)
selogor = with(diuretic, sqrt(1/pet + 1/pec + 1/ht + 1/hc))

# calculate effect sizes - using function ESCALC
diuretic = escalc(ai=pet, n1i=nt, ci=pec, n2i=nc, data=diuretic, measure="OR") # logOR

save(diuretic, file="diuretic.RData")
load(file="diuretic.RData")

############################################
##       Perform the analysis             ##
############################################
res1 = rma(yi, vi, data=diuretic, method="FE", measure="OR")
res1

# # The output provides important information for reporting the results of the MA; let's look section 
# by section at the relevant output.

# Fixed-Effects Model (k = 9)

# This line tells us we have used a fixed effect model with 9 studies; when a random-effects model is used
# the type of between-study variance ??^2 estimator is specified.

# Test for Heterogeneity: Q(df = 8) = 27.2649, p-val = 0.0006

# This line shows the value of Q-statistic with degrees of freedom; the weighted deviations about the summary effect size
# Large vlues of Q reflect greater between-study heterogeneity. The p-value =0.0006 suggests that the included studies
# do not share a common effect size 

# Model Results:
# The "estimate" provides the estimated model coefficient (i.e., the summary effect size) with standard error ("se")
# The "z-value" is the corresponding test statistic, "pval" the corresponding p-value etc.

predict(res1, transf=exp, digits=4) # back-transformation 
names(res1) # returns all components of the object
res1$b

############################################
##    Plot the result - Forestplot        ##
############################################

forest(res1, digits=2) # default plot
args(forest.rma)

## Some "standard" modifications
# order:  sort by "obs", "fit", "prec", etc
# slab:   change study labels
# ilab:   study information
# transf: apply function to effects
# psize:  symbol sizes

forest(res1, order = "obs",
       slab = diuretic$trialid, 
       ilab.xpos = res1$b - 1,
       refline = res1$b)

forest(res1, xlim=c(-16, 6), at=log(c(0.05, 0.25, 1, 4)), order = "obs", atransf=exp,
       slab = diuretic$trialid, 
       ilab = cbind(diuretic$pet, ht, diuretic$pec, hc),
       ilab.xpos=c(-9.5,-8,-6,-4.5), cex=0.75, ylim=c(-1,12),
       refline = res1$b)

op <- par(cex=1, font=4)

## switch to bold font
par(font=2)

### add column headings to the plot
text(c(-9.5,-8,-6,-4.5), 11, c("pet", "ht", "pec", "hc"))
text(c(-8.75,-5.25),     12, c("Treated", "Control"))
text(-16,                11, "Study",  pos=4)
text(6,                  11, "Odds Ratio [95% CI]", pos=2)


## set par back to the original settings
par(op)

############################################
##       Perform the analysis - RE MODEL  ##
############################################

res2 = rma(yi, vi, data=diuretic, method="REML", measure="OR") # default 
res2
# Available options for tau^2: DL, EB, HE, HS, ML, REML, SJ

predict(res2, transf=exp, digits=4) # back-transformation 

############################################
## Using number of events instead of effect 
# sizes                                  
############################################

 
res3 = metabin(pet, ht, pec, hc, data = diuretic, studlab = trialid, sm="RR", method="MH", comb.random=FALSE)
res3

# Odds ratio as outcome 
res4 = metabin(pet, ht, pec, hc, data = diuretic, studlab = trialid, sm="OR", method="Inverse", comb.random=TRUE)
res4



#############################################
##            EXERCISE II                  ##
#############################################


############################################
##   Import data and make calculations    ##
############################################

strep = read.dta("streptok.dta")

# Take a look at the data 
View(strep)
head(strep) # prints first 6 rows of data including headers



# calculate effect sizes - MANUALLY
# 1. calculate number of mothers without pre-eclampsia
h1 = with(strep,pop1-cases1)
h0 = with(strep,pop0-cases0)

save(strep, file="strep.RData")
load(file="strep.RData")


############################################
##       Perform the analysis - RE MODEL  ##
############################################

# Odds ratio as outcome 
res1 = metabin(cases1, h1, cases0, h0, data = strep, studlab = trialnam, sm="OR", method="MH", comb.random=TRUE)
res1

forest(res1,  order = "obs", atransf=exp,
       slab = strep$trialnam, 
       refline = res1$b)


# Restrict the analysis to year <= 1977
res2 = metabin(cases1, h1, cases0, h0, data = strep, subset = strep$year <= 1977, studlab = trialnam, sm="OR", method="Inverse", comb.random=TRUE)
res2

forest(res2,  order = "obs", atransf=exp,
       slab = strep$trialnam, 
       refline = res1$b)


# Exclude trial 22 - ISIS-2
res3 = metabin(cases1, h1, cases0, h0, data = strep, subset = strep$trialnam != "ISIS-2", studlab = trialnam, sm="OR", method="Inverse", comb.random=TRUE)
res3

forest(res3,  order = "obs", atransf=exp,
       slab = strep$trialnam, 
       refline = res1$b)

# Cumulative meta-analysis 
res4 = metacum(res1, sortvar=year, pooled="random")
res4
forest(res4) 

#############################################
##            EXERCISE III                 ##
#############################################


############################################
##   Import data and make calculations    ##
############################################

chol = read.dta("cholesterol.dta")

# Take a look at the data 
View(chol)
head(chol) # prints first 6 rows of data including headers

save(chol, file="chol.RData")
load(file="chol.RData")

############################################
##   Perform the analysis - FE Model      ##
############################################
res1 = rma(yi=logor, vi=varlogor, data=chol , method="FE", measure="OR")
res1 # point estimate 0.8246 (0.7720, 0.8807)

predict(res1, transf=exp, digits=4) # back-transformation 

############################################
##    Plot the result - Forestplot        ##
############################################

forest(res1, atransf=exp,  slab = chol$trialname)
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

# point estimate ± 1.96*tau
# exp(-0.2128 ± 1.96*0.1770) = (0.5714, 1.1435)

predict(res2, transf=exp, digits=4) # (0.5616, 1.1635)

############################################
##   Forestplot with prediction interval  ##
############################################


forest(res2,  atransf=exp,
       slab = chol$trialname, addcred=TRUE, col=c("blue", "red"))

############################################
##      Moderator analysis                ##
############################################

res3 = rma(y=logor, vi=varlogor, mods = ~ factor(ihdentry) - 1, data=chol)
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

