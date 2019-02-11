# Install and load the libraries
# load libraries
# install.packages(c("foreign", "metafor", "meta"))
library(foreign)
library(metafor)
library(meta)

# set your working directory using setwd
setwd("D:/CURSUS/Meta analyse/Practica")


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

############################################
##       Perform the analysis             ##
############################################
res1 = rma(yi =logor, sei =selogor, data=diuretic, method="FE", measure="OR")
res1

predict(res1, transf=exp, digits=4) # back-transformation 
#names(res1) # returns all components of the object

############################################
##    Plot the result - Forestplot        ##
############################################

forest(res1) # default plot
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
text(c(-9.5,-8,-6,-4.5), 11, c("pet+", "ht", "pec+", "hc"))
text(c(-8.75,-5.25),     12, c("Treated", "Control"))
text(-16,                11, "Study",  pos=4)
text(6,                  11, "Odds Ratio [95% CI]", pos=2)


## set par back to the original settings
par(op)

############################################
##       Perform the analysis - RE MODEL  ##
############################################

res2 = rma(yi =logor, sei =selogor, data=diuretic, method="REML", measure="OR") # default 
res2

predict(res2, transf=exp, digits=4) # back-transformation 

############################################
## Using number of events instead of effect 
# sizes                                  
############################################

# 
res3 = metabin(pet, ht, pec, hc, data = diuretic, studlab = trialid, sm="OR", method="MH",  comb.random=TRUE)
res3

