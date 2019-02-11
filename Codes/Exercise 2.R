# set your working directory using setwd
setwd("D:/CURSUS/Meta analyse/Practica")
library(foreign)
library(metafor)
library(meta)
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
