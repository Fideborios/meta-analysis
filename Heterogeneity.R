## ----echo=F, warning=FALSE, message=FALSE--------------------------------
library(knitr)
opts_chunk$set(fig.width=9, fig.height=6, fig.path='Figs/',
               warning=FALSE, message=FALSE, fig.pos = "H", comment = "")


## ------------------------------------------------------------------------
library("readxl")
Ketotifen =  read_xlsx("Data/Ketotifen.xlsx")

## ------------------------------------------------------------------------
head(Ketotifen, 
     n =10 # number of row we want to print
     )

## ------------------------------------------------------------------------
library("meta")

## ----eval=FALSE----------------------------------------------------------
## 
## 
## res.RE = metabin(event.e = .. ,      ## Events of treated
##                  n.e = .. ,          ## Total number of treated
##                  event.c = .. ,      ## Events of control
##                  n.c = ..,           ## Total number of treated
##                  sm = ..,            ## Effect size
##                  method = ..,        ## weight calculation method
##                  data = ..,          ## the data-set
##                  studlab = ..,       ## The trial names
##                  method.tau=..       ## tau estimator method
## 
##                  )
## 
## forest(res.RE)
## 
## 

## ----echo=FALSE----------------------------------------------------------

res.RE = metabin(event.e = Ee ,      ## Events of treated
                 n.e = Ne ,          ## Total number of treated
                 event.c = Ec ,      ## Events of control
                 n.c = Nc,           ## Total number of treated
                 sm = "RR",          ## Effect size
                 method = "Inverse", ## weight calculation method
                 data = Ketotifen,   ## the data-set
                 studlab = study,    ## The trial names
                 method.tau = "EB"
                 )





## ----eval=FALSE----------------------------------------------------------
## 
## res.SA = metabin(event.e = .. ,      ## Events of treated
##                  n.e = .. ,          ## Total number of treated
##                  event.c = .. ,      ## Events of control
##                  n.c = ..,           ## Total number of treated
##                  sm = ..,            ## Effect size
##                  method = ..,        ## weight calculation method
##                  data = ..,          ## the data-set
##                  studlab = ..,       ## The trial names
##                  method.tau=..,      ## tau estimator method
##                  byvar = ..,         ## The splitting variable
##                  comb.fixed = F      ## A logical variable (True/False) indicating
##                                      ## whether a fixed effect meta-analysis should be conducted.
##                  )
## 
## forest(res.SA)
## 

## ----echo=FALSE----------------------------------------------------------

res.SA = metabin(event.e = Ee ,      ## Events of treated
                 n.e = Ne ,          ## Total number of treated
                 event.c = Ec ,      ## Events of control
                 n.c = Nc,           ## Total number of treated
                 sm = "RR",          ## Effect size
                 method = "Inverse", ## weight calculation method
                 data = Ketotifen,   ## the data-set
                 studlab = study,    ## The trial names
                 method.tau="EB",    ## tau estimator method
                 byvar =  blind,     ## The splitting variable  
                 comb.fixed = F      ## A logical variable (True/False) indicating 
                                     ## whether a fixed effect meta-analysis should be conducted. 
                 )

# forest(res.SA)


## ----eval=FALSE----------------------------------------------------------
## res.MR.SA =metareg(x = .... , ## an object of class meta
##                   formula = ....,
##                   hakn = T)

## ----eval=FALSE----------------------------------------------------------
## 
## res.SA.2 = metabin(event.e = Ee ,      ## Events of treated
##                  n.e = Ne ,          ## Total number of treated
##                  event.c = Ec ,      ## Events of control
##                  n.c = Nc,           ## Total number of treated
##                  sm = "RR",          ## Effect size
##                  method = "Inverse", ## weight calculation method
##                  data = Ketotifen,   ## the data-set
##                  studlab = study,    ## The trial names
##                  method.tau="EB",    ## tau estimator method
##                  byvar =  blind,     ## The splitting variable
##                  comb.fixed = F,    ## A logical variable (True/False) indicating
##                                      ## whether a fixed effect meta-analysis should be conducted.
##                  tau.common = T
##                  )
## 
## # forest(res.SA)
## 

## ------------------------------------------------------------------------
library(metafor)
dat <- dat.colditz1994
head(dat)

## ------------------------------------------------------------------------
res.RE =  metabin(event.e = tpos,
                  n.e =  tpos+tneg ,
                  event.c =  cpos,
                  n.c =  cpos+cneg,
                  data = dat, 
                  studlab = paste(author, year), 
                  sm = "RR")

res.RE

## ------------------------------------------------------------------------
res.RE.MR = metareg(res.RE, ablat)

## ------------------------------------------------------------------------
bubble(res.RE.MR)

