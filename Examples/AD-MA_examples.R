## ----echo=F, warning=FALSE, message=FALSE--------------------------------
library(knitr)
opts_chunk$set(fig.width=9, fig.height=6, fig.path='Figs/',
        echo=F, warning=FALSE, message=FALSE, fig.pos = "H", comment = "")


## ------------------------------------------------------------------------
# install.packages('metafor')
library(metafor)

## ------------------------------------------------------------------------
dat.bcg

## ----echo=F,message=FALSE,  out.width = "50%", out.height="50%"----------
library(knitr)
#img1 <- readPNG("Figures/2x2.png", native = TRUE, info = TRUE)
include_graphics("Figures/2x2.png")

## ---- fig.height=5, fig.cap="2x2 matrix", fig.width=5--------------------
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
dat

## ------------------------------------------------------------------------
Risk_exposed   =  with(dat, tpos/(tpos+tneg) )
Risk_unexposed =  with(dat, cpos/(cpos+cneg) )
RR = Risk_exposed/Risk_unexposed
log_RR=  log(RR)

## ------------------------------------------------------------------------
FE = rma(data = dat, yi = yi , vi = vi, 
         slab = paste(author, year),# indicate which variable contains the names of the trials 
         method = "FE")


FE =  metabin(measure="RR", event.e =tpos,n.e = (tneg+tpos), event.c = cpos, n.c = (cneg+cpos), 
              data = dat.bcg,studlab = paste(author, year) )

forest(FE)

dat$weights =  1/dat$vi
ggscatter(data= dat, y =  "yi", x=0.2, size = "weights")

## ------------------------------------------------------------------------
forest(FE, atransf = exp ,showweights = T)

## ------------------------------------------------------------------------
RE <- rma(yi, vi, data=dat, method="DL", slab = author)
RE
forest(RE)

## ------------------------------------------------------------------------
?rma

## ------------------------------------------------------------------------
RE.EB <- rma(yi, vi, data=dat, method="EB", slab = author)
RE.EB
forest(RE.EB)

## ------------------------------------------------------------------------
res.ME <- rma(yi, vi, mods=~I(ablat-33.46), data=dat, method="EB")
res.ME

## ------------------------------------------------------------------------

anova(RE, res.ME)


## ------------------------------------------------------------------------
predict(res.ME, newmods=c(33.46,42)-33.46, transf=exp, digits=2)

## ------------------------------------------------------------------------
res.FE <- rma(yi, vi, mods=~I(ablat-33.46), data=dat, method="FE")
res.FE

## ------------------------------------------------------------------------
predict(res.FE, newmods=c(33.46,42)-33.46, transf=exp, digits=2)

