
# higgins methode 3 voor SE ln  H in Higgins stat med. 2002
# exp(ln H ± Z * SE.lnH
# large Q
# SE.lnH <-  0.5 * (log(out$QE)-log(out$k-1)) / (sqrt(2*out$QE) - sqrt(2*out$k-3))
# small Q (<k-1)
# SE.lnH <- sqrt( (1/(2*out$k-4)) * (1 - 1/(3*(out$k-2)^2)))


# log(sqrt(out$H2))



##### meta-analyses for dichotomous outcome; number of studies

# let op volgorde: laatste is belangrijikste
library (metafor)
library(Hmisc)  # for data import
library (meta)


# citation("metafor")
citation()
# citation("meta")

# help("metafor")
# help ("meta")



############################### start program ################################

rm(list=ls()) #cleaning up 
search()

### read data 

transport.dir <- "H:/Biostatistiek/Projecten/Projecten JintHout/Onderzoek/accepted_art_5_tau_predictionintervals/data/transport/"
#transport.dir <- "C:/Users/Joanna/Documents/Publicaties/art_5_prediction_intervals/"


# setwd ("C:/Users/Joanna/Documents/Publicaties/art_2_tau_studysize/Programs_R")
 setwd("H:/Biostatistiek/Projecten/Projecten JintHout/Onderzoek/accepted_art_5_tau_predictionintervals/Programs_R")
        


################################################ example
# dich data terug gevonden met dich_meta_first uit art 2 study size tau
# dichotomous meta-analyse 1502:	cd007503	
# review: Antidepressants for depression in physically ill people, analysis 1.1



# continuous meta-analysis 746, cd006549 Ear Nose Throat.
# Topical steroids for nasal polyps (Review)

#### all cont meta_analyses 
metadata <- sasxport.get(paste(transport.dir,"BCadata.xpt", sep=""))
nrow(metadata)

data <- metadata[metadata$meta.an==746,]

?meta

#data <- metadata[metadata$meta.an==750,]
#studynames = c("1", "2", "3")

# data <- escalc(measure="OR",  ai = tes, ci =ces, n1i= nt, n2i = nc, add = 0.5, data=data ) 

data <- escalc( measure = "SMD",m1i = mean.1, m2i = mean.2,  sd1i = sd.1, sd2i = sd.2, n1i = total.1, n2i=total.2, data=data)


# studynames <- c("Costa 1985", "Devos 2008", "Menza 2009", "Razavi 1997", "Schiffer 1990", "Shi 2006", "Tan 1994")
studynames <- c("Filiaci 2000", "Holopainen 1982", "Johansson 2002", "Jorissen 2009", "Mastalerz 1997", "Mygind 1975", "Vlckova 2009")

orderyr <- c(4,2,5,6,3,1,7) 
data2<- data[order(orderyr),]
studynames2 <- studynames[order(orderyr)]


######## package meta: oorspronkelijke Higgins formules  (df=k-2)
#outmeta <- metabin(tes, nt, ces, nc, studlab=studynames, sm="OR", hakn=TRUE, method.tau="EB", prediction=TRUE, 
#                   allstudies=TRUE, method="Inverse", data=data)

# meta
outmeta <- metacont(total.1, mean.1, sd.1,total.2, mean.2, sd.2,data=data, sm="SMD", studlab=studynames,
                  hakn=TRUE, prediction=TRUE, method.tau="EB") #  method.tau="EB") 

# metafor
out <- rma(measure = "SMD", m1i = mean.1, m2i = mean.2,  sd1i = sd.1, sd2i = sd.2, 
                n1i = total.1, n2i=total.2, knha=TRUE, method="EB",  data=data) # 

prout <- predict(out)   # met echte SE, niet met geschatte SE
prout


# prout$pred + qt(0.975, 2)*prout$se


#(-0.96 - (-0.07))/3.92

# eigenlijk zouden we de higgins (df=k-2) prediction intervals 
# moeten vervangen door metafor (df=k-1) prediction intervals
# maar in het voorbeeld gebruiken we niet de echte SE maar 
# de SE afgeleid van het HKSJ confidence interval, en dat is breder, 
# toevalligerwijze is daardoor het outmeta predictieinterval bijna precies correct
# op klein detail na
outmeta$lower.predict <- -1.60 # prout$cr.lb
outmeta$upper.predict <- 0.58  # prout$cr.ub

# NOTE: what happens is that if we take the df=6 (viechtbauer), en de 
# SE derived from de reported hksj confidence interval, we get a prediction interval
# that is almost exactly equal to the prediction interval that we would get if 
# we would use the estimated SE from the meta-analysis, in combination with the 
# df=k-2 approach.
# NB bij viechtbauer (metafor) is SE schatting van model met of zonder HKSJ identiek.


# check prediction interval outcomes
# qt(0.025, 2, lower.tail = FALSE, log.p = FALSE)

# Example in paper: df=k-1, se derived from HKSJ CI /3.92
# -0.51 - 2.45 * sqrt(0.1477 + 0.227^2)  # -1.60
# -0.51 + 2.45 * sqrt(0.1477 + 0.227^2)   #  0.58
#  true viechtbauer  (t verdeling, df=k-1), met HKSJ
# -0.51 - 2.45 * sqrt(0.1477 + 0.182^2)  # -1.55
# -0.51 + 2.45 * sqrt(0.1477 + 0.182^2)   #  0.53
#  true viechtbauer  (z verdeling), zonder HKSJ
# -0.51 - 1.96 * sqrt(0.1477 + 0.182^2)  # -1.34
# -0.51 + 1.96 * sqrt(0.1477 + 0.182^2)   #  0.32
#  true meta pakket  (t verdeling, df=k-2)
# -0.51 - 2.57 * sqrt(0.1477 + 0.182^2)  # ~-1.60  (-1.61)
# -0.51 + 2.57 * sqrt(0.1477 + 0.182^2)  #  0.58


# Start tiff device driver to save output to figure.png
# tiff(filename="Figure_thesis.tif",width = 4000, height = 2000, units = "px")
#png(file = 'pngtest1.png', width = 5*256, height = 3*256, units = "px")
#postscript(file = 'pngtest1.eps')
par(mar=c(0, 0, 0, 0)) 


# Start tiff device driver to save output to figure.png
 # tiff(filename="Figure_thesis.tif",width = 4000, height = 2000, units = "px")
# png(file = 'figure_thesis.png', width = 5*256, height = 3*256, units = "px")
# pdf(file = 'pngtest1.pdf')
# par(mar=c(0, 0, 0, 0)) 


# artikel predictie intervallen
# win.graph(height=8, width=30)

tiff(filename = "H:/Biostatistiek/Projecten/Projecten JinH/Publicaties/art_5_tau_predictionintervals/Programs_R/PI_thesis600_upd.tif", 
    width = 600*14, height = 600*7, units = "px", pointsize = 12,
     res = 600, family = "", restoreConsole = TRUE)

forest(outmeta, prediction =TRUE, comb.fixed=FALSE, addspace=TRUE,print.Q=TRUE,
           xlab="favours steroid          favours placebo", xlab.pos=0, fs.xlab=12, xlim=c(-2,1),  
           text.predict="95% Prediction interval",   
           smlab="Standardized mean diff.", smlab.pos=-0.5,
           text.random="95% CI Random effects model", 
           lab.e="Steroid", lab.c="Placebo", text.I2 = "I2",text.tau2 = "tau2", plotwidth=grid::unit(13, "cm"))

dev.off()




# powerpoint ISCB utrecht tau-study size
win.graph(height=7, width=13)
forest(outmeta, prediction =TRUE, comb.fixed=TRUE, addspace=TRUE,print.Q=TRUE,
           xlab="favours steroid          favours placebo", xlab.pos=0, fs.xlab=12, xlim=c(-2,1),  
           text.predict="95% Prediction interval",   
           leftcols=NULL,
           smlab="Standardized mean diff.", smlab.pos=-0.5,
           text.random="95% CI Random effects model", 
           lab.e="Steroid", lab.c="Placebo", text.I2 = "I2",text.tau2 = "tau2" )





#
#forest(outmeta, prediction =TRUE, comb.fixed=FALSE, pooled.events=TRUE, xlab="favours antidepressants", print.Q=TRUE,
#           addspace=TRUE,xlab.pos=2, fs.xlab=12, text.predict="95% Prediction interval", xlim=c(0.2,20),  
#           text.random="95% CI Random effects model" , lab.e="Anti-depr.", lab.c="Placebo", text.I2 = "I2",
#       text.tau2 = "tau2")

dev.off()




dev.off()




str(outmeta)
TE <- outmeta$TE.random
SE <- outmeta$seTE.random
tau <- outmeta$tau
paste(outmeta$pval.random, outmeta$zval.random)




#eventuele tekst toevoegen met:
#.9 is hier de y-as, die loopt van 0 tot 1
#grid.text("My custom title", .5, .9, gp=gpar(cex=2))











############ package metafor: k-1 DF

out <- rma.uni(yi, vi, method= "EB", knha=TRUE, data=data, control=list(maxiter=1000))
# out.tr <- rma.uni(yi, vi, method= "DL", knha=FALSE, data=data, control=list(maxiter=1000))

prout <- predict.rma(out)
prout
    CR.se <- sqrt(out$vb + out$tau2)
    tval <- out$b/CR.se
    CR.pval <- 2*pt(abs(tval), df=out$k-1, lower.tail = FALSE, log.p = FALSE)
# str(out)

# estimated summary odds ratio
# exp(out$b)

# corresponding SE on log scale
sqrt(out$vb)

# corresponding CI
# round(c(exp(out$ci.lb), exp(out$ci.ub)),2)

sqrt(out$tau2)

# PI according to metafor package (viechtbauer)
round(c(exp(prout$cr.lb), exp(prout$cr.ub)),3)

alpha <- 0.05 
critV    <- qt(alpha/2, df = out$k -out$p, lower.tail = FALSE); # critV ;         # Viechtbauer metafor
critHigg <- qt(alpha/2, df = out$k -out$p-1, lower.tail = FALSE);#  critHigg      # Higgins 2009  

round(c(exp(out$b - critV*CR.se), exp(out$b + critV*CR.se)),3)
round(c(exp(out$b - critHigg*CR.se), exp(out$b + critHigg*CR.se)),3)


##### wat is de kans dat het echte effect groter is dan D  "power"
k=7
D=0
CR.se <- sqrt(0.1477 + 0.227^2) 
# Prob(true effect >D) 
pt(q=((D-out$b)/CR.se), df=k-1,lower.tail=F)	

log(2.28)
sdpi = sqrt(0.353 + 0.318^2); sdpi
tstat = log(2.28)/sdpi; tstat

# steekproef mean: -0.51
# steekproef SE 0.426
# --> standardized sample mean om steekproefgemiddelde te vergelijken met threshold D
Tstand <- (-0.51 - D)/0.426; Tstand

# uitleg
# SMDs met sample mean -0.51 en SE 0.426, zijn normaal verdeeld rond -0.51.
# wat is de kans dat een SMD groter dan 0 is? 
# T-waarde van de SMDS is:  (smd+0.51)/0.426 zijn t-verdeeld rond 0, df=6
# de kans dat SMD is groter dan 0, is gelijk aan de kans dat de 
# T-waarde van de SMDs groter dan (0 + 0.51)/0.426
# Deze kans is gelijk aan 
pt(q=((0-out$b)/CR.se), df=k-1,lower.tail=F)	
pt(q=0, df=6, lower.tail=F)








?pt
x <- seq(-3,3, by=0.1)
y <- dt(x, 6)
plot(x,y, type="l")
x1 = (x-out$b)/CR.se
y1 = dt(x1,6)
lines(x1,y1)



# OR voorbeeld
pisd<-sqrt(.353 + .318^2)
pisd
pt(q=((D-log(2.28))/pisd), df=k-1,lower.tail=T)	



forest(out)
addpoly(x, vi, sei, ci.lb, ci.ub, rows=-1, level=95, digits=2,
annotate=TRUE, mlab, transf, atransf, targs,
efac=1, col, border, cex, ...)



res <- rma.mh(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg,
slab=paste(author, year, sep=", "))
### forest plot of the observed relative risks with summary estimate
forest(res, atransf=exp, xlim=c(-7,5), ylim=c(-2.5,16))
### meta-analysis of the log relative risks using a random-effects model
res <- rma(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
### add summary estimate from the random-effects model to forest plot
addpoly(res, atransf=exp)
### forest plot with subgrouping of studies and summaries per subgroup
res <- rma(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg,
slab=paste(author, year, sep=", "))
forest(res, xlim=c(-16, 6), at=log(c(.05, .25, 1, 4)), atransf=exp,
ilab=cbind(dat.bcg$tpos, dat.bcg$tneg, dat.bcg$cpos, dat.bcg$cneg),
ilab.xpos=c(-9.5,-8,-6,-4.5), cex=.75, ylim=c(-1, 27),
order=order(dat.bcg$alloc), rows=c(3:4,9:15,20:23),
mlab="RE Model for All Studies")
op <- par(cex=.75, font=4)
text(-16, c(24,16,5), c("Systematic Allocation", "Random Allocation",
"Alternate Allocation"), pos=4)
par(font=2)
text(c(-9.5,-8,-6,-4.5), 26, c("TB+", "TB-", "TB+", "TB-"))
text(c(-8.75,-5.25), 27, c("Vaccinated", "Control"))
text(-16, 26, "Author(s) and Year", pos=4)
text(6, 26, "Relative Risk [95% CI]", pos=2)
par(op)
res <- rma(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg,
subset=(alloc=="systematic"))
addpoly(res, row=18.5, cex=.75, atransf=exp, mlab="RE Model for Subgroup")
res <- rma(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg,
subset=(alloc=="random"))
addpoly(res, row=7.5, cex=.75, atransf=exp, mlab="RE Model for Subgroup")
res <- rma(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg,
subset=(alloc=="alternate"))
addpoly(res, row=1.5, cex=.75, atransf=exp, mlab="RE Model for Subgroup")



############ evt data invoer

zinc<- matrix(c(
1,19,12,33,30,
2,5,17,35,38,
3,9,29,50,50,
4,24,29,57,53,
5,31,48,61,69),ncol=5, byrow=TRUE)
colnames(zinc) <- c("study", "tes", "ces", "nt", "nc")
zinc








