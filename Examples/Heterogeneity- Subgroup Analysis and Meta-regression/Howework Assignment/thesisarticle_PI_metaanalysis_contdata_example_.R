
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

study <- c("Filiaci 2000", "Holopainen 1982", "Johansson 2002", "Jorissen 2009", "Mastalerz 1997", "Mygind 1975", "Vlckova 2009")
data<- cbind(study, data)

#library(xlsx)
#write.xlsx(data, "H:\\Education\\Master_BMW\\18 Systematic Reviews and Meta-analysis\\2018-2019\\Week 2 Vr\\SSA\\topical_steroids.xlsx") 
# View(data)

# import topical_steroids
library(readxl)
topical_steroids <- read_excel("H:/Education/Master_BMW/18 Systematic Reviews and Meta-analysis/2018-2019/Week 2 Vr/SSA/topical_steroids.xlsx")
View(topical_steroids)

# conduct MA on SMD

?metacont

win.graph(height=7, width=13)

outmeta_z <- metacont(total.Steroid, mean.Steroid, sd.Steroid,total.PL, mean.PL, sd.PL,data=topical_steroids, 
                    sm="SMD", studlab=study, 
                    method.tau="EB") #  method.tau="EB") 

win.graph(height=7, width=13)
forest(outmeta_z, prediction =FALSE, comb.fixed=TRUE, addspace=TRUE,print.Q=TRUE,
       xlab="favours steroid          favours placebo", xlab.pos=0, fs.xlab=12, xlim=c(-2,1),  
       text.predict="95% Prediction interval",   
       leftcols=NULL,
       smlab="Standardized mean diff.", smlab.pos=-0.5,
       text.random="95% CI Random effects model", 
       lab.e="Steroid", lab.c="Placebo", text.I2 = "I2",text.tau2 = "tau2" )



# CI changes if we use HKSJ instead of Z-distribution
outmeta_HKSJ <- metacont(total.Steroid, mean.Steroid, sd.Steroid,total.PL, mean.PL, sd.PL,data=topical_steroids, 
                    sm="SMD", studlab=study,
                    hakn=TRUE, method.tau="EB") #  method.tau="EB") 
outmeta_HKSJ



# add prediction interval
outmeta_PI <- metacont(total.Steroid, mean.Steroid, sd.Steroid,total.PL, mean.PL, sd.PL,data=topical_steroids, 
                    sm="SMD", studlab=study, 
                hakn=TRUE, prediction=TRUE, method.tau="EB") #  method.tau="EB") 

outmeta_PI
forest(outmeta_PI)

# powerpoint mooi
win.graph(height=7, width=13)
forest(outmeta_PI, prediction =TRUE, comb.fixed=TRUE, addspace=TRUE,print.Q=TRUE,
           xlab="favours steroid          favours placebo", xlab.pos=0, fs.xlab=12, xlim=c(-2,1),  
           text.predict="95% Prediction interval",   
           leftcols=NULL,
           smlab="Standardized mean diff.", smlab.pos=-0.5,
           text.random="95% CI Random effects model", 
           lab.e="Steroid", lab.c="Placebo", text.I2 = "I2",text.tau2 = "tau2" )



# sensitivity analysis
metainf(outmeta_PI, pooled = "random")







