



source("../Binary-covariate-effect-modification/Functions/Assisting functions/exact_binomial.R")

source("../Binary-covariate-effect-modification/Functions/Data-Generation functions/Binomial_Data_Simulator.R")


c(50,40,100,200,500,150)
IPD =  Binomial.Outcome.Data.Simulator(6,1000,
                                       ptreat = 0.5,
                                       phigh = c(0.3,0.4,0.5,0.6,0.7,0.1),
                                       betas = c(-1.385, 1 ,0.98,-0.98), beta_across = 0,tau = 0)


library(dplyr)
library(broom)


df = IPD %>% 
  group_by(studyid) %>%
  do(fit = glm(Outcome ~ Treat, data = ., family = poisson))

# get the coefficients by group in a tidy data_frame
df = tidy(df, fit)
df

library(meta)


meta_data = IPD %>% 
  group_by(studyid,Treat) %>%
  summarise(counts= sum(Outcome), n=n())

meta_data$Risk_percentage = c(0.3,0.3,0.4,0.4,0.5,0.5,0.6,0.6,0.7,0.7,0.1,0.1)
meta_data =  cbind(meta_data[meta_data$Treat==0,], meta_data[meta_data$Treat==1,])




res =metabin(event.e = counts,n.e = n,
        event.c = counts1,n.c = n1,
        data = meta_data, backtransf = T)

forest(res)

meta.reg= metareg(res,formula = Risk_percentage, hakn = T)
bubble(meta.reg, 
       xlab = "Trial percentage of smokers", 
       ylab = "Treatment effect(log-scale)",
       main= "Bubble-plot of treatment effects over percentage of smokers")




new_df = IPD %>% 
  group_by(studyid) %>%
  do(fit = glm(Outcome ~ Treat*Risk, data = ., family = poisson))

# get the coefficients by group in a tidy data_frame
new_df = tidy(new_df, fit)
new_df




