#### Meta-regression 

library(readxl)
library(meta)

data1 <- read_xlsx("Data/Data1.xlsx")
head(data1)


m <- metacont(Ne, Me, Se, Nc, Mc, Sc,
              studlab=paste(author, year),
              data=data1)
m



forest(m, xlab="Difference in outcome measure")



### Calculate by hand 
### The mean differences per trial 
MD <- with(data1, Me - Mc)

### The standard errors of the mean differences
seMD <- with(data1, sqrt(Se^2/Ne + Sc^2/Nc))

### Their z-scores
zscores <- MD/seMD

### The total number of participants per trial
N <- with(data1, Ne + Nc)

## The standardised mean difference per trial
SMD <- with(data1,
            (1 - 3/(4 * N - 9)) * (Me - Mc) /
              sqrt(((Ne - 1) * Se^2 + (Nc - 1) * Sc^2)/(N - 2)))

## The standard errors of the standardised mean differences per trial

seSMD <- with(data1,
              sqrt(N/(Ne * Nc) + SMD^2/(2 * (N - 3.94))))

round(c(SMD, SMD + qnorm(1-(0.05/2)) * seSMD), 2)

### Let's cross-validate the results with the metacont function
print(metacont(Ne, Me, Se, Nc, Mc, Sc, sm="SMD",data=data1), digits=2)


### Now we can calculate the weights per trial 
### Using the inverse variance method

varMD = seMD^2
weight <- 1/varMD

### Calculate the pooled mean difference  
round(weighted.mean(MD, weight), 4)

### Calculate the standard error of the mean difference
round(1/sum(weight), 4)

### Or do it using the metacont function
mc1 <- metacont(Ne, Me, Se, Nc, Mc, Sc,
                data=data1,
                studlab=paste(author, year))
mc1


round(c(mc1$TE.fixed, mc1$seTE.fixed^2), 4)



forest(mc1, comb.random=FALSE, 
       xlab= "Difference in mean response",
       xlim=c(-50,10), xlab.pos=-20, smlab.pos=-20)

## Add the prediction interval too
forest(mc1, comb.random=FALSE, 
       xlab= "Difference in mean response",
       xlim=c(-50,10), xlab.pos=-20, smlab.pos=-20,
       prediction=TRUE, col.predict="black")


### We can use the metagen function too 

mc1.gen <- metagen(TE, seTE, data=mc1, sm="MD")

c(mc1$TE.fixed, mc1$TE.random)

c(mc1.gen$TE.fixed, mc1.gen$TE.random)


### Let's make the same analysis with the SMD

VarSMD =  seSMD^2  
weight =  1/VarSMD

### Calculate the pooled standardised mean difference  
round(weighted.mean(SMD, weight), 4)

### Calculate the standard error of the standardised mean difference
round(1/sum(weight), 4)

### Or calcluate them using the metacont function
mc2 <- metacont(n.e = Ne, mean.e = Me, sd.e = Se, n.c = Nc, mean.c = Mc, sd.c = Sc, sm="SMD",
                data=data1, studlab = paste(author, year))
round(c(mc2$TE.fixed, mc2$seTE.fixed^2), 4)

### Print the results
print(summary(mc2), digits=2)

mc2

forest(mc2, comb.random=FALSE, 
       xlab= "Difference in mean response")

## Add the prediction interval too
forest(mc2, comb.random=FALSE, 
       xlab= "Difference in mean response",
       xlim=c(-50,10), xlab.pos=-20, smlab.pos=-20,
       prediction=TRUE, col.predict="black")

