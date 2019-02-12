
library(meta)

library(readxl)

#Load the data and save them to the object data 3
data3 = read_xlsx(path = "Data/Data3.xlsx")
## Look at the 6 first rows
head(data3)




mc3 <- metacont(Ne, Me, Se, Nc, Mc, Sc, data=data3,sm = "MD",
                studlab=paste(author, year))

## Print the meta-analysis
mc3

forest(mc3)
## Print the summary of our meta-analysis
print(summary(mc3), digits=2)


### Subgroup Analysis

### We can add a categorical variable to check if it modifies the treatment effect results.

mc3.sub <- metacont(Ne, Me, Se, Nc, Mc, Sc, data=data3,sm = "MD",
                studlab=paste(author, year),
                byvar=duration)


## Print the meta-analysis
mc3.sub

## Print the summary of our meta-analysis
print(summary(mc3.sub), digits=2)

### Plot the forest plot 


forest(mc3.sub,
       xlab="Difference in mean response")


### Another example 

data4 <- read_xlsx("Data/Data4.xlsx")
data4

summary(data4$Ee/data4$Ne)


mb3 <- metabin(Ee, Ne, Ec, Nc, sm="RR", method="I",
               data=data4, studlab=study)
print(summary(mb3), digits=2)


mb3.sub <- metabin(Ee, Ne, Ec, Nc, sm="RR", method="I",
               data=data4, studlab=study,  byvar=blind, print.byvar=FALSE)
print(summary(mb3.sub), digits=2)


round(mb3$Q, 2) # Cochran's Q statistic

round(mb3.sub$Q, 2) # Cochran's Q statistic

# Conduct meta-analysis for first subgroup:
mc3s1 <- metacont(Ne, Me, Se, Nc, Mc, Sc, data=data3, studlab=paste(author, year), subset=duration=="<= 6 months")
# Conduct meta-analysis for second subgroup:
mc3s2 <- metacont(Ne, Me, Se, Nc, Mc, Sc, data=data3, studlab=paste(author, year), subset=duration=="> 6 months")

# Subgroup treatment effects (fixed effect model)
TE.duration <- c(mc3s1$TE.fixed, mc3s2$TE.fixed)
# Corresponding standard errors (fixed effect model)
seTE.duration <- c(mc3s1$seTE.fixed, mc3s2$seTE.fixed)

mh1 <- metagen(TE.duration, seTE.duration, sm="MD", 
               studlab=c("<= 6 months", " > 6 months"), comb.random=FALSE)
print(mh1, digits=2)




