# library(gmodels)
# library(doBy)
# install.packages("[package_name]")
library(Hmisc)
library(car)
library(MASS)
library(doBy)
library(lme4)
library(mice)
library(VIM)

# working directory
setwd("C:/Users/Owner/Documents/UNL/2022 Fall/Math 435/Datasets")

# turn the cleaned excel file into a csv file
# read it in
all = read.csv("ValidData-MitC2022data - SalesPopulation.csv")
names = colnames(all)
names
summary(all)
ncol(all)
sd(all$Price)

cor(all$Qual,all$CDU, use = "pairwise.complete.obs")

# get only the quantitative variables
quant = all[c("CDU","Qual","TLA","GarCap","Bedrm",
              "FND","X801Units","X802Units","X803Units","FP",
              "FixCt","Pool","LandValue",
              "TotAcres","SaleYear","Price")] # HSA_Acres and Acres is gone
ncol(quant)

usable.1 = all[names[4:22]]
usable.2 = all[names[24:26]]
usable.3 = all[names[29:36]]
usable = cbind(usable.1,usable.2,usable.3)
colnames(usable)

usable$PrUse = factor(usable$PrUse)
usable$Remd = factor(usable$Remd)
usable$YrRD = factor(usable$YrRD)
usable$YrBlt = factor(usable$YrBlt)
usable$LandType = factor(usable$LandType)
usable$SaleQuater = factor(usable$SaleQuarter)
usable$Imp = factor(usable$Imp)

####### correlation for each year #######

c = 0 # counter
corAll = vector(length = 15)
priorYear

for (x in 1:7){
  c = c + 1
  
  
  subYear.all = subset(quant, SaleYear == c)
  subYear.1 = subYear.all[1:14]
  subYear.2 = subYear.all[16]
  subYear = cbind(subYear.1,subYear.2)
  
  corYear = cor(subYear, use = "pairwise.complete.obs")
  
  corAll = rbind(corAll, corYear)
  
  print(corYear)
}
corAll
write.csv(corAll, file = "C:/Users/Owner/Documents/UNL/2022 Fall/Math 435/QuantCorMatrixByYear.csv")
#########################################

corYear = cor(subset(usable, SaleYear == 1))
   
usable.2016 = subset(usable, SaleYear == 1)
cor(usable.2016)

head(usable.2016)

#######################

lm.usable.full = lm(Price ~ PrUse + CDU + Qual + Imp + TLA + YrRD + Remd
                    + TLA + YrBlt + GarCap + Bedrm + FND + X801Units + X802Units
                    + X803Units + FP + FixCt + Pool + LandType + LandValue + Infl1 + Infl2
                    + TotAcres + SaleQuarter + SaleYear, data = usable)
nrow(lm.usable.full$model)

int.usable.mod = lm(Price ~ 1, data = lm.usable.full$model)

aov.usable.full = aov(Price ~ PrUse + CDU + Qual + Imp + TLA + YrRD + Remd
                    + TLA + YrBlt + GarCap + Bedrm + FND + X801Units + X802Units
                    + X803Units + FP + FixCt + Pool + LandType + LandValue + Infl1 + Infl2
                    + TotAcres + SaleQuarter + SaleYear, data = usable)

summary(lm.usable.full)
summary(aov.usable.full)


########################

fwd.sel <- step(object = int.usable.mod, scope = list(upper = lm.usable.full), 
                 direction = "forward", k = 2, trace = TRUE) 

lm.fwd <- lm(Price ~ LandValue + FixCt + SaleYear + Imp + TLA + X803Units + 
                CDU + X801Units + Infl1 + GarCap + Pool + YrBlt + Bedrm + 
                Qual + LandType + Infl2 + TotAcres + SaleQuarter + PrUse + 
                FP + FND + Remd + X802Units, data = usable)

summary(lm.fwd)


########################

# correlation matrix doesn't work for more that 12 of them...
corQuant = cor(quant, use = "pairwise.complete.obs") # only works up to 12 for whatever reason 
# might want to CHANGE the 0:12 here
corQuant

cor()

write.csv(corQuant, file = "C:/Users/Owner/Documents/UNL/2022 Fall/Math 435/QuantCorMatrix.csv")

# different correlation function, can get p-values with this one if you want
res2 <- rcorr(as.matrix(quant))  
res2

# flatten it to look at it function
flattenCorrMatrix <- function(cormat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut]
  )
}

flatCor = flattenCorrMatrix(corQuant)
flatOrder = flatCor[order(-flatCor$cor),] # order flattened correlation matrix descending
flatOrder


# plot the correlation coefficients, might crash your computer if you go through too many
# not really helpful for a lot of them
plot(quant[5:10], pch=20 , cex=1.5 , col="#69b3a2") # might want to CHANGE 5:10 here

a.1 = aov(Price ~ CDU + Qual + TLA + GarCap + Bedrm + FND + X801Units + X802Units + X803Units + FP 
            + FixCt + Pool + LandValue, data = quant)
summary(a.1)

test.mod = lm(Price ~ 1, data = quant)
nrow(test.mod$model)


lm.full = lm(Price ~ CDU + Qual + TLA + GarCap + Bedrm + FND + X801Units + X802Units + X803Units + FP + FixCt + LandValue + TotAcres, data = quant, na.action = na.exclude)
lm.full$model
nrow(lm.full$model)

int.mod = lm(Price ~ 1, data = lm.full$model)
summary(lm.full)
# issues with including: Pool, Acres, HSA_Acres


fwd.sel  <- step(object = int.mod, scope = list(upper = lm.full), 
                 direction = "forward", k = 2, trace = TRUE) 
# gives Price ~ LandValue + TLA + FP + GarCap + CDU + X801Units + TotAcres + FixCt + Qual + Bedrm
# AIC=5822.29
back.sel <- step(object = lm.full, scope = list(upper = lm.full), 
                 direction = "backward", k = 2, trace = TRUE) 
# gives Price ~ CDU + Qual + TLA + GarCap + Bedrm + X802Units + X803Units + FP + FixCt + LandValue + TotAcres
# AIC=5822.73
step.sel <- step(object = int.mod, scope = list(upper = lm.full), 
                 direction = "both", k = 2, trace = TRUE)
# same thing as backwards

# potentially best model
lm.best = lm(Price ~ LandValue + TLA + FP + GarCap + CDU + X801Units + TotAcres + FixCt + Qual + Bedrm, data = quant)
summary(lm.best)
lm.small = lm(Price ~ CDU + Qual + TLA + GarCap + Bedrm, data = quant)
summary(lm.small)


lm.test = lm(Price ~ FP + LandValue + Qual + FixCt + GarCap + TLA + FND, data = quant)
summary(lm.test)
nrow(lm.test$model)

########## trying to use mice ##########
md.pattern(usable)
aggr_plot <- aggr(quant, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
