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
######## reading stuff in/housekeeping data stuff ########
# working directory
setwd("C:/Users/Owner/Documents/UNL/2022 Fall/Math 435/Datasets")

# turn the cleaned excel file into a csv file
# read it in
all = read.csv("CleanedDataV2.csv")
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
# usable$YrBlt = factor(usable$YrBlt)
usable$LandType = factor(usable$LandType)
usable$SaleQuater = factor(usable$SaleQuarter)
usable$Imp = factor(usable$Imp)

######## Full linear model ##########


lm.usable.full = lm(Price ~ PrUse + CDU + Qual + Imp + TLA + YrRD + Remd
                    + TLA + YrBlt + GarCap + Bedrm + FND + X801Units + X802Units
                    + X803Units + FP + FixCt + Pool + LandType + LandValue + Infl1 + Infl2
                    + TotAcres + SaleQuarter + SaleYear, data = usable)
summary(lm.usable.full) # this one gives p-values

aov.usable.full = aov(Price ~ PrUse + CDU + Qual + Imp + TLA + YrRD + Remd
                    + TLA + YrBlt + GarCap + Bedrm + FND + X801Units + X802Units
                    + X803Units + FP + FixCt + Pool + LandType + LandValue + Infl1 + Infl2
                    + TotAcres + SaleQuarter + SaleYear, data = usable)

summary(aov.usable.full) # this one gives f-values



##### run this section if you want to see how it picks its terms #####

lm.squares.full = lm(Price ~ PrUse + CDU + I(CDU^2) + Qual + I(Qual^2) + Imp + TLA + I(TLA^2) 
                     + YrRD + Remd + TLA + I(TLA^2) + YrBlt  + GarCap + I(GarCap^2) 
                     + Bedrm + I(Bedrm^2) + FND + I(FND^2) + X801Units + I(X801Units^2) + X802Units 
                     + I(X802Units^2) + X803Units + I(X803Units^2) + FP + I(FP^2) + FixCt + I(FixCt^2)
                     + Pool + I(Pool^2) + LandType + LandValue + I(LandValue^2) + Infl1 + Infl2
                     + TotAcres + SaleQuarter + SaleYear + I(SaleYear^2), data = usable) # can't use yrblt squares for some reason # + I(YrBlt^2)

int.usable.mod = lm(Price ~ 1, data = lm.usable.full$model)

fwd.sel <- step(object = int.usable.mod, scope = list(upper = lm.squares.full), 
                direction = "forward", k = 2, trace = TRUE) 

# but it ends up picking this


lm.square.fwd <- lm(Price ~ LandValue + I(FixCt^2) + I(SaleYear^2) + I(X801Units^2) + 
                      Imp + TLA + CDU + GarCap + I(Qual^2) + Qual + YrBlt + Infl1 + 
                      TotAcres + I(Pool^2) + X803Units + LandType + SaleQuarter + 
                      Infl2 + I(LandValue^2) + Bedrm + X801Units + FND + SaleYear + 
                      Pool + I(CDU^2) + FP + PrUse + I(X802Units^2) + X802Units + 
                      I(GarCap^2) + I(FP^2) + I(X803Units^2) + Remd + I(TLA^2), data = usable)


nrow(lm.usable.full$model)
ncol(lm.usable.full$model)