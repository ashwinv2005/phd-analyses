greentime2 = greentime[greentime$species != "HC" & greentime$species != "DL",]
head(greentime2)
unique(greentime2$survgerm)

greentime2$germstatus = NA
greentime2$mortstatus = NA
greentime2$interval = 0

# Symp

newSymp = greentime2[greentime2$species == "Symp",]
unique(newSymp$survgerm)

newSymp[newSymp$status0 > 0,]$status0 = 1
newSymp[newSymp$status16 > 0,]$status16 = 1
newSymp[newSymp$status30 > 0,]$status30 = 1
newSymp[newSymp$status37 > 0,]$status37 = 1
newSymp[newSymp$status63 > 0,]$status63 = 1


newSymp1 = newSymp
newSymp1$germstatus = newSymp$status0
newSymp1$mortstatus = NA
newSymp1$interval = 39
newSymp1$census = as.factor(1)

newSymp2 = newSymp
newSymp2[newSymp1$germstatus == 0,]$germstatus = newSymp[newSymp1$germstatus == 0,]$status16
newSymp2[newSymp1$germstatus == 1,]$mortstatus = newSymp[newSymp1$germstatus == 1,]$status16
newSymp2$interval = 17
newSymp2$census = as.factor(2)

newSymp3 = newSymp
newSymp3[!is.na(newSymp2$germstatus) & newSymp2$germstatus == 0,]$germstatus = newSymp[!is.na(newSymp2$germstatus) & newSymp2$germstatus == 0,]$status30
newSymp3[!is.na(newSymp2$mortstatus) & newSymp2$mortstatus == 1,]$mortstatus = newSymp[!is.na(newSymp2$mortstatus) & newSymp2$mortstatus == 1,]$status30
newSymp3[!is.na(newSymp2$germstatus) & newSymp2$germstatus == 1,]$mortstatus = newSymp[!is.na(newSymp2$germstatus) & newSymp2$germstatus == 1,]$status30
newSymp3$interval = 15
newSymp3$census = as.factor(3)

newSymp4 = newSymp
newSymp4[!is.na(newSymp3$germstatus) & newSymp3$germstatus == 0,]$germstatus = newSymp[!is.na(newSymp3$germstatus) & newSymp3$germstatus == 0,]$status37
newSymp4[!is.na(newSymp3$mortstatus) & newSymp3$mortstatus == 1,]$mortstatus = newSymp[!is.na(newSymp3$mortstatus) & newSymp3$mortstatus == 1,]$status37
newSymp4[!is.na(newSymp3$germstatus) & newSymp3$germstatus == 1,]$mortstatus = newSymp[!is.na(newSymp3$germstatus) & newSymp3$germstatus == 1,]$status37
newSymp4$interval = 8
newSymp4$census = as.factor(4)

newSymp5 = newSymp
newSymp5[!is.na(newSymp4$germstatus) & newSymp4$germstatus == 0,]$germstatus = newSymp[!is.na(newSymp4$germstatus) & newSymp4$germstatus == 0,]$status63
newSymp5[!is.na(newSymp4$mortstatus) & newSymp4$mortstatus == 1,]$mortstatus = newSymp[!is.na(newSymp4$mortstatus) & newSymp4$mortstatus == 1,]$status63
newSymp5[!is.na(newSymp4$germstatus) & newSymp4$germstatus == 1,]$mortstatus = newSymp[!is.na(newSymp4$germstatus) & newSymp4$germstatus == 1,]$status63
newSymp5$interval = 27
newSymp5$census = as.factor(5)

Symp = rbind(newSymp1,newSymp2,newSymp3,newSymp4,newSymp5)
Symp$mortstatus = 1 - Symp$mortstatus
Symp$census1 = Symp$census

# SC

newSC = greentime2[greentime2$species == "SC",]
unique(newSC$survgerm)

newSC[newSC$status0 > 0,]$status0 = 1
newSC[newSC$status16 > 0,]$status16 = 1
newSC[newSC$status30 > 0,]$status30 = 1
newSC[newSC$status37 > 0,]$status37 = 1
newSC[newSC$status63 > 0,]$status63 = 1


newSC1 = newSC
newSC1$germstatus = newSC$status0
newSC1$mortstatus = NA
newSC1$interval = 39
newSC1$census = as.factor(1)

newSC2 = newSC
newSC2[newSC1$germstatus == 0,]$germstatus = newSC[newSC1$germstatus == 0,]$status16
newSC2[newSC1$germstatus == 1,]$mortstatus = newSC[newSC1$germstatus == 1,]$status16
newSC2$interval = 17
newSC2$census = as.factor(2)

newSC3 = newSC
newSC3[!is.na(newSC2$germstatus) & newSC2$germstatus == 0,]$germstatus = newSC[!is.na(newSC2$germstatus) & newSC2$germstatus == 0,]$status30
newSC3[!is.na(newSC2$mortstatus) & newSC2$mortstatus == 1,]$mortstatus = newSC[!is.na(newSC2$mortstatus) & newSC2$mortstatus == 1,]$status30
newSC3[!is.na(newSC2$germstatus) & newSC2$germstatus == 1,]$mortstatus = newSC[!is.na(newSC2$germstatus) & newSC2$germstatus == 1,]$status30
newSC3$interval = 15
newSC3$census = as.factor(3)

newSC4 = newSC
newSC4[!is.na(newSC3$germstatus) & newSC3$germstatus == 0,]$germstatus = newSC[!is.na(newSC3$germstatus) & newSC3$germstatus == 0,]$status37
newSC4[!is.na(newSC3$mortstatus) & newSC3$mortstatus == 1,]$mortstatus = newSC[!is.na(newSC3$mortstatus) & newSC3$mortstatus == 1,]$status37
newSC4[!is.na(newSC3$germstatus) & newSC3$germstatus == 1,]$mortstatus = newSC[!is.na(newSC3$germstatus) & newSC3$germstatus == 1,]$status37
newSC4$interval = 8
newSC4$census = as.factor(4)

newSC5 = newSC
newSC5[!is.na(newSC4$germstatus) & newSC4$germstatus == 0,]$germstatus = newSC[!is.na(newSC4$germstatus) & newSC4$germstatus == 0,]$status63
newSC5[!is.na(newSC4$mortstatus) & newSC4$mortstatus == 1,]$mortstatus = newSC[!is.na(newSC4$mortstatus) & newSC4$mortstatus == 1,]$status63
newSC5[!is.na(newSC4$germstatus) & newSC4$germstatus == 1,]$mortstatus = newSC[!is.na(newSC4$germstatus) & newSC4$germstatus == 1,]$status63
newSC5$interval = 27
newSC5$census = as.factor(5)

SC = rbind(newSC1,newSC2,newSC3,newSC4,newSC5)
SC$mortstatus = 1 - SC$mortstatus
SC$census1 = SC$census

# SG

newSG = greentime2[greentime2$species == "SG",]
unique(newSG$survgerm)

newSG[newSG$status0 > 0,]$status0 = 1
newSG[newSG$status16 > 0,]$status16 = 1
newSG[newSG$status30 > 0,]$status30 = 1
newSG[newSG$status37 > 0,]$status37 = 1
newSG[newSG$status63 > 0,]$status63 = 1


newSG1 = newSG
newSG1$germstatus = newSG$status0
newSG1$mortstatus = NA
newSG1$interval = 39
newSG1$census = as.factor(1)

newSG2 = newSG
newSG2[newSG1$germstatus == 0,]$germstatus = newSG[newSG1$germstatus == 0,]$status16
newSG2[newSG1$germstatus == 1,]$mortstatus = newSG[newSG1$germstatus == 1,]$status16
newSG2$interval = 17
newSG2$census = as.factor(2)

newSG3 = newSG
newSG3[!is.na(newSG2$germstatus) & newSG2$germstatus == 0,]$germstatus = newSG[!is.na(newSG2$germstatus) & newSG2$germstatus == 0,]$status30
newSG3[!is.na(newSG2$mortstatus) & newSG2$mortstatus == 1,]$mortstatus = newSG[!is.na(newSG2$mortstatus) & newSG2$mortstatus == 1,]$status30
newSG3[!is.na(newSG2$germstatus) & newSG2$germstatus == 1,]$mortstatus = newSG[!is.na(newSG2$germstatus) & newSG2$germstatus == 1,]$status30
newSG3$interval = 15
newSG3$census = as.factor(3)

newSG4 = newSG
newSG4[!is.na(newSG3$germstatus) & newSG3$germstatus == 0,]$germstatus = newSG[!is.na(newSG3$germstatus) & newSG3$germstatus == 0,]$status37
newSG4[!is.na(newSG3$mortstatus) & newSG3$mortstatus == 1,]$mortstatus = newSG[!is.na(newSG3$mortstatus) & newSG3$mortstatus == 1,]$status37
newSG4[!is.na(newSG3$germstatus) & newSG3$germstatus == 1,]$mortstatus = newSG[!is.na(newSG3$germstatus) & newSG3$germstatus == 1,]$status37
newSG4$interval = 8
newSG4$census = as.factor(4)

newSG5 = newSG
newSG5[!is.na(newSG4$germstatus) & newSG4$germstatus == 0,]$germstatus = newSG[!is.na(newSG4$germstatus) & newSG4$germstatus == 0,]$status63
newSG5[!is.na(newSG4$mortstatus) & newSG4$mortstatus == 1,]$mortstatus = newSG[!is.na(newSG4$mortstatus) & newSG4$mortstatus == 1,]$status63
newSG5[!is.na(newSG4$germstatus) & newSG4$germstatus == 1,]$mortstatus = newSG[!is.na(newSG4$germstatus) & newSG4$germstatus == 1,]$status63
newSG5$interval = 27
newSG5$census = as.factor(5)

SG = rbind(newSG1,newSG2,newSG3,newSG4,newSG5)
SG$mortstatus = 1 - SG$mortstatus
SG$census1 = SG$census

# SR

newSR = greentime2[greentime2$species == "SR",]
unique(newSR$survgerm)

newSR[newSR$status16 > 0,]$status16 = 1
newSR[newSR$status30 > 0,]$status30 = 1
newSR[newSR$status37 > 0,]$status37 = 1
newSR[newSR$status63 > 0,]$status63 = 1
newSR[newSR$status110 > 0,]$status110 = 1


newSR1 = newSR
newSR1$germstatus = newSR$status16
newSR1$mortstatus = NA
newSR1$interval = 17
newSR1$census = as.factor(1)
newSR1$census1 = as.factor(2)

newSR2 = newSR
newSR2[newSR1$germstatus == 0,]$germstatus = newSR[newSR1$germstatus == 0,]$status30
newSR2[newSR1$germstatus == 1,]$mortstatus = newSR[newSR1$germstatus == 1,]$status30
newSR2$interval = 15
newSR2$census = as.factor(2)
newSR2$census1 = as.factor(3)

newSR3 = newSR
newSR3[!is.na(newSR2$germstatus) & newSR2$germstatus == 0,]$germstatus = newSR[!is.na(newSR2$germstatus) & newSR2$germstatus == 0,]$status37
newSR3[!is.na(newSR2$mortstatus) & newSR2$mortstatus == 1,]$mortstatus = newSR[!is.na(newSR2$mortstatus) & newSR2$mortstatus == 1,]$status37
newSR3[!is.na(newSR2$germstatus) & newSR2$germstatus == 1,]$mortstatus = newSR[!is.na(newSR2$germstatus) & newSR2$germstatus == 1,]$status37
newSR3$interval = 8
newSR3$census = as.factor(3)
newSR3$census1 = as.factor(4)

newSR4 = newSR
newSR4[!is.na(newSR3$germstatus) & newSR3$germstatus == 0,]$germstatus = newSR[!is.na(newSR3$germstatus) & newSR3$germstatus == 0,]$status63
newSR4[!is.na(newSR3$mortstatus) & newSR3$mortstatus == 1,]$mortstatus = newSR[!is.na(newSR3$mortstatus) & newSR3$mortstatus == 1,]$status63
newSR4[!is.na(newSR3$germstatus) & newSR3$germstatus == 1,]$mortstatus = newSR[!is.na(newSR3$germstatus) & newSR3$germstatus == 1,]$status63
newSR4$interval = 27
newSR4$census = as.factor(4)
newSR4$census1 = as.factor(5)

newSR5 = newSR
newSR5[!is.na(newSR4$germstatus) & newSR4$germstatus == 0,]$germstatus = newSR[!is.na(newSR4$germstatus) & newSR4$germstatus == 0,]$status110
newSR5[!is.na(newSR4$mortstatus) & newSR4$mortstatus == 1,]$mortstatus = newSR[!is.na(newSR4$mortstatus) & newSR4$mortstatus == 1,]$status110
newSR5[!is.na(newSR4$germstatus) & newSR4$germstatus == 1,]$mortstatus = newSR[!is.na(newSR4$germstatus) & newSR4$germstatus == 1,]$status110
newSR5$interval = 38
newSR5$census = as.factor(5)
newSR5$census1 = as.factor(6)

SR = rbind(newSR1,newSR2,newSR3,newSR4,newSR5)
SR$mortstatus = 1 - SR$mortstatus

# TC

newTC = greentime2[greentime2$species == "TC",]
unique(newTC$survgerm)

newTC[newTC$status16 > 0,]$status16 = 1
newTC[newTC$status30 > 0,]$status30 = 1
newTC[newTC$status37 > 0,]$status37 = 1


newTC1 = newTC
newTC1$germstatus = newTC$status16
newTC1$mortstatus = NA
newTC1$interval = 47
newTC1$census = as.factor(1)
newTC1$census1 = as.factor(2)

newTC2 = newTC
newTC2[newTC1$germstatus == 0,]$germstatus = newTC[newTC1$germstatus == 0,]$status30
newTC2[newTC1$germstatus == 1,]$mortstatus = newTC[newTC1$germstatus == 1,]$status30
newTC2$interval = 15
newTC2$census = as.factor(2)
newTC2$census1 = as.factor(3)

newTC3 = newTC
newTC3[!is.na(newTC2$germstatus) & newTC2$germstatus == 0,]$germstatus = newTC[!is.na(newTC2$germstatus) & newTC2$germstatus == 0,]$status37
newTC3[!is.na(newTC2$mortstatus) & newTC2$mortstatus == 1,]$mortstatus = newTC[!is.na(newTC2$mortstatus) & newTC2$mortstatus == 1,]$status37
newTC3[!is.na(newTC2$germstatus) & newTC2$germstatus == 1,]$mortstatus = newTC[!is.na(newTC2$germstatus) & newTC2$germstatus == 1,]$status37
newTC3$interval = 8
newTC3$census = as.factor(3)
newTC3$census1 = as.factor(4)

TC = rbind(newTC1,newTC2,newTC3)
TC$mortstatus = 1 - TC$mortstatus


# OD

newOD = greentime2[greentime2$species == "OD",]
unique(newOD$survgerm)


newOD[newOD$status30 > 0,]$status30 = 1
newOD[newOD$status63 > 0,]$status63 = 1
newOD[newOD$status110 > 0,]$status110 = 1
newOD[newOD$status139 > 0,]$status139 = 1


newOD1 = newOD
newOD1$germstatus = newOD$status30
newOD1$mortstatus = NA
newOD1$interval = 31
newOD1$census = as.factor(1)
newOD1$census1 = as.factor(3)

newOD2 = newOD
newOD2[newOD1$germstatus == 0,]$germstatus = newOD[newOD1$germstatus == 0,]$status63
newOD2[newOD1$germstatus == 1,]$mortstatus = newOD[newOD1$germstatus == 1,]$status63
newOD2$interval = 34
newOD2$census = as.factor(2)
newOD2$census1 = as.factor(5)

newOD3 = newOD
newOD3[!is.na(newOD2$germstatus) & newOD2$germstatus == 0,]$germstatus = newOD[!is.na(newOD2$germstatus) & newOD2$germstatus == 0,]$status110
newOD3[!is.na(newOD2$mortstatus) & newOD2$mortstatus == 1,]$mortstatus = newOD[!is.na(newOD2$mortstatus) & newOD2$mortstatus == 1,]$status110
newOD3[!is.na(newOD2$germstatus) & newOD2$germstatus == 1,]$mortstatus = newOD[!is.na(newOD2$germstatus) & newOD2$germstatus == 1,]$status110
newOD3$interval = 48
newOD3$census = as.factor(3)
newOD3$census1 = as.factor(6)

newOD4 = newOD
newOD4[!is.na(newOD3$germstatus) & newOD3$germstatus == 0,]$germstatus = newOD[!is.na(newOD3$germstatus) & newOD3$germstatus == 0,]$status139
newOD4[!is.na(newOD3$mortstatus) & newOD3$mortstatus == 1,]$mortstatus = newOD[!is.na(newOD3$mortstatus) & newOD3$mortstatus == 1,]$status139
newOD4[!is.na(newOD3$germstatus) & newOD3$germstatus == 1,]$mortstatus = newOD[!is.na(newOD3$germstatus) & newOD3$germstatus == 1,]$status139
newOD4$interval = 30
newOD4$census = as.factor(4)
newOD4$census1 = as.factor(7)

OD = rbind(newOD1,newOD2,newOD3,newOD4)
OD$mortstatus = 1 - OD$mortstatus

cloglog = rbind(Symp,SC,SR,SG,TC,OD)

### The analyses ###

cloglog$week = log(cloglog$interval/7)

library(lattice)

# binned plot

temp = cloglog
temp = temp[temp$alignment == "Y",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

germfit = glmer(germstatus ~  treatment + sizem + sizem:treatment + weightm + treatment*weightm + sizem*weightm  + offset(week)+
                 (1|species) + (1|census) + (treatment|fragment),
                contrasts = list(treatment = cMat), family=binomial(link="cloglog"), data= temp, nAGQ=1)

#germfit = glmer(germstatus ~  treatment*sizem + offset(week)+
#                  (1|species) + (1|census)+ (treatment|fragment),
#                contrasts = list(treatment = cMat), family=binomial(link="cloglog"), data= temp, nAGQ=1)

summary(germfit)

mod = summary(germfit)

write.csv(coef(mod), "C:/Users/ashwinv/Desktop/mod.csv")

ranef(germfit, condVar=TRUE)
dotplot(ranef(germfit, condVar=T))$species
dotplot(ranef(germfit, condVar=T))$census
dotplot(ranef(germfit, condVar=T))$fragment

temp = cloglog[cloglog$size != 30.6 | cloglog$treatment != "F",]
temp = temp[temp$alignment == "Y" & temp$mechanism != "b",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)

mat = rbind(c(-0.5,0.5,0,0),c(-0.5,0,0.5,0),c(-0.5,0,0,0.5))
library(MASS)
cMat1 = ginv(mat)

mortfit = glmer(mortstatus ~  treatment + sizem + treatment*sizem + weightm + treatment*weightm + weightm:sizem + offset(week)+
                  (1|species) + (1|census) + (treatment|fragment),
                contrasts = list(treatment = cMat), family=binomial(link="cloglog"), data= temp, nAGQ=0)

#mortfit = glmer(mortstatus ~  treatment*sizem + offset(week)+
#                  (1|species) + (1|census1) + (treatment|fragment),
#                contrasts = list(treatment = cMat), family=binomial(link="cloglog"), data= temp, nAGQ=0)

summary(mortfit)

mod = summary(mortfit)

write.csv(coef(mod), "C:/Users/ashwinv/Desktop/mod.csv")

ranef(mortfit, condVar=TRUE)
dotplot(ranef(mortfit, condVar=T))$species
dotplot(ranef(mortfit, condVar=T))$census
dotplot(ranef(mortfit, condVar=T))$fragment

rm(cMat,greentime1,greentime2,mat,newOD,newOD1,newOD2,newOD4,newSC,newSC1,newSC2,newSC3,newSC4)
rm(newSG,newSG1,newSG2,newSG3,newSG4,newSR,newSR1,newSR2,newSR3,newSR4,newSymp,newSymp1,newSymp2,newSymp3,newSymp4)
rm(newTC,newTC1,newTC2,newTC3,OD,res,SC,SG,sizeFres,sizeres,SR,Symp,TC,temp,temp1)
rm(fit,fitph,germclog,germfit,ID,mod,mortfit)
rm(newOD3,newSC5,newSG5,newSR5,newSymp5,cMat1)

