library(ggplot2)
library(ggthemes)
library(tidyverse)

theme_set(theme_tufte())

## all species

y = table(sggroup[sggroup$status1 != 0,]$species)
#y = y[y>=5]

temp = sggroup[sggroup$status1 >= 1 & sggroup$species != "SC" & sggroup$species %in% names(y),]
temp = temp[temp$species != "Hopea" & temp$species != "Artoh" & temp$species != "Palm",]
#temp[temp$plot == "6",]$plot57 = 2
temp$fplot57 = as.factor(temp$plot57)
#temp[temp$plot == "5",]$plot67 = 2
temp$fplot67 = as.factor(temp$plot67)
temp$statusm = scale(temp$status1, center = F)
temp$sizem = scale(temp$size, center = F)
temp$logdens = log(temp$status1)
temp$logdensm = scale(temp$logdens)
temp[is.na(temp$slope),]$slope = 5 
temp$slope = temp$slope/100
temp$slope = scale(temp$slope, center = F)


temp = temp[temp$status1 != 49,]
temp = temp[temp$species != "Symp" | temp$plot != "5" | temp$status1 != 15,]


temp$plotx = temp$plot
temp[temp$plotx == "2",]$plotx = "1"

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit9c = glmer (dd ~ fplot57*fplot67*statusm + fplot57*fplot67*sizem + slope + (1|species) + (0 + statusm|species) +
                 (1|locationfac/group/plot), weights = status1, data = temp, contrasts = list(fplot57 = cMat, fplot67 = cMat), family="binomial", nAGQ = 0)


library(mgcv)
library(lme4)
library(ggplot2)

temp$pred = predict(fit9c, type = "response", re.form = NA, allow.new.levels=TRUE)

a = predict(fit9c, type = "response", re.form = NA)
b = predict(fit9c, type = "response")
c = b - a
temp$newdd = temp$dd - c
temp[temp$newdd > 1,]$newdd = 1
temp[temp$newdd < 0,]$newdd = 0

temp1 = temp

temp1$predm = 0
temp1$sel = 0
temp1$ser = 0


ltemp = temp1

predFun = function(fit) {
  predict(fit,ltemp, re.form = NA, allow.new.levels=TRUE)
}

bb = bootMer(fit9c,nsim=100,FUN=predFun)

for (i in 1:length(temp1$dd))
{
  temp1$predm[i] = binomial()$linkinv(median(bb$t[,i]))
  temp1$sel[i] = binomial()$linkinv(median(bb$t[,i]) - sd(bb$t[,i]))
  temp1$ser[i] = binomial()$linkinv(median(bb$t[,i]) + sd(bb$t[,i]))
}

ggp = ggplot(temp1, aes(x = status1, y = predm, col = as.factor(plotx)))  +
  geom_point(size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F) +
  geom_smooth(aes(x = status1, y = sel, col = as.factor(plotx)), method = "glm", method.args = list(family = "binomial"), se = F) +
  
  geom_smooth(aes(x = status1, y = ser, col = as.factor(plotx)), method = "glm", method.args = list(family = "binomial"), se = F)

##### 1

selribx = ggplot_build(ggp)$data[[3]]$x[1:80]
selriby = ggplot_build(ggp)$data[[3]]$y[1:80]

serribx = ggplot_build(ggp)$data[[4]]$x[1:80]
serriby = ggplot_build(ggp)$data[[4]]$y[1:80]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot12 == 1,]$dd),replace = T),]
temp1$selribx[temp1$plot12 == 1] = srib$selribx
temp1$selriby[temp1$plot12 == 1] = srib$selriby
temp1$serriby[temp1$plot12 == 1] = srib$serriby

##### 2

selribx = ggplot_build(ggp)$data[[3]]$x[81:160]
selriby = ggplot_build(ggp)$data[[3]]$y[81:160]

serribx = ggplot_build(ggp)$data[[4]]$x[81:160]
serriby = ggplot_build(ggp)$data[[4]]$y[81:160]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "5",]$dd),replace = T),]
temp1$selribx[temp1$plot == "5"] = srib$selribx
temp1$selriby[temp1$plot == "5"] = srib$selriby
temp1$serriby[temp1$plot == "5"] = srib$serriby

##### 3

selribx = ggplot_build(ggp)$data[[3]]$x[161:240]
selriby = ggplot_build(ggp)$data[[3]]$y[161:240]

serribx = ggplot_build(ggp)$data[[4]]$x[161:240]
serriby = ggplot_build(ggp)$data[[4]]$y[161:240]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "6",]$dd),replace = T),]
temp1$selribx[temp1$plot == "6"] = srib$selribx
temp1$selriby[temp1$plot == "6"] = srib$selriby
temp1$serriby[temp1$plot == "6"] = srib$serriby

##### 4

selribx = ggplot_build(ggp)$data[[3]]$x[241:320]
selriby = ggplot_build(ggp)$data[[3]]$y[241:320]

serribx = ggplot_build(ggp)$data[[4]]$x[241:320]
serriby = ggplot_build(ggp)$data[[4]]$y[241:320]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "7",]$dd),replace = T),]
temp1$selribx[temp1$plot == "7"] = srib$selribx
temp1$selriby[temp1$plot == "7"] = srib$selriby
temp1$serriby[temp1$plot == "7"] = srib$serriby

tempall = temp1
tempall$group = "All Species" 

# SR

temp = sggroup[sggroup$status1 >= 1 & sggroup$species == "SR" & sggroup$status1 <= 300,]
#temp[temp$plot == "6",]$plot57 = 2
temp$fplot57 = as.factor(temp$plot57)
#temp[temp$plot == "5",]$plot67 = 2
temp$fplot67 = as.factor(temp$plot67)
temp$statusm = scale(temp$status1, center = F)
temp$sizem = scale(temp$size, center = F)
temp$logdens = log(temp$status1)
temp$logdensm = scale(temp$logdens)
temp[is.na(temp$slope),]$slope = 5 
temp$slope = temp$slope/100
temp$slope = scale(temp$slope, center = F)
temp = temp[-c(142,49),]

temp$plotx = temp$plot
temp[temp$plotx == "2",]$plotx = "1"

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit9c = glmer (dd ~ fplot57*fplot67*statusm + fplot57*fplot67*sizem + slope + 
                 (1|locationfac/group/plot), weights = status1, data = temp, contrasts = list(fplot57 = cMat,fplot67 = cMat), family="binomial", nAGQ = 0)


library(mgcv)
library(lme4)
library(ggplot2)

temp$pred = predict(fit9c, type = "response", re.form = NA, allow.new.levels=TRUE)

a = predict(fit9c, type = "response", re.form = NA)
b = predict(fit9c, type = "response")
c = b - a
temp$newdd = temp$dd - c
temp[temp$newdd > 1,]$newdd = 1
temp[temp$newdd < 0,]$newdd = 0

temp1 = temp

temp1$predm = 0
temp1$sel = 0
temp1$ser = 0


ltemp = temp1

predFun = function(fit) {
  predict(fit,ltemp, re.form = NA, allow.new.levels=TRUE)
}

bb = bootMer(fit9c,nsim=100,FUN=predFun)

for (i in 1:length(temp1$dd))
{
  temp1$predm[i] = binomial()$linkinv(median(bb$t[,i]))
  temp1$sel[i] = binomial()$linkinv(median(bb$t[,i]) - sd(bb$t[,i]))
  temp1$ser[i] = binomial()$linkinv(median(bb$t[,i]) + sd(bb$t[,i]))
}

ggp = ggplot(temp1, aes(x = status1, y = predm, col = as.factor(plotx)))  +
  geom_point(size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F) +
  geom_smooth(aes(x = status1, y = sel, col = as.factor(plotx)), method = "glm", method.args = list(family = "binomial"), se = F) +
  
  geom_smooth(aes(x = status1, y = ser, col = as.factor(plotx)), method = "glm", method.args = list(family = "binomial"), se = F)

##### 1

selribx = ggplot_build(ggp)$data[[3]]$x[1:80]
selriby = ggplot_build(ggp)$data[[3]]$y[1:80]

serribx = ggplot_build(ggp)$data[[4]]$x[1:80]
serriby = ggplot_build(ggp)$data[[4]]$y[1:80]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot12 == 1,]$dd),replace = T),]
temp1$selribx[temp1$plot12 == 1] = srib$selribx
temp1$selriby[temp1$plot12 == 1] = srib$selriby
temp1$serriby[temp1$plot12 == 1] = srib$serriby

##### 2

selribx = ggplot_build(ggp)$data[[3]]$x[81:160]
selriby = ggplot_build(ggp)$data[[3]]$y[81:160]

serribx = ggplot_build(ggp)$data[[4]]$x[81:160]
serriby = ggplot_build(ggp)$data[[4]]$y[81:160]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "5",]$dd),replace = T),]
temp1$selribx[temp1$plot == "5"] = srib$selribx
temp1$selriby[temp1$plot == "5"] = srib$selriby
temp1$serriby[temp1$plot == "5"] = srib$serriby

##### 3

selribx = ggplot_build(ggp)$data[[3]]$x[161:240]
selriby = ggplot_build(ggp)$data[[3]]$y[161:240]

serribx = ggplot_build(ggp)$data[[4]]$x[161:240]
serriby = ggplot_build(ggp)$data[[4]]$y[161:240]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "6",]$dd),replace = T),]
temp1$selribx[temp1$plot == "6"] = srib$selribx
temp1$selriby[temp1$plot == "6"] = srib$selriby
temp1$serriby[temp1$plot == "6"] = srib$serriby

##### 4

selribx = ggplot_build(ggp)$data[[3]]$x[241:320]
selriby = ggplot_build(ggp)$data[[3]]$y[241:320]

serribx = ggplot_build(ggp)$data[[4]]$x[241:320]
serriby = ggplot_build(ggp)$data[[4]]$y[241:320]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "7",]$dd),replace = T),]
temp1$selribx[temp1$plot == "7"] = srib$selribx
temp1$selriby[temp1$plot == "7"] = srib$selriby
temp1$serriby[temp1$plot == "7"] = srib$serriby

tempSR = temp1
tempSR$group = "SR" 



# SP


temp = sggroup[sggroup$status1 >= 1 & sggroup$species == "Climber1",]
#temp[temp$plot == "6",]$plot57 = 2
temp$fplot57 = as.factor(temp$plot57)
#temp[temp$plot == "5",]$plot67 = 2
temp$fplot67 = as.factor(temp$plot67)
temp$statusm = scale(temp$status1, center = F)
temp$sizem = scale(temp$size, center = F)
temp$logdens = log(temp$status1)
temp$logdensm = scale(temp$logdens)
temp[is.na(temp$slope),]$slope = 5 
temp$slope = temp$slope/100
temp$slope = scale(temp$slope, center = F)
temp = temp[temp$status1 != 49,]

temp$plotx = temp$plot
temp[temp$plotx == "2",]$plotx = "1"


mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit9c = glmer (dd ~ fplot57*fplot67*statusm + slope + 
                 (1|groupfac/plot), weights = status1, data = temp, contrasts = list(fplot57 = cMat,fplot67 = cMat), family="binomial", nAGQ = 0)

library(mgcv)
library(lme4)
library(ggplot2)

temp$pred = predict(fit9c, type = "response", re.form = NA, allow.new.levels=TRUE)

a = predict(fit9c, type = "response", re.form = NA)
b = predict(fit9c, type = "response")
c = b - a
temp$newdd = temp$dd - c
temp[temp$newdd > 1,]$newdd = 1
temp[temp$newdd < 0,]$newdd = 0

temp1 = temp

temp1$predm = 0
temp1$sel = 0
temp1$ser = 0


ltemp = temp1

predFun = function(fit) {
  predict(fit,ltemp, re.form = NA, allow.new.levels=TRUE)
}

bb = bootMer(fit9c,nsim=100,FUN=predFun)

for (i in 1:length(temp1$dd))
{
  temp1$predm[i] = binomial()$linkinv(median(bb$t[,i]))
  temp1$sel[i] = binomial()$linkinv(median(bb$t[,i]) - sd(bb$t[,i]))
  temp1$ser[i] = binomial()$linkinv(median(bb$t[,i]) + sd(bb$t[,i]))
}

ggp = ggplot(temp1, aes(x = status1, y = predm, col = as.factor(plotx)))  +
  geom_point(size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F) +
  geom_smooth(aes(x = status1, y = sel, col = as.factor(plotx)), method = "glm", method.args = list(family = "binomial"), se = F) +
  
  geom_smooth(aes(x = status1, y = ser, col = as.factor(plotx)), method = "glm", method.args = list(family = "binomial"), se = F)

##### 1

selribx = ggplot_build(ggp)$data[[3]]$x[1:80]
selriby = ggplot_build(ggp)$data[[3]]$y[1:80]

serribx = ggplot_build(ggp)$data[[4]]$x[1:80]
serriby = ggplot_build(ggp)$data[[4]]$y[1:80]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot12 == 1,]$dd),replace = T),]
temp1$selribx[temp1$plot12 == 1] = srib$selribx
temp1$selriby[temp1$plot12 == 1] = srib$selriby
temp1$serriby[temp1$plot12 == 1] = srib$serriby

##### 2

selribx = ggplot_build(ggp)$data[[3]]$x[81:160]
selriby = ggplot_build(ggp)$data[[3]]$y[81:160]

serribx = ggplot_build(ggp)$data[[4]]$x[81:160]
serriby = ggplot_build(ggp)$data[[4]]$y[81:160]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "5",]$dd),replace = T),]
temp1$selribx[temp1$plot == "5"] = srib$selribx
temp1$selriby[temp1$plot == "5"] = srib$selriby
temp1$serriby[temp1$plot == "5"] = srib$serriby

##### 3

selribx = ggplot_build(ggp)$data[[3]]$x[161:240]
selriby = ggplot_build(ggp)$data[[3]]$y[161:240]

serribx = ggplot_build(ggp)$data[[4]]$x[161:240]
serriby = ggplot_build(ggp)$data[[4]]$y[161:240]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "6",]$dd),replace = T),]
temp1$selribx[temp1$plot == "6"] = srib$selribx
temp1$selriby[temp1$plot == "6"] = srib$selriby
temp1$serriby[temp1$plot == "6"] = srib$serriby

##### 4

selribx = ggplot_build(ggp)$data[[3]]$x[241:320]
selriby = ggplot_build(ggp)$data[[3]]$y[241:320]

serribx = ggplot_build(ggp)$data[[4]]$x[241:320]
serriby = ggplot_build(ggp)$data[[4]]$y[241:320]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "7",]$dd),replace = T),]
temp1$selribx[temp1$plot == "7"] = srib$selribx
temp1$selriby[temp1$plot == "7"] = srib$selriby
temp1$serriby[temp1$plot == "7"] = srib$serriby

tempSP = temp1
tempSP$group = "SP" 



# Symp

temp = sggroup[sggroup$status1 >= 1 & sggroup$species == "Symp",]
#temp[temp$plot == "6",]$plot57 = 2
temp$fplot57 = as.factor(temp$plot57)
#temp[temp$plot == "5",]$plot67 = 2
temp$fplot67 = as.factor(temp$plot67)
temp$statusm = scale(temp$status1, center = F)
temp$sizem = scale(temp$size, center = F)
temp$logdens = log(temp$status1)
temp$logdensm = scale(temp$logdens)
temp[is.na(temp$slope),]$slope = 5 
temp$slope = temp$slope/100
temp$slope = scale(temp$slope, center = F)
temp = temp[temp$species != "Symp" | temp$plot != "5" | temp$status1 != 15,]

temp$plotx = temp$plot
temp[temp$plotx == "2",]$plotx = "1"

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit9c = glmer (dd ~ fplot57*fplot67*statusm + slope +
                 (1|locationfac/group/plot), weights = status1, data = temp, contrasts = list(fplot57 = cMat, fplot67 = cMat), family="binomial", nAGQ = 0)

library(mgcv)
library(lme4)
library(ggplot2)

temp$pred = predict(fit9c, type = "response", re.form = NA, allow.new.levels=TRUE)

a = predict(fit9c, type = "response", re.form = NA)
b = predict(fit9c, type = "response")
c = b - a
temp$newdd = temp$dd - c
temp[temp$newdd > 1,]$newdd = 1
temp[temp$newdd < 0,]$newdd = 0

temp1 = temp

temp1$predm = 0
temp1$sel = 0
temp1$ser = 0


ltemp = temp1

predFun = function(fit) {
  predict(fit,ltemp, re.form = NA, allow.new.levels=TRUE)
}

bb = bootMer(fit9c,nsim=100,FUN=predFun)

for (i in 1:length(temp1$dd))
{
  temp1$predm[i] = binomial()$linkinv(median(bb$t[,i]))
  temp1$sel[i] = binomial()$linkinv(median(bb$t[,i]) - sd(bb$t[,i]))
  temp1$ser[i] = binomial()$linkinv(median(bb$t[,i]) + sd(bb$t[,i]))
}

ggp = ggplot(temp1, aes(x = status1, y = predm, col = as.factor(plotx)))  +
  geom_point(size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F) +
  geom_smooth(aes(x = status1, y = sel, col = as.factor(plotx)), method = "glm", method.args = list(family = "binomial"), se = F) +
  
  geom_smooth(aes(x = status1, y = ser, col = as.factor(plotx)), method = "glm", method.args = list(family = "binomial"), se = F)

##### 1

selribx = ggplot_build(ggp)$data[[3]]$x[1:80]
selriby = ggplot_build(ggp)$data[[3]]$y[1:80]

serribx = ggplot_build(ggp)$data[[4]]$x[1:80]
serriby = ggplot_build(ggp)$data[[4]]$y[1:80]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot12 == 1,]$dd),replace = T),]
temp1$selribx[temp1$plot12 == 1] = srib$selribx
temp1$selriby[temp1$plot12 == 1] = srib$selriby
temp1$serriby[temp1$plot12 == 1] = srib$serriby

##### 2

selribx = ggplot_build(ggp)$data[[3]]$x[81:160]
selriby = ggplot_build(ggp)$data[[3]]$y[81:160]

serribx = ggplot_build(ggp)$data[[4]]$x[81:160]
serriby = ggplot_build(ggp)$data[[4]]$y[81:160]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "5",]$dd),replace = T),]
temp1$selribx[temp1$plot == "5"] = srib$selribx
temp1$selriby[temp1$plot == "5"] = srib$selriby
temp1$serriby[temp1$plot == "5"] = srib$serriby

##### 3

selribx = ggplot_build(ggp)$data[[3]]$x[161:240]
selriby = ggplot_build(ggp)$data[[3]]$y[161:240]

serribx = ggplot_build(ggp)$data[[4]]$x[161:240]
serriby = ggplot_build(ggp)$data[[4]]$y[161:240]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "6",]$dd),replace = T),]
temp1$selribx[temp1$plot == "6"] = srib$selribx
temp1$selriby[temp1$plot == "6"] = srib$selriby
temp1$serriby[temp1$plot == "6"] = srib$serriby

##### 4

selribx = ggplot_build(ggp)$data[[3]]$x[241:320]
selriby = ggplot_build(ggp)$data[[3]]$y[241:320]

serribx = ggplot_build(ggp)$data[[4]]$x[241:320]
serriby = ggplot_build(ggp)$data[[4]]$y[241:320]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "7",]$dd),replace = T),]
temp1$selribx[temp1$plot == "7"] = srib$selribx
temp1$selriby[temp1$plot == "7"] = srib$selriby
temp1$serriby[temp1$plot == "7"] = srib$serriby

tempSymp = temp1
tempSymp$group = "Symp" 


# VM

temp = sggroup[sggroup$status1 >= 1 & sggroup$species == "FLC",]
temp = temp[temp$status1 < 60,]

#temp[temp$plot == "6",]$plot57 = 2
temp$fplot57 = as.factor(temp$plot57)
#temp[temp$plot == "5",]$plot67 = 2
temp$fplot67 = as.factor(temp$plot67)
temp$statusm = scale(temp$status1, center = F)
temp$sizem = scale(temp$size, center = F)
temp$logdens = log(temp$status1)
temp$logdensm = scale(temp$logdens)
temp[is.na(temp$slope),]$slope = 5 
temp$slope = temp$slope/100
temp$slope = scale(temp$slope, center = F)

temp$plotx = temp$plot
temp[temp$plotx == "2",]$plotx = "1"

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit9c = glmer (dd ~ fplot57*fplot67 + statusm + slope + (1|locationfac/group),
               weights = status1, data = temp, contrasts = list(fplot57 = cMat, fplot67 = cMat), family="binomial", nAGQ = 0)

library(mgcv)
library(lme4)
library(ggplot2)

temp$pred = predict(fit9c, type = "response", re.form = NA, allow.new.levels=TRUE)

a = predict(fit9c, type = "response", re.form = NA)
b = predict(fit9c, type = "response")
c = b - a
temp$newdd = temp$dd - c
temp[temp$newdd > 1,]$newdd = 1
temp[temp$newdd < 0,]$newdd = 0

temp1 = temp

temp1$predm = 0
temp1$sel = 0
temp1$ser = 0


ltemp = temp1

predFun = function(fit) {
  predict(fit,ltemp, re.form = NA, allow.new.levels=TRUE)
}

bb = bootMer(fit9c,nsim=100,FUN=predFun)

for (i in 1:length(temp1$dd))
{
  temp1$predm[i] = binomial()$linkinv(median(bb$t[,i]))
  temp1$sel[i] = binomial()$linkinv(median(bb$t[,i]) - sd(bb$t[,i]))
  temp1$ser[i] = binomial()$linkinv(median(bb$t[,i]) + sd(bb$t[,i]))
}

ggp = ggplot(temp1, aes(x = status1, y = predm, col = as.factor(plotx)))  +
  geom_point(size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F) +
  geom_smooth(aes(x = status1, y = sel, col = as.factor(plotx)), method = "glm", method.args = list(family = "binomial"), se = F) +
  
  geom_smooth(aes(x = status1, y = ser, col = as.factor(plotx)), method = "glm", method.args = list(family = "binomial"), se = F)

##### 1

selribx = ggplot_build(ggp)$data[[3]]$x[1:80]
selriby = ggplot_build(ggp)$data[[3]]$y[1:80]

serribx = ggplot_build(ggp)$data[[4]]$x[1:80]
serriby = ggplot_build(ggp)$data[[4]]$y[1:80]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot12 == 1,]$dd),replace = T),]
temp1$selribx[temp1$plot12 == 1] = srib$selribx
temp1$selriby[temp1$plot12 == 1] = srib$selriby
temp1$serriby[temp1$plot12 == 1] = srib$serriby

##### 2

selribx = ggplot_build(ggp)$data[[3]]$x[81:160]
selriby = ggplot_build(ggp)$data[[3]]$y[81:160]

serribx = ggplot_build(ggp)$data[[4]]$x[81:160]
serriby = ggplot_build(ggp)$data[[4]]$y[81:160]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "5",]$dd),replace = T),]
temp1$selribx[temp1$plot == "5"] = srib$selribx
temp1$selriby[temp1$plot == "5"] = srib$selriby
temp1$serriby[temp1$plot == "5"] = srib$serriby

##### 3

selribx = ggplot_build(ggp)$data[[3]]$x[161:240]
selriby = ggplot_build(ggp)$data[[3]]$y[161:240]

serribx = ggplot_build(ggp)$data[[4]]$x[161:240]
serriby = ggplot_build(ggp)$data[[4]]$y[161:240]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "6",]$dd),replace = T),]
temp1$selribx[temp1$plot == "6"] = srib$selribx
temp1$selriby[temp1$plot == "6"] = srib$selriby
temp1$serriby[temp1$plot == "6"] = srib$serriby

##### 4

selribx = ggplot_build(ggp)$data[[3]]$x[241:320]
selriby = ggplot_build(ggp)$data[[3]]$y[241:320]

serribx = ggplot_build(ggp)$data[[4]]$x[241:320]
serriby = ggplot_build(ggp)$data[[4]]$y[241:320]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "7",]$dd),replace = T),]
temp1$selribx[temp1$plot == "7"] = srib$selribx
temp1$selriby[temp1$plot == "7"] = srib$selriby
temp1$serriby[temp1$plot == "7"] = srib$serriby

tempVM = temp1
tempVM$group = "VM" 


# All others


y = table(sggroup[sggroup$status1 != 0,]$species)
y = y[y>=5]

temp = sggroup[sggroup$status1 >= 1 & sggroup$species != "SR" & sggroup$species != "Symp" & 
                 sggroup$species != "Climber1" & sggroup$species != "SC" & sggroup$species %in% names(y),]
temp = temp[temp$species != "Hopea" & temp$species != "Artoh" & temp$species != "Palm",]
temp = temp[temp$species != "FLC",]

#temp[temp$plot == "6",]$plot57 = 2
temp$fplot57 = as.factor(temp$plot57)
#temp[temp$plot == "5",]$plot67 = 2
temp$fplot67 = as.factor(temp$plot67)
temp$statusm = scale(temp$status1, center = F)
#temp$AOm = scale(temp$AO, center = F)
temp$sizem = scale(temp$size, center = F)
temp$logdens = log(temp$status1)
temp$logdensm = scale(temp$logdens)
temp[is.na(temp$slope),]$slope = 5 
temp$slope = temp$slope/100
temp$slope = scale(temp$slope, center = F)

temp$plotx = temp$plot
temp[temp$plotx == "2",]$plotx = "1"

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit9c = glmer (dd ~ fplot57*fplot67*statusm + fplot57*fplot67*sizem + slope + (1|species) + (0 + statusm|species) +
                 (1|locationfac/group/plot), weights = status1, data = temp, contrasts = list(fplot57 = cMat,fplot67 = cMat), family="binomial", nAGQ = 0)


library(mgcv)
library(lme4)
library(ggplot2)

temp$pred = predict(fit9c, type = "response", re.form = NA, allow.new.levels=TRUE)

a = predict(fit9c, type = "response", re.form = NA)
b = predict(fit9c, type = "response")
c = b - a
temp$newdd = temp$dd - c
temp[temp$newdd > 1,]$newdd = 1
temp[temp$newdd < 0,]$newdd = 0

temp1 = temp

temp1$predm = 0
temp1$sel = 0
temp1$ser = 0


ltemp = temp1

predFun = function(fit) {
  predict(fit,ltemp, re.form = NA, allow.new.levels=TRUE)
}

bb = bootMer(fit9c,nsim=100,FUN=predFun)

for (i in 1:length(temp1$dd))
{
  temp1$predm[i] = binomial()$linkinv(median(bb$t[,i]))
  temp1$sel[i] = binomial()$linkinv(median(bb$t[,i]) - sd(bb$t[,i]))
  temp1$ser[i] = binomial()$linkinv(median(bb$t[,i]) + sd(bb$t[,i]))
}

ggp = ggplot(temp1, aes(x = status1, y = predm, col = as.factor(plotx)))  +
  geom_point(size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F) +
  geom_smooth(aes(x = status1, y = sel, col = as.factor(plotx)), method = "glm", method.args = list(family = "binomial"), se = F) +
  
  geom_smooth(aes(x = status1, y = ser, col = as.factor(plotx)), method = "glm", method.args = list(family = "binomial"), se = F)

##### 1

selribx = ggplot_build(ggp)$data[[3]]$x[1:80]
selriby = ggplot_build(ggp)$data[[3]]$y[1:80]

serribx = ggplot_build(ggp)$data[[4]]$x[1:80]
serriby = ggplot_build(ggp)$data[[4]]$y[1:80]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot12 == 1,]$dd),replace = T),]
temp1$selribx[temp1$plot12 == 1] = srib$selribx
temp1$selriby[temp1$plot12 == 1] = srib$selriby
temp1$serriby[temp1$plot12 == 1] = srib$serriby

##### 2

selribx = ggplot_build(ggp)$data[[3]]$x[81:160]
selriby = ggplot_build(ggp)$data[[3]]$y[81:160]

serribx = ggplot_build(ggp)$data[[4]]$x[81:160]
serriby = ggplot_build(ggp)$data[[4]]$y[81:160]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "5",]$dd),replace = T),]
temp1$selribx[temp1$plot == "5"] = srib$selribx
temp1$selriby[temp1$plot == "5"] = srib$selriby
temp1$serriby[temp1$plot == "5"] = srib$serriby

##### 3

selribx = ggplot_build(ggp)$data[[3]]$x[161:240]
selriby = ggplot_build(ggp)$data[[3]]$y[161:240]

serribx = ggplot_build(ggp)$data[[4]]$x[161:240]
serriby = ggplot_build(ggp)$data[[4]]$y[161:240]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "6",]$dd),replace = T),]
temp1$selribx[temp1$plot == "6"] = srib$selribx
temp1$selriby[temp1$plot == "6"] = srib$selriby
temp1$serriby[temp1$plot == "6"] = srib$serriby

##### 4

selribx = ggplot_build(ggp)$data[[3]]$x[241:320]
selriby = ggplot_build(ggp)$data[[3]]$y[241:320]

serribx = ggplot_build(ggp)$data[[4]]$x[241:320]
serriby = ggplot_build(ggp)$data[[4]]$y[241:320]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "7",]$dd),replace = T),]
temp1$selribx[temp1$plot == "7"] = srib$selribx
temp1$selriby[temp1$plot == "7"] = srib$selriby
temp1$serriby[temp1$plot == "7"] = srib$serriby

tempOt = temp1
tempOt$group = "Other Common Species" 



temp1 = rbind(tempall,tempSR,tempSP,tempSymp,tempVM,tempOt)
temp1$group = as.factor(temp1$group)

temp1$group = factor(temp1$group,levels = c("All Species","SR","SP","Symp","VM","Other Common Species"))


# Plotting

# Fungi

ggp = ggplot(temp1[temp1$plot != "6" & temp1$plot != "7",], aes(x = status1, y = predm, col = as.factor(plotx)))  +
  geom_point(aes(x = status1, y = newdd, col = as.factor(plotx)), size = 2) +
  facet_wrap(~group) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, col = as.factor(plotx), linetype = NA), alpha=0.1) +
  xlab("Conspecific density") +
  ylab("Per-capita mortality") 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  scale_size(guide = 'none') +
  #scale_color_manual(values = c("#D55E00", "#009E73"), name="",
  #                   breaks=c("1","5"),
  #                   labels=c("Control", "Fungicide"))
  theme(legend.position = "none")

# Insects

ggp = ggplot(temp1[temp1$plot != "5" & temp1$plot != "7",], aes(x = status1, y = predm, col = as.factor(plotx)))  +
  geom_point(aes(x = status1, y = newdd, col = as.factor(plotx)), size = 2) +
  facet_wrap(~group) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, col = as.factor(plotx), linetype = NA), alpha=0.1) +
  xlab("Conspecific density") +
  ylab("Per-capita mortality")
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  scale_size(guide = 'none') +
  #scale_color_manual(values = c("#D55E00", "#0072B2"), name="",
  #                   breaks=c("1","6"),
  #                   labels=c("Control", "Insecticide"))
  theme(legend.position = "none")

# Both

ggp = ggplot(temp1[temp1$plot != "5" & temp1$plot != "6",], aes(x = status1, y = predm, col = as.factor(plotx)))  +
  geom_point(aes(x = status1, y = newdd, col = as.factor(plotx)), size = 2) +
  facet_wrap(~group) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, col = as.factor(plotx), linetype = NA), alpha=0.1) +
  xlab("Conspecific density") +
  ylab("Per-capita mortality") 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  scale_size(guide = 'none') +
  #scale_color_manual(values = c("#D55E00", "#999999"), name="",
  #                   breaks=c("1","7"),
  #                   labels=c("Control", "Both"))
  theme(legend.position = "none")