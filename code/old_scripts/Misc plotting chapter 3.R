


library(ggplot2)
library(ggthemes)
library(tidyverse)

theme_set(theme_tufte())

## Beta diversity

beta = data.frame(rep(1:8,2))
names(beta) = "square"
beta$size = c(plots$size,plots$size)
beta$group = c(plots$group,plots$group)
beta$type = c(rep("Basic beta diversity", 8),rep("Weighted beta diversity",8))
beta$type = as.factor(beta$type)
beta$dist = c(plots$distsa,plots$gdistsa)
beta$sd = c(plots$sdsa,plots$gsdsa)

ggp = ggplot(beta, aes(x = size, y = dist))  +
  facet_wrap(~type, nrow = 2, scales = "free") +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = dist - sd, ymax = dist + sd)) +
  geom_smooth(method = "lm", alpha = 0.1, level = 0.95) +
  xlab("Fragment size (ha)") +
  ylab("Beta diversity") 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  #scale_y_continuous(breaks = seq(0,100,10)) +
  scale_x_continuous(trans = "reverse") +
  scale_size(guide = 'none') +
  #scale_color_manual(values = c("#D55E00", "#009E73"), name="",
  #                   breaks=c("1","5"),
  #                   labels=c("Control", "Fungicide"))
  theme(legend.position = "none")

## seedling div vs. adult div

ggp = ggplot(plots, aes(x = divsa, y = divsg))  +
  #facet_wrap(~age) +
  geom_point(size = 1) +
  #geom_errorbar(aes(ymin = gdistsa - gsdsa, ymax = gdistsa + gsdsa)) +
  geom_smooth(method = "lm", alpha = 0.1) +
  xlab("Adult diversity") +
  ylab("Seedling diversity") 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  #scale_y_continuous(breaks = seq(0,100,10)) +
  scale_size(guide = 'none') +
  #scale_color_manual(values = c("#D55E00", "#009E73"), name="",
  #                   breaks=c("1","5"),
  #                   labels=c("Control", "Fungicide"))
  theme(legend.position = "none")

## sapling div vs. adult div

fit = lmer(divsl~divsa + (1|site), data = quadrats)

temp = quadrats

a = predict(fit, type = "response", re.form = NA)
b = predict(fit, type = "response")
d = b - a
temp$new = temp$divsl - d

ggp = ggplot(temp, aes(x = divsa, y = divsl))  +
  #facet_wrap(~age) +
  geom_point(size = 1) +
  #geom_errorbar(aes(ymin = gdistsa - gsdsa, ymax = gdistsa + gsdsa)) +
  geom_smooth(method = "lm", alpha = 0.1) +
  xlab("Adult diversity") +
  ylab("Sapling diversity") 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  #scale_y_continuous(breaks = seq(0,100,10)) +
  scale_size(guide = 'none') +
  #scale_color_manual(values = c("#D55E00", "#009E73"), name="",
  #                   breaks=c("1","5"),
  #                   labels=c("Control", "Fungicide"))
  theme(legend.position = "none")

## seedling and sapling diversity vs. adult diversity

fit = lmer(divsgsl~divsa + (1|site), data = quadrats)

temp = quadrats

a = predict(fit, type = "response", re.form = NA)
b = predict(fit, type = "response")
d = b - a
temp$new = temp$divsgsl - d

ggp = ggplot(temp, aes(x = divsa, y = new))  +
  #facet_wrap(~age) +
  geom_point(size = 1) +
  #geom_errorbar(aes(ymin = gdistsa - gsdsa, ymax = gdistsa + gsdsa)) +
  geom_smooth(method = "lm", alpha = 0.1) +
  xlab("Adult diversity") +
  ylab("Sapling diversity") 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  #scale_y_continuous(breaks = seq(0,100,10)) +
  scale_size(guide = 'none') +
  #scale_color_manual(values = c("#D55E00", "#009E73"), name="",
  #                   breaks=c("1","5"),
  #                   labels=c("Control", "Fungicide"))
  theme(legend.position = "none")


## aggregations saplings quadrat scale

temp = data.frame(rep(1:128,6))
names(temp) = "square"
temp$size = rep(quadrats$size,6)
temp$site = rep(quadrats$site,6)
temp$group = rep(quadrats$group,6)
temp$species = rep(c("All species","Psni","Dige","Psma","Lifl","Notr"), each = 128)
temp$species = as.factor(temp$species)
temp$agg = c(quadrats$meanagg,quadrats$Psni,quadrats$Dige,quadrats$Psma,quadrats$Lifl,quadrats$Notr)
temp[temp$species == "Dige",]$group[65:80] = "Small"
temp[temp$species == "Notr",]$group[65:80] = "Small"

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

temp1 = temp

temp1$predm = 0
temp1$cil = 0
temp1$cir = 0

ltemp1 = temp1[temp1$species == "All species",]
ltemp2 = temp1[temp1$species == "Psni",]
ltemp3 = temp1[temp1$species == "Dige",]
ltemp4 = temp1[temp1$species == "Psma",]
ltemp5 = temp1[temp1$species == "Lifl",]
ltemp6 = temp1[temp1$species == "Notr",]  

fit1 = lmer(agg~group + (1|site), contrasts = list(group = cMat), data = ltemp1)
fit2 = lmer(agg~group + (1|site), contrasts = list(group = cMat), data = ltemp2)
fit3 = lmer(agg~group + (1|site), contrasts = list(group = cMat), data = ltemp3)
fit4 = lmer(agg~group + (1|site), contrasts = list(group = cMat), data = ltemp4)
fit5 = lmer(agg~group + (1|site), contrasts = list(group = cMat), data = ltemp5)
fit6 = lmer(agg~group + (1|site), contrasts = list(group = cMat), data = ltemp6)

predFun1 = function(fit1) {
  predict(fit1,ltemp1, re.form = NA, allow.new.levels=TRUE)
}

predFun2 = function(fit2) {
  predict(fit2,ltemp2, re.form = NA, allow.new.levels=TRUE)
}

predFun3 = function(fit3) {
  predict(fit3,ltemp3, re.form = NA, allow.new.levels=TRUE)
}

predFun4 = function(fit4) {
  predict(fit4,ltemp4, re.form = NA, allow.new.levels=TRUE)
}

predFun5 = function(fit5) {
  predict(fit5,ltemp5, re.form = NA, allow.new.levels=TRUE)
}

predFun6 = function(fit6) {
  predict(fit6,ltemp6, re.form = NA, allow.new.levels=TRUE)
}

bb1 = bootMer(fit1,nsim=100,FUN=predFun1)
bb2 = bootMer(fit2,nsim=100,FUN=predFun2)
bb3 = bootMer(fit3,nsim=100,FUN=predFun3)
bb4 = bootMer(fit4,nsim=100,FUN=predFun4)
bb5 = bootMer(fit5,nsim=100,FUN=predFun5)
bb6 = bootMer(fit6,nsim=100,FUN=predFun6)


for (i in 1:length(ltemp1$agg))
{
  ltemp1$predm[i] = median(bb1$t[,i])
  ltemp1$sel[i] = quantile(bb1$t[,i],0.025)
  ltemp1$ser[i] = quantile(bb1$t[,i],0.975)
}

for (i in 1:length(ltemp2$agg))
{
  ltemp2$predm[i] = median(bb2$t[,i])
  ltemp2$sel[i] = quantile(bb2$t[,i],0.025)
  ltemp2$ser[i] = quantile(bb2$t[,i],0.975)
}

for (i in 1:length(ltemp3$agg))
{
  ltemp3$predm[i] = median(bb3$t[,i])
  ltemp3$sel[i] = quantile(bb3$t[,i],0.025)
  ltemp3$ser[i] = quantile(bb3$t[,i],0.975)
}

for (i in 1:length(ltemp4$agg))
{
  ltemp4$predm[i] = median(bb4$t[,i])
  ltemp4$sel[i] = quantile(bb4$t[,i],0.025)
  ltemp4$ser[i] = quantile(bb4$t[,i],0.975)
}

for (i in 1:length(ltemp5$agg))
{
  ltemp5$predm[i] = median(bb5$t[,i])
  ltemp5$sel[i] = quantile(bb5$t[,i],0.025)
  ltemp5$ser[i] = quantile(bb5$t[,i],0.975)
}

for (i in 1:length(ltemp6$agg))
{
  ltemp6$predm[i] = median(bb6$t[,i])
  ltemp6$sel[i] = quantile(bb6$t[,i],0.025)
  ltemp6$ser[i] = quantile(bb6$t[,i],0.975)
}

temp1$predm = c(ltemp1$predm,ltemp2$predm,ltemp3$predm,ltemp4$predm,ltemp5$predm,ltemp6$predm)
temp1$cil = c(ltemp1$sel,ltemp2$sel,ltemp3$sel,ltemp4$sel,ltemp5$sel,ltemp6$sel)
temp1$cir = c(ltemp1$ser,ltemp2$ser,ltemp3$ser,ltemp4$ser,ltemp5$ser,ltemp6$ser)

temp1$species = factor(temp1$species,levels = c("All species","Psni","Dige","Psma","Lifl","Notr"))
temp1$group = factor(temp1$group,levels = c("Small","Large"))
temp1$highlight = "1"
temp1[temp1$species == "Psma",]$highlight = "2"
temp1$highlight = as.factor(temp1$highlight)

ggp = ggplot(temp1, aes(x = group, y = predm, col = highlight))  +
  facet_wrap(~species, scale = "free") +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = cil, ymax = cir), width = 0.1, size = 0.6) +
  #geom_smooth(method = "lm", alpha = 0.1) +
  xlab("Fragment size") +
  ylab("Saplings per adult") 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  #scale_y_continuous(breaks = seq(0,100,10)) +
  scale_size(guide = 'none') +
  scale_color_manual(values = c("light blue", "red"), name="",
                     breaks=c("1","2"),
                     labels=c("non-significant", "significant")) +
  theme(legend.position = "none")


## aggregations plot scale - 10.5 is small for two species

temp1 = plots$Dige
temp2 = plots$Notr

small = plots[plots$group == "Small",]
large = plots[plots$group == "Large",]

temp = data.frame(rep(1:2,6))
temp$group = rep(c("Small","Large"),6)
temp$species = rep(c("All species","Psni","Dige","Psma","Lifl","Notr"), each = 2)
temp$species = as.factor(temp$species)
temp$pred = c(mean(small$meanagg),mean(large$meanagg),mean(small$Psni),mean(large$Psni),mean(na.omit(temp1[1:5])),mean(na.omit(temp1[6:8])),
              mean(small$Psma),mean(large$Psma),mean(small$Lifl),mean(large$Lifl),mean(na.omit(temp2[1:5])),mean(na.omit(temp2[6:8])))
temp$sd = c(sd(small$meanagg),sd(large$meanagg),sd(small$Psni),sd(large$Psni),sd(na.omit(temp1[1:5])),sd(na.omit(temp1[6:8])),
              sd(small$Psma),sd(large$Psma),sd(small$Lifl),sd(large$Lifl),sd(na.omit(temp2[1:5])),sd(na.omit(temp2[6:8])))


temp$species = factor(temp$species,levels = c("All species","Psni","Dige","Psma","Lifl","Notr"))
temp$group = factor(temp$group,levels = c("Small","Large"))
temp$highlight = "1"
temp[temp$species == "Psma" | temp$species == "Notr",]$highlight = "2"
temp$highlight = as.factor(temp$highlight)

ggp = ggplot(temp, aes(x = group, y = pred, col = highlight))  +
  facet_wrap(~species, scale = "free") +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = pred-sd, ymax = pred+sd), width = 0.1, size = 0.6) +
  #geom_smooth(method = "lm", alpha = 0.1) +
  xlab("Fragment size") +
  ylab("Saplings per adult") 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  #scale_y_continuous(breaks = seq(0,100,10)) +
  scale_size(guide = 'none') +
  scale_color_manual(values = c("light blue", "red"), name="",
                     breaks=c("1","2"),
                     labels=c("non-significant", "significant")) +
  theme(legend.position = "none")

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

temp1 = plots
temp2 = plots
temp2[temp2$size == 10.5,]$group = "Small"


fit1 = lm(meanagg~group, contrasts = list(group = cMat), data = temp1)
fit2 = lm(Psni~group, contrasts = list(group = cMat), data = temp1)
fit3 = lm(Dige~group, contrasts = list(group = cMat), data = temp2)
fit4 = lm(Psma~group, contrasts = list(group = cMat), data = temp1)
fit5 = lm(Lifl~group, contrasts = list(group = cMat), data = temp1)
fit6 = lm(Notr~group, contrasts = list(group = cMat), data = temp2)






## aggregations seedlings quadrat scale

temp = data.frame(rep(1:128,4))
names(temp) = "square"
temp$size = rep(quadrats$size,4)
temp$site = rep(quadrats$site,4)
temp$group = rep(quadrats$group,4)
temp$species = rep(c("All species","Psni","Psma","Lifl"), each = 128)
temp$species = as.factor(temp$species)
temp$agg = c(quadrats$sgmeanagg,quadrats$sgPsni,quadrats$sgPsma,quadrats$sgLifl)


mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

temp1 = temp

temp1$predm = 0
temp1$cil = 0
temp1$cir = 0

ltemp1 = temp1[temp1$species == "All species",]
ltemp2 = temp1[temp1$species == "Psni",]
ltemp3 = temp1[temp1$species == "Psma",]
ltemp4 = temp1[temp1$species == "Lifl",]


fit1 = lmer(agg~group + (1|site), contrasts = list(group = cMat), data = ltemp1)
fit2 = lmer(agg~group + (1|site), contrasts = list(group = cMat), data = ltemp2)
fit3 = lmer(agg~group + (1|site), contrasts = list(group = cMat), data = ltemp3)
fit4 = lmer(agg~group + (1|site), contrasts = list(group = cMat), data = ltemp4)


predFun1 = function(fit1) {
  predict(fit1,ltemp1, re.form = NA, allow.new.levels=TRUE)
}

predFun2 = function(fit2) {
  predict(fit2,ltemp2, re.form = NA, allow.new.levels=TRUE)
}

predFun3 = function(fit3) {
  predict(fit3,ltemp3, re.form = NA, allow.new.levels=TRUE)
}

predFun4 = function(fit4) {
  predict(fit4,ltemp4, re.form = NA, allow.new.levels=TRUE)
}


bb1 = bootMer(fit1,nsim=100,FUN=predFun1)
bb2 = bootMer(fit2,nsim=100,FUN=predFun2)
bb3 = bootMer(fit3,nsim=100,FUN=predFun3)
bb4 = bootMer(fit4,nsim=100,FUN=predFun4)


for (i in 1:length(ltemp1$agg))
{
  ltemp1$predm[i] = median(bb1$t[,i])
  ltemp1$sel[i] = quantile(bb1$t[,i],0.025)
  ltemp1$ser[i] = quantile(bb1$t[,i],0.975)
}

for (i in 1:length(ltemp2$agg))
{
  ltemp2$predm[i] = median(bb2$t[,i])
  ltemp2$sel[i] = quantile(bb2$t[,i],0.025)
  ltemp2$ser[i] = quantile(bb2$t[,i],0.975)
}

for (i in 1:length(ltemp3$agg))
{
  ltemp3$predm[i] = median(bb3$t[,i])
  ltemp3$sel[i] = quantile(bb3$t[,i],0.025)
  ltemp3$ser[i] = quantile(bb3$t[,i],0.975)
}

for (i in 1:length(ltemp4$agg))
{
  ltemp4$predm[i] = median(bb4$t[,i])
  ltemp4$sel[i] = quantile(bb4$t[,i],0.025)
  ltemp4$ser[i] = quantile(bb4$t[,i],0.975)
}


temp1$predm = c(ltemp1$predm,ltemp2$predm,ltemp3$predm,ltemp4$predm)
temp1$cil = c(ltemp1$sel,ltemp2$sel,ltemp3$sel,ltemp4$sel)
temp1$cir = c(ltemp1$ser,ltemp2$ser,ltemp3$ser,ltemp4$ser)

temp1$species = factor(temp1$species,levels = c("All species","Psni","Psma","Lifl"))
temp1$group = factor(temp1$group,levels = c("Small","Large"))

ggp = ggplot(temp1, aes(x = group, y = predm))  +
  facet_wrap(~species, scale = "free") +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = cil, ymax = cir), width = 0.1) +
  #geom_smooth(method = "lm", alpha = 0.1) +
  xlab("Fragment size") +
  ylab("Seedlings per adult") 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  #scale_y_continuous(breaks = seq(0,100,10)) +
  scale_size(guide = 'none') +
  #scale_color_manual(values = c("#D55E00", "#009E73"), name="",
  #                   breaks=c("1","5"),
  #                   labels=c("Control", "Fungicide"))
  theme(legend.position = "none")



## aggregations plot scale seedlings

small = plots[plots$group == "Small",]
large = plots[plots$group == "Large",]

temp = data.frame(rep(1:2,4))
temp$group = rep(c("Small","Large"),4)
temp$species = rep(c("All species","Psni","Psma","Lifl"), each = 2)
temp$species = as.factor(temp$species)
temp$pred = c(mean(small$sgmeanagg),mean(large$sgmeanagg),mean(small$sgPsni),mean(large$sgPsni),
              mean(small$sgPsma),mean(large$sgPsma),mean(small$sgLifl),mean(large$sgLifl))
temp$sd = c(sd(small$sgmeanagg),sd(large$sgmeanagg),sd(small$sgPsni),sd(large$sgPsni),
            sd(small$sgPsma),sd(large$sgPsma),sd(small$sgLifl),sd(large$sgLifl))


temp$species = factor(temp$species,levels = c("All species","Psni","Psma","Lifl"))
temp$group = factor(temp$group,levels = c("Small","Large"))

ggp = ggplot(temp, aes(x = group, y = pred))  +
  facet_wrap(~species, scale = "free") +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = pred-sd, ymax = pred+sd), width = 0.1) +
  #geom_smooth(method = "lm", alpha = 0.1) +
  xlab("Fragment size") +
  ylab("Seedlings per adult") 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  #scale_y_continuous(breaks = seq(0,100,10)) +
  scale_size(guide = 'none') +
  #scale_color_manual(values = c("#D55E00", "#009E73"), name="",
  #                   breaks=c("1","5"),
  #                   labels=c("Control", "Fungicide"))
  theme(legend.position = "none")










############################# Seedlings and saplings per adult ########################

temp = data.frame(rep(1:128,6))
names(temp) = "square"
temp$size = rep(quadrats$size,6)
temp$site = rep(quadrats$site,6)
temp$group = rep(quadrats$group,6)
temp$species = rep(c("All species","Psni","Dige","Psma","Lifl","Notr"), each = 128)
temp$species = as.factor(temp$species)
temp$agg = c(quadrats$sgslmeanagg,quadrats$sgslPsni,quadrats$sgslDige,quadrats$sgslPsma,quadrats$sgslLifl,quadrats$sgslNotr)
temp[temp$species == "Dige",]$group[65:80] = "Small"
temp[temp$species == "Notr",]$group[65:80] = "Small"

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

temp1 = temp

temp1$predm = 0
temp1$cil = 0
temp1$cir = 0

ltemp1 = temp1[temp1$species == "All species",]
ltemp2 = temp1[temp1$species == "Psni",]
ltemp3 = temp1[temp1$species == "Dige",]
ltemp4 = temp1[temp1$species == "Psma",]
ltemp5 = temp1[temp1$species == "Lifl",]
ltemp6 = temp1[temp1$species == "Notr",]  

fit1 = lmer(agg~group + (1|site), contrasts = list(group = cMat), data = ltemp1)
fit2 = lmer(agg~group + (1|site), contrasts = list(group = cMat), data = ltemp2)
fit3 = lmer(agg~group + (1|site), contrasts = list(group = cMat), data = ltemp3)
fit4 = lmer(agg~group + (1|site), contrasts = list(group = cMat), data = ltemp4)
fit5 = lmer(agg~group + (1|site), contrasts = list(group = cMat), data = ltemp5)
fit6 = lmer(agg~group + (1|site), contrasts = list(group = cMat), data = ltemp6)

predFun1 = function(fit1) {
  predict(fit1,ltemp1, re.form = NA, allow.new.levels=TRUE)
}

predFun2 = function(fit2) {
  predict(fit2,ltemp2, re.form = NA, allow.new.levels=TRUE)
}

predFun3 = function(fit3) {
  predict(fit3,ltemp3, re.form = NA, allow.new.levels=TRUE)
}

predFun4 = function(fit4) {
  predict(fit4,ltemp4, re.form = NA, allow.new.levels=TRUE)
}

predFun5 = function(fit5) {
  predict(fit5,ltemp5, re.form = NA, allow.new.levels=TRUE)
}

predFun6 = function(fit6) {
  predict(fit6,ltemp6, re.form = NA, allow.new.levels=TRUE)
}

bb1 = bootMer(fit1,nsim=100,FUN=predFun1)
bb2 = bootMer(fit2,nsim=100,FUN=predFun2)
bb3 = bootMer(fit3,nsim=100,FUN=predFun3)
bb4 = bootMer(fit4,nsim=100,FUN=predFun4)
bb5 = bootMer(fit5,nsim=100,FUN=predFun5)
bb6 = bootMer(fit6,nsim=100,FUN=predFun6)


for (i in 1:length(ltemp1$agg))
{
  ltemp1$predm[i] = median(bb1$t[,i])
  ltemp1$sel[i] = quantile(bb1$t[,i],0.025)
  ltemp1$ser[i] = quantile(bb1$t[,i],0.975)
}

for (i in 1:length(ltemp2$agg))
{
  ltemp2$predm[i] = median(bb2$t[,i])
  ltemp2$sel[i] = quantile(bb2$t[,i],0.025)
  ltemp2$ser[i] = quantile(bb2$t[,i],0.975)
}

for (i in 1:length(ltemp3$agg))
{
  ltemp3$predm[i] = median(bb3$t[,i])
  ltemp3$sel[i] = quantile(bb3$t[,i],0.025)
  ltemp3$ser[i] = quantile(bb3$t[,i],0.975)
}

for (i in 1:length(ltemp4$agg))
{
  ltemp4$predm[i] = median(bb4$t[,i])
  ltemp4$sel[i] = quantile(bb4$t[,i],0.025)
  ltemp4$ser[i] = quantile(bb4$t[,i],0.975)
}

for (i in 1:length(ltemp5$agg))
{
  ltemp5$predm[i] = median(bb5$t[,i])
  ltemp5$sel[i] = quantile(bb5$t[,i],0.025)
  ltemp5$ser[i] = quantile(bb5$t[,i],0.975)
}

for (i in 1:length(ltemp6$agg))
{
  ltemp6$predm[i] = median(bb6$t[,i])
  ltemp6$sel[i] = quantile(bb6$t[,i],0.025)
  ltemp6$ser[i] = quantile(bb6$t[,i],0.975)
}

temp1$predm = c(ltemp1$predm,ltemp2$predm,ltemp3$predm,ltemp4$predm,ltemp5$predm,ltemp6$predm)
temp1$cil = c(ltemp1$sel,ltemp2$sel,ltemp3$sel,ltemp4$sel,ltemp5$sel,ltemp6$sel)
temp1$cir = c(ltemp1$ser,ltemp2$ser,ltemp3$ser,ltemp4$ser,ltemp5$ser,ltemp6$ser)

temp1$species = factor(temp1$species,levels = c("All species","Psni","Dige","Psma","Lifl","Notr"))
temp1$group = factor(temp1$group,levels = c("Small","Large"))
temp1$highlight = "1"
temp1[temp1$species == "Psma",]$highlight = "2"
temp1$highlight = as.factor(temp1$highlight)

ggp = ggplot(temp1, aes(x = group, y = predm, col = highlight))  +
  facet_wrap(~species, scale = "free") +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = cil, ymax = cir), width = 0.1, size = 0.6) +
  #geom_smooth(method = "lm", alpha = 0.1) +
  xlab("Fragment size") +
  ylab("Saplings per adult") 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  #scale_y_continuous(breaks = seq(0,100,10)) +
  scale_size(guide = 'none') +
  scale_color_manual(values = c("light blue", "red"), name="",
                     breaks=c("1","2"),
                     labels=c("non-significant", "significant")) +
  theme(legend.position = "none")


## aggregations plot scale - 10.5 is small for two species

temp1 = plots$sgslDige
temp2 = plots$sgslNotr

small = plots[plots$group == "Small",]
large = plots[plots$group == "Large",]

temp = data.frame(rep(1:2,6))
temp$group = rep(c("Small","Large"),6)
temp$species = rep(c("All species","Psni","Dige","Psma","Lifl","Notr"), each = 2)
temp$species = as.factor(temp$species)
temp$pred = c(mean(small$sgslmeanagg),mean(large$sgslmeanagg),mean(small$sgslPsni),mean(large$sgslPsni),mean(na.omit(temp1[1:5])),mean(na.omit(temp1[6:8])),
              mean(small$sgslPsma),mean(large$sgslPsma),mean(small$sgslLifl),mean(large$sgslLifl),mean(na.omit(temp2[1:5])),mean(na.omit(temp2[6:8])))
temp$sd = c(sd(small$sgslmeanagg),sd(large$sgslmeanagg),sd(small$sgslPsni),sd(large$sgslPsni),sd(na.omit(temp1[1:5])),sd(na.omit(temp1[6:8])),
            sd(small$sgslPsma),sd(large$sgslPsma),sd(small$sgslLifl),sd(large$sgslLifl),sd(na.omit(temp2[1:5])),sd(na.omit(temp2[6:8])))


temp$species = factor(temp$species,levels = c("All species","Psni","Dige","Psma","Lifl","Notr"))
temp$group = factor(temp$group,levels = c("Small","Large"))
temp$highlight = "1"
temp[temp$species == "Psma" | temp$species == "Notr",]$highlight = "2"
temp$highlight = as.factor(temp$highlight)

ggp = ggplot(temp, aes(x = group, y = pred, col = highlight))  +
  facet_wrap(~species, scale = "free") +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = pred-sd, ymax = pred+sd), width = 0.1, size = 0.6) +
  #geom_smooth(method = "lm", alpha = 0.1) +
  xlab("Fragment size") +
  ylab("Saplings per adult") 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  #scale_y_continuous(breaks = seq(0,100,10)) +
  scale_size(guide = 'none') +
  scale_color_manual(values = c("light blue", "red"), name="",
                     breaks=c("1","2"),
                     labels=c("non-significant", "significant")) +
  theme(legend.position = "none")

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

temp1 = plots

ltemp1 = temp1[temp1$species == "All species",]
ltemp2 = temp1[temp1$species == "Psni",]
ltemp3 = temp1[temp1$species == "Dige",]
ltemp4 = temp1[temp1$species == "Psma",]
ltemp5 = temp1[temp1$species == "Lifl",]
ltemp6 = temp1[temp1$species == "Notr",]  

fit1 = lm(agg~group, contrasts = list(group = cMat), data = ltemp1)
fit2 = lm(agg~group, contrasts = list(group = cMat), data = ltemp2)
fit3 = lm(agg~group, contrasts = list(group = cMat), data = ltemp3)
fit4 = lm(agg~group, contrasts = list(group = cMat), data = ltemp4)
fit5 = lm(agg~group, contrasts = list(group = cMat), data = ltemp5)
fit6 = lm(agg~group, contrasts = list(group = cMat), data = ltemp6)



############# diversity vs. fragment size


temp = data.frame(c(1:128,1:128))
temp$size = c(quadrats$size,quadrats$size)
temp$stage = c(rep("Standardized saplings",128),rep("Adults",128))
temp = temp[,-1]
temp$div = NA
temp[quadrats$divsa != 0,]$div = c((((quadrats[quadrats$divsa != 0,]$divsgsl-1.7)/quadrats[quadrats$divsa != 0,]$divsa) + 1.7), quadrats[quadrats$divsa != 0,]$divsa)

temp = temp[!is.na(temp$div),]
temp1 = summarySE(temp, groupvars = c("size","stage"), measurevar = "div")
head(temp1)  

library("scales")
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

ggp = ggplot(temp1, aes(x = size, y = div))  +
  facet_wrap(~stage, ncol = 2) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = div-ci, ymax = div+ci), width = 0.1, size = 0.6) +
  geom_smooth(method = "lm", alpha = 0.1) +
  xlab("Fragment size (ha)") +
  ylab("Diversity") 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  scale_x_continuous(trans = "reverse") +
  #scale_x_continuous(trans=reverselog_trans(10)) +
  scale_size(guide = 'none') +
  #scale_color_manual(values = c("light blue", "red"), name="",
  #                   breaks=c("1","2"),
  #                   labels=c("non-significant", "significant")) +
  theme(legend.position = "none")


#### canopy ####


can = summarySE(canopy, groupvar = "size", measurevar = "canopy")

library("scales")
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

ggp = ggplot(can, aes(x = size, y = canopy))  +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = canopy-ci, ymax = canopy+ci), width = 0.1, size = 0.6) +
  geom_smooth(data = canopy, aes(x = size, y = canopy), method = "lm", alpha = 0.1) +
  xlab("Fragment size (ha)") +
  ylab("Proportion canopy cover") 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  #scale_y_continuous(breaks = seq(0,100,10)) +
  scale_x_continuous(trans=reverselog_trans(1)) +
  scale_size(guide = 'none') +
  #scale_color_manual(values = c("light blue", "red"), name="",
  #                   breaks=c("1","2"),
  #                   labels=c("non-significant", "significant")) +
  theme(legend.position = "none")
