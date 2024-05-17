library(ggplot2)
library(ggthemes)
library(tidyverse)
library(vegan)
library(grid)
library(geosphere)
library(mgcv)
library(lme4)
library(lmerTest)

theme_set(theme_tufte())

load("curated_data_field.RData")
seedling_census_data = seedling_census_data %>%
  left_join(metadata_site_level) %>%
  left_join(metadata_plot_level)

## all species

y = table(seedling_census_data[seedling_census_data$census.start != 0,]$species)
#y = y[y>=5]

temp = seedling_census_data[seedling_census_data$census.start >= 1 & seedling_census_data$species != "SC" & seedling_census_data$species %in% names(y),]
temp = temp[temp$species != "Hopea" & temp$species != "Artoh" & temp$species != "Palm",]
#temp[temp$plot == "6",]$treatment.fungicide = 2
temp$treatment.fungicide = as.factor(temp$treatment.fungicide)
#temp[temp$plot == "5",]$treatment.insecticide = 2
temp$treatment.insecticide = as.factor(temp$treatment.insecticide)
temp$statusm = scale(temp$census.start, center = F)
temp$sizem = scale(temp$fragment.size, center = F)
temp$logdens = log(temp$census.start)
temp$logdensm = scale(temp$logdens)
temp[is.na(temp$slope.degrees),]$slope.degrees = 5 
temp$slope.degrees = temp$slope.degrees/100
temp$slope.degrees = scale(temp$slope.degrees, center = F)


temp = temp[temp$census.start != 49,]
temp = temp[temp$species != "Symp" | temp$plot != "5" | temp$census.start != 15,]


temp$plotx = temp$plot
temp[temp$plotx == "2",]$plotx = "1"

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

temp1 = temp

fit9a = glmer (proportion.mortality ~ treatment.fungicide*treatment.insecticide*statusm + treatment.fungicide*treatment.insecticide*sizem + slope.degrees + (1|species) + (0 + statusm|species) +
                 (1|location.code/group/plot), weights = census.start, data = temp1, contrasts = list(treatment.fungicide = cMat,
                 treatment.insecticide = cMat), family="binomial", nAGQ = 0)





# SR

temp = seedling_census_data[seedling_census_data$census.start >= 1 & seedling_census_data$species == "SR" & seedling_census_data$census.start <= 300,]
#temp[temp$plot == "6",]$treatment.fungicide = 2
temp$treatment.fungicide = as.factor(temp$treatment.fungicide)
#temp[temp$plot == "5",]$treatment.insecticide = 2
temp$treatment.insecticide = as.factor(temp$treatment.insecticide)
temp$statusm = scale(temp$census.start, center = F)
temp$sizem = scale(temp$fragment.size, center = F)
temp$logdens = log(temp$census.start)
temp$logdensm = scale(temp$logdens)
#temp[is.na(temp$slope.degrees),]$slope.degrees = 5 
temp$slope.degrees = temp$slope.degrees/100
temp$slope.degrees = scale(temp$slope.degrees, center = F)
temp = temp[-c(142,49),]

temp$plotx = temp$plot
temp[temp$plotx == "2",]$plotx = "1"

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

temp2 = temp

fit9b = glmer (proportion.mortality ~ treatment.fungicide*treatment.insecticide*statusm + treatment.fungicide*treatment.insecticide*sizem + slope.degrees + 
                 (1|location.code/group/plot), weights = census.start, data = temp2, 
               contrasts = list(treatment.fungicide = cMat,treatment.insecticide = cMat), family="binomial", nAGQ = 0)





# SP


temp = seedling_census_data[seedling_census_data$census.start >= 1 & seedling_census_data$species == "Climber1",]
#temp[temp$plot == "6",]$treatment.fungicide = 2
temp$treatment.fungicide = as.factor(temp$treatment.fungicide)
#temp[temp$plot == "5",]$treatment.insecticide = 2
temp$treatment.insecticide = as.factor(temp$treatment.insecticide)
temp$statusm = scale(temp$census.start, center = F)
temp$sizem = scale(temp$fragment.size, center = F)
temp$logdens = log(temp$census.start)
temp$logdensm = scale(temp$logdens)
#temp[is.na(temp$slope.degrees),]$slope.degrees = 5 
temp$slope.degrees = temp$slope.degrees/100
temp$slope.degrees = scale(temp$slope.degrees, center = F)
temp = temp[temp$census.start != 49,]

temp$plotx = temp$plot
temp[temp$plotx == "2",]$plotx = "1"


mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

temp3 = temp

fit9c = glmer (proportion.mortality ~ treatment.fungicide*treatment.insecticide*statusm + slope.degrees + 
                 (1|group.code/plot), weights = census.start, data = temp3,
               contrasts = list(treatment.fungicide = cMat,treatment.insecticide = cMat), family="binomial", nAGQ = 0)





# Symp

temp = seedling_census_data[seedling_census_data$census.start >= 1 & seedling_census_data$species == "Symp",]
#temp[temp$plot == "6",]$treatment.fungicide = 2
temp$treatment.fungicide = as.factor(temp$treatment.fungicide)
#temp[temp$plot == "5",]$treatment.insecticide = 2
temp$treatment.insecticide = as.factor(temp$treatment.insecticide)
temp$statusm = scale(temp$census.start, center = F)
temp$sizem = scale(temp$fragment.size, center = F)
temp$logdens = log(temp$census.start)
temp$logdensm = scale(temp$logdens)
temp[is.na(temp$slope.degrees),]$slope.degrees = 5 
temp$slope.degrees = temp$slope.degrees/100
temp$slope.degrees = scale(temp$slope.degrees, center = F)
temp = temp[temp$species != "Symp" | temp$plot != "5" | temp$census.start != 15,]

temp$plotx = temp$plot
temp[temp$plotx == "2",]$plotx = "1"

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

temp4 = temp

fit9d = glmer (proportion.mortality ~ treatment.fungicide*treatment.insecticide*statusm + slope.degrees +
                 (1|location.code/group/plot), weights = census.start, data = temp4,
               contrasts = list(treatment.fungicide = cMat, treatment.insecticide = cMat), family="binomial", nAGQ = 0)






# VM

temp = seedling_census_data[seedling_census_data$census.start >= 1 & seedling_census_data$species == "FLC",]
temp = temp[temp$census.start < 60,]

#temp[temp$plot == "6",]$treatment.fungicide = 2
temp$treatment.fungicide = as.factor(temp$treatment.fungicide)
#temp[temp$plot == "5",]$treatment.insecticide = 2
temp$treatment.insecticide = as.factor(temp$treatment.insecticide)
temp$statusm = scale(temp$census.start, center = F)
temp$sizem = scale(temp$fragment.size, center = F)
temp$logdens = log(temp$census.start)
temp$logdensm = scale(temp$logdens)
#temp[is.na(temp$slope.degrees),]$slope.degrees = 5 
temp$slope.degrees = temp$slope.degrees/100
temp$slope.degrees = scale(temp$slope.degrees, center = F)

temp$plotx = temp$plot
temp[temp$plotx == "2",]$plotx = "1"

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

temp5 = temp

fit9e = glmer (proportion.mortality ~ treatment.fungicide*treatment.insecticide + statusm + slope.degrees + (1|location.code/group),
               weights = census.start, data = temp5, contrasts = list(treatment.fungicide = cMat, treatment.insecticide = cMat),
               family="binomial", nAGQ = 0)





# all others

y = table(seedling_census_data[seedling_census_data$census.start != 0,]$species)
y = y[y>=5]

temp = seedling_census_data[seedling_census_data$census.start >= 1 & seedling_census_data$species != "SR" & seedling_census_data$species != "Symp" & 
                 seedling_census_data$species != "Climber1" & seedling_census_data$species != "SC" & seedling_census_data$species %in% names(y),]
temp = temp[temp$species != "Hopea" & temp$species != "Artoh" & temp$species != "Palm",]
temp = temp[temp$species != "FLC",]

#temp[temp$plot == "6",]$treatment.fungicide = 2
temp$treatment.fungicide = as.factor(temp$treatment.fungicide)
#temp[temp$plot == "5",]$treatment.insecticide = 2
temp$treatment.insecticide = as.factor(temp$treatment.insecticide)
temp$statusm = scale(temp$census.start, center = F)
#temp$AOm = scale(temp$AO, center = F)
temp$sizem = scale(temp$fragment.size, center = F)
temp$logdens = log(temp$census.start)
temp$logdensm = scale(temp$logdens)
temp[is.na(temp$slope.degrees),]$slope.degrees = 5 
temp$slope.degrees = temp$slope.degrees/100
temp$slope.degrees = scale(temp$slope.degrees, center = F)

temp$plotx = temp$plot
temp[temp$plotx == "2",]$plotx = "1"

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

temp6 = temp

fit9f = glmer (proportion.mortality ~ treatment.fungicide*treatment.insecticide*statusm + treatment.fungicide*treatment.insecticide*sizem + slope.degrees + (1|species) + (0 + statusm|species) +
                 (1|location.code/group/plot), weights = census.start, data = temp6,
               contrasts = list(treatment.fungicide = cMat,treatment.insecticide = cMat), family="binomial", nAGQ = 0)





##################### plotting - all species ######################



temp1$pred = predict(fit9a, type = "response", re.form = NA, allow.new.levels=TRUE)

a = predict(fit9a, type = "response", re.form = NA)
b = predict(fit9a, type = "response")
c = b - a
temp1$newproportion.mortality = temp1$proportion.mortality - c
temp1[temp1$newproportion.mortality > 1,]$newproportion.mortality = 1
temp1[temp1$newproportion.mortality < 0,]$newproportion.mortality = 0

temp1$predm = 0
temp1$sel = 0
temp1$ser = 0


ltemp = temp1

predFun = function(fit) {
  predict(fit,ltemp, re.form = NA, allow.new.levels=TRUE)
}

bb = bootMer(fit9a,nsim=100,FUN=predFun)

for (i in 1:length(temp1$proportion.mortality))
{
  temp1$predm[i] = binomial()$linkinv(median(bb$t[,i]))
  temp1$sel[i] = binomial()$linkinv(median(bb$t[,i]) - sd(bb$t[,i]))
  temp1$ser[i] = binomial()$linkinv(median(bb$t[,i]) + sd(bb$t[,i]))
}

ggp = ggplot(temp1, aes(x = census.start, y = predm, col = as.factor(plotx)))  +
  geom_point(size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F) +
  geom_smooth(aes(x = census.start, y = sel, col = as.factor(plotx)), method = "glm", method.args = list(family = "binomial"), se = F) +
  
  geom_smooth(aes(x = census.start, y = ser, col = as.factor(plotx)), method = "glm", method.args = list(family = "binomial"), se = F)

##### 1

selribx = ggplot_build(ggp)$data[[3]]$x[1:80]
selriby = ggplot_build(ggp)$data[[3]]$y[1:80]

serribx = ggplot_build(ggp)$data[[4]]$x[1:80]
serriby = ggplot_build(ggp)$data[[4]]$y[1:80]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$treatment.control == 1,]$proportion.mortality),replace = T),]
temp1$selribx[temp1$treatment.control == 1] = srib$selribx
temp1$selriby[temp1$treatment.control == 1] = srib$selriby
temp1$serriby[temp1$treatment.control == 1] = srib$serriby

##### 2

selribx = ggplot_build(ggp)$data[[3]]$x[81:160]
selriby = ggplot_build(ggp)$data[[3]]$y[81:160]

serribx = ggplot_build(ggp)$data[[4]]$x[81:160]
serriby = ggplot_build(ggp)$data[[4]]$y[81:160]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "5",]$proportion.mortality),replace = T),]
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

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "6",]$proportion.mortality),replace = T),]
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

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "7",]$proportion.mortality),replace = T),]
temp1$selribx[temp1$plot == "7"] = srib$selribx
temp1$selriby[temp1$plot == "7"] = srib$selriby
temp1$serriby[temp1$plot == "7"] = srib$serriby



ggp = ggplot(temp1, aes(x = census.start, y = predm, col = as.factor(plotx), fill = as.factor(plotx)))  +
  geom_point(aes(x = census.start, y = newproportion.mortality, col = as.factor(plotx)), size = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F, linewidth = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab("") +
  ylab("") 
ggp1 = ggp + 
  #ggtitle("all species") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,300)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))




##################### plotting - SR ######################



temp2$pred = predict(fit9b, type = "response", re.form = NA, allow.new.levels=TRUE)

a = predict(fit9b, type = "response", re.form = NA)
b = predict(fit9b, type = "response")
c = b - a
temp2$newproportion.mortality = temp2$proportion.mortality - c
temp2[temp2$newproportion.mortality > 1,]$newproportion.mortality = 1
temp2[temp2$newproportion.mortality < 0,]$newproportion.mortality = 0

temp2$predm = 0
temp2$sel = 0
temp2$ser = 0


ltemp = temp2

predFun = function(fit) {
  predict(fit,ltemp, re.form = NA, allow.new.levels=TRUE)
}

bb = bootMer(fit9b,nsim=100,FUN=predFun)

for (i in 1:length(temp2$proportion.mortality))
{
  temp2$predm[i] = binomial()$linkinv(median(bb$t[,i]))
  temp2$sel[i] = binomial()$linkinv(median(bb$t[,i]) - sd(bb$t[,i]))
  temp2$ser[i] = binomial()$linkinv(median(bb$t[,i]) + sd(bb$t[,i]))
}

ggp = ggplot(temp2, aes(x = census.start, y = predm, col = as.factor(plotx)))  +
  geom_point(size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F) +
  geom_smooth(aes(x = census.start, y = sel, col = as.factor(plotx)), method = "glm", method.args = list(family = "binomial"), se = F) +
  
  geom_smooth(aes(x = census.start, y = ser, col = as.factor(plotx)), method = "glm", method.args = list(family = "binomial"), se = F)

##### 1

selribx = ggplot_build(ggp)$data[[3]]$x[1:80]
selriby = ggplot_build(ggp)$data[[3]]$y[1:80]

serribx = ggplot_build(ggp)$data[[4]]$x[1:80]
serriby = ggplot_build(ggp)$data[[4]]$y[1:80]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp2[temp2$treatment.control == 1,]$proportion.mortality),replace = T),]
temp2$selribx[temp2$treatment.control == 1] = srib$selribx
temp2$selriby[temp2$treatment.control == 1] = srib$selriby
temp2$serriby[temp2$treatment.control == 1] = srib$serriby

##### 2

selribx = ggplot_build(ggp)$data[[3]]$x[81:160]
selriby = ggplot_build(ggp)$data[[3]]$y[81:160]

serribx = ggplot_build(ggp)$data[[4]]$x[81:160]
serriby = ggplot_build(ggp)$data[[4]]$y[81:160]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp2[temp2$plot == "5",]$proportion.mortality),replace = T),]
temp2$selribx[temp2$plot == "5"] = srib$selribx
temp2$selriby[temp2$plot == "5"] = srib$selriby
temp2$serriby[temp2$plot == "5"] = srib$serriby

##### 3

selribx = ggplot_build(ggp)$data[[3]]$x[161:240]
selriby = ggplot_build(ggp)$data[[3]]$y[161:240]

serribx = ggplot_build(ggp)$data[[4]]$x[161:240]
serriby = ggplot_build(ggp)$data[[4]]$y[161:240]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp2[temp2$plot == "6",]$proportion.mortality),replace = T),]
temp2$selribx[temp2$plot == "6"] = srib$selribx
temp2$selriby[temp2$plot == "6"] = srib$selriby
temp2$serriby[temp2$plot == "6"] = srib$serriby

##### 4

selribx = ggplot_build(ggp)$data[[3]]$x[241:320]
selriby = ggplot_build(ggp)$data[[3]]$y[241:320]

serribx = ggplot_build(ggp)$data[[4]]$x[241:320]
serriby = ggplot_build(ggp)$data[[4]]$y[241:320]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp2[temp2$plot == "7",]$proportion.mortality),replace = T),]
temp2$selribx[temp2$plot == "7"] = srib$selribx
temp2$selriby[temp2$plot == "7"] = srib$selriby
temp2$serriby[temp2$plot == "7"] = srib$serriby



ggp = ggplot(temp2, aes(x = census.start, y = predm, col = as.factor(plotx), fill = as.factor(plotx)))  +
  geom_point(aes(x = census.start, y = newproportion.mortality, col = as.factor(plotx)), size = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F, linewidth = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab("") +
  ylab(expression(paste(italic("Syzygium rubicundum"))))
ggp2 = ggp + 
  #ggtitle("Syzygium rubicundum") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 9),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,300)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))





##################### plotting - SP ######################



temp3$pred = predict(fit9c, type = "response", re.form = NA, allow.new.levels=TRUE)

a = predict(fit9c, type = "response", re.form = NA)
b = predict(fit9c, type = "response")
c = b - a
temp3$newproportion.mortality = temp3$proportion.mortality - c
temp3[temp3$newproportion.mortality > 1,]$newproportion.mortality = 1
#temp3[temp3$newproportion.mortality < 0,]$newproportion.mortality = 0

temp3$predm = 0
temp3$sel = 0
temp3$ser = 0


ltemp = temp3

predFun = function(fit) {
  predict(fit,ltemp, re.form = NA, allow.new.levels=TRUE)
}

bb = bootMer(fit9c,nsim=100,FUN=predFun)

for (i in 1:length(temp3$proportion.mortality))
{
  temp3$predm[i] = binomial()$linkinv(median(bb$t[,i]))
  temp3$sel[i] = binomial()$linkinv(median(bb$t[,i]) - sd(bb$t[,i]))
  temp3$ser[i] = binomial()$linkinv(median(bb$t[,i]) + sd(bb$t[,i]))
}

ggp = ggplot(temp3, aes(x = census.start, y = predm, col = as.factor(plotx)))  +
  geom_point(size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F) +
  geom_smooth(aes(x = census.start, y = sel, col = as.factor(plotx)), method = "glm", method.args = list(family = "binomial"), se = F) +
  
  geom_smooth(aes(x = census.start, y = ser, col = as.factor(plotx)), method = "glm", method.args = list(family = "binomial"), se = F)

##### 1

selribx = ggplot_build(ggp)$data[[3]]$x[1:80]
selriby = ggplot_build(ggp)$data[[3]]$y[1:80]

serribx = ggplot_build(ggp)$data[[4]]$x[1:80]
serriby = ggplot_build(ggp)$data[[4]]$y[1:80]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp3[temp3$treatment.control == 1,]$proportion.mortality),replace = T),]
temp3$selribx[temp3$treatment.control == 1] = srib$selribx
temp3$selriby[temp3$treatment.control == 1] = srib$selriby
temp3$serriby[temp3$treatment.control == 1] = srib$serriby

##### 2

selribx = ggplot_build(ggp)$data[[3]]$x[81:160]
selriby = ggplot_build(ggp)$data[[3]]$y[81:160]

serribx = ggplot_build(ggp)$data[[4]]$x[81:160]
serriby = ggplot_build(ggp)$data[[4]]$y[81:160]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp3[temp3$plot == "5",]$proportion.mortality),replace = T),]
temp3$selribx[temp3$plot == "5"] = srib$selribx
temp3$selriby[temp3$plot == "5"] = srib$selriby
temp3$serriby[temp3$plot == "5"] = srib$serriby

##### 3

selribx = ggplot_build(ggp)$data[[3]]$x[161:240]
selriby = ggplot_build(ggp)$data[[3]]$y[161:240]

serribx = ggplot_build(ggp)$data[[4]]$x[161:240]
serriby = ggplot_build(ggp)$data[[4]]$y[161:240]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp3[temp3$plot == "6",]$proportion.mortality),replace = T),]
temp3$selribx[temp3$plot == "6"] = srib$selribx
temp3$selriby[temp3$plot == "6"] = srib$selriby
temp3$serriby[temp3$plot == "6"] = srib$serriby

##### 4

selribx = ggplot_build(ggp)$data[[3]]$x[241:320]
selriby = ggplot_build(ggp)$data[[3]]$y[241:320]

serribx = ggplot_build(ggp)$data[[4]]$x[241:320]
serriby = ggplot_build(ggp)$data[[4]]$y[241:320]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp3[temp3$plot == "7",]$proportion.mortality),replace = T),]
temp3$selribx[temp3$plot == "7"] = srib$selribx
temp3$selriby[temp3$plot == "7"] = srib$selriby
temp3$serriby[temp3$plot == "7"] = srib$serriby



ggp = ggplot(temp3, aes(x = census.start, y = predm, col = as.factor(plotx), fill = as.factor(plotx)))  +
  geom_point(aes(x = census.start, y = newproportion.mortality, col = as.factor(plotx)), size = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F, linewidth = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab("") +
  ylab("")
ggp3 = ggp + 
  #ggtitle("Spatholobus purpureus") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 9),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,100)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))



##################### plotting - all other species ######################



temp6$pred = predict(fit9f, type = "response", re.form = NA, allow.new.levels=TRUE)

a = predict(fit9f, type = "response", re.form = NA)
b = predict(fit9f, type = "response")
c = b - a
temp6$newproportion.mortality = temp6$proportion.mortality - c
temp6[temp6$newproportion.mortality > 1,]$newproportion.mortality = 1
temp6[temp6$newproportion.mortality < 0,]$newproportion.mortality = 0

temp6$predm = 0
temp6$sel = 0
temp6$ser = 0


ltemp = temp6

predFun = function(fit) {
  predict(fit,ltemp, re.form = NA, allow.new.levels=TRUE)
}

bb = bootMer(fit9f,nsim=100,FUN=predFun)

for (i in 1:length(temp6$proportion.mortality))
{
  temp6$predm[i] = binomial()$linkinv(median(bb$t[,i]))
  temp6$sel[i] = binomial()$linkinv(median(bb$t[,i]) - sd(bb$t[,i]))
  temp6$ser[i] = binomial()$linkinv(median(bb$t[,i]) + sd(bb$t[,i]))
}

ggp = ggplot(temp6, aes(x = census.start, y = predm, col = as.factor(plotx)))  +
  geom_point(size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F) +
  geom_smooth(aes(x = census.start, y = sel, col = as.factor(plotx)), method = "glm", method.args = list(family = "binomial"), se = F) +
  
  geom_smooth(aes(x = census.start, y = ser, col = as.factor(plotx)), method = "glm", method.args = list(family = "binomial"), se = F)

##### 1

selribx = ggplot_build(ggp)$data[[3]]$x[1:80]
selriby = ggplot_build(ggp)$data[[3]]$y[1:80]

serribx = ggplot_build(ggp)$data[[4]]$x[1:80]
serriby = ggplot_build(ggp)$data[[4]]$y[1:80]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp6[temp6$treatment.control == 1,]$proportion.mortality),replace = T),]
temp6$selribx[temp6$treatment.control == 1] = srib$selribx
temp6$selriby[temp6$treatment.control == 1] = srib$selriby
temp6$serriby[temp6$treatment.control == 1] = srib$serriby

##### 2

selribx = ggplot_build(ggp)$data[[3]]$x[81:160]
selriby = ggplot_build(ggp)$data[[3]]$y[81:160]

serribx = ggplot_build(ggp)$data[[4]]$x[81:160]
serriby = ggplot_build(ggp)$data[[4]]$y[81:160]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp6[temp6$plot == "5",]$proportion.mortality),replace = T),]
temp6$selribx[temp6$plot == "5"] = srib$selribx
temp6$selriby[temp6$plot == "5"] = srib$selriby
temp6$serriby[temp6$plot == "5"] = srib$serriby

##### 3

selribx = ggplot_build(ggp)$data[[3]]$x[161:240]
selriby = ggplot_build(ggp)$data[[3]]$y[161:240]

serribx = ggplot_build(ggp)$data[[4]]$x[161:240]
serriby = ggplot_build(ggp)$data[[4]]$y[161:240]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp6[temp6$plot == "6",]$proportion.mortality),replace = T),]
temp6$selribx[temp6$plot == "6"] = srib$selribx
temp6$selriby[temp6$plot == "6"] = srib$selriby
temp6$serriby[temp6$plot == "6"] = srib$serriby

##### 4

selribx = ggplot_build(ggp)$data[[3]]$x[241:320]
selriby = ggplot_build(ggp)$data[[3]]$y[241:320]

serribx = ggplot_build(ggp)$data[[4]]$x[241:320]
serriby = ggplot_build(ggp)$data[[4]]$y[241:320]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp6[temp6$plot == "7",]$proportion.mortality),replace = T),]
temp6$selribx[temp6$plot == "7"] = srib$selribx
temp6$selriby[temp6$plot == "7"] = srib$selriby
temp6$serriby[temp6$plot == "7"] = srib$serriby



ggp = ggplot(temp6, aes(x = census.start, y = predm, col = as.factor(plotx), fill = as.factor(plotx)))  +
  geom_point(aes(x = census.start, y = newproportion.mortality, col = as.factor(plotx)), size = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F, linewidth = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab("conspecific density (per square-meter)") 
ggp4 = ggp + 
  #ggtitle("all other species") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,50)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))




############################


library(gridExtra)
library(grid)

grid_arrange_shared_legend1 <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(top = textGrob("all species", rot=0, vjust = 1,
                              gp = gpar(fontfamily = "serif", fontsize = 10, col = 'black')),
               do.call(arrangeGrob, c(lapply(plots, function(x)
                 x + theme(legend.position="none")), list(nrow = 1))),
               ncol = 1,
               heights = unit.c(unit(0.895, "npc") - lheight, lheight))
}

grid_arrange_shared_legend2 <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(top = textGrob(expression(paste(italic("Syzygium rubicundum"))), rot=0, vjust = 0,
                              gp = gpar(fontfamily = "serif", fontsize = 10, col = 'black')),
               do.call(arrangeGrob, c(lapply(plots, function(x)
                 x + theme(legend.position="none")), list(nrow = 1))),
               ncol = 1,
               heights = unit.c(unit(0.895, "npc") - lheight, lheight))
}

grid_arrange_shared_legend3 <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(top = textGrob(expression(paste(italic("Spatholobus purpureus"))), rot=0, vjust = 0,
                              gp = gpar(fontfamily = "serif", fontsize = 10, col = 'black')),
               do.call(arrangeGrob, c(lapply(plots, function(x)
                 x + theme(legend.position="none")), list(nrow = 1))),
               ncol = 1,
               heights = unit.c(unit(0.895, "npc") - lheight, lheight))
}

grid_arrange_shared_legend4 <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(top = textGrob("all other species", rot=0, vjust = 1,
                              gp = gpar(fontfamily = "serif", fontsize = 10, col = 'black')),
               do.call(arrangeGrob, c(lapply(plots, function(x)
                 x + theme(legend.position="none")), list(nrow = 1))),
               legend,
               ncol = 1,
               heights = unit.c(unit(0.895, "npc") - lheight, lheight))
}

p1 = grid_arrange_shared_legend2(ggp2)
p2 = grid_arrange_shared_legend3(ggp3)
p3 = grid_arrange_shared_legend4(ggp4)

#, bottom=grid::textGrob("conspecific density (per square-meter)") top=grid::textGrob("seedling mortality from germination to establishment",
g = gridExtra::arrangeGrob(p1,p2,p3,nrow=3,ncol=1, 
                            left=grid::textGrob("probability of mortality - germination to establishment", rot=90, gp = gpar(fontfamily = "serif", fontsize = 12, col = 'black'))) ; 
  #grid::grid.newpage() ; 

jpeg('density_dependence.jpg', units="in", width=5, height=7, res=1000)
grid::grid.draw(g)
dev.off()










################ For presentation FUNGI ###################

## empty SR with only fungi

ggpw = ggplot(temp2[temp2$treatment.insecticide != 1,], aes(x = census.start, y = predm, col = as.factor(plotx), fill = as.factor(plotx)))  +
  #geom_point(aes(x = census.start, y = newproportion.mortality, col = as.factor(plotx)), size = 0.5) +
  #geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F, linewidth = 0.5) +
  #geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab(expression(paste(conspecific~density~"/"~m^2))) +
  ylab("per-capita mortality")


ggpw1 = ggpw + 
  ggtitle("Syzygium rubicundum") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,300)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))

ggp = ggplot(temp2[temp2$treatment.insecticide != 1,], aes(x = census.start, y = predm, col = as.factor(plotx), fill = as.factor(plotx)))  +
  geom_point(aes(x = census.start, y = newproportion.mortality, col = as.factor(plotx)), size = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F, linewidth = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab(expression(paste(conspecific~density~"/"~m^2))) +
  ylab("per-capita mortality")


ggp1 = ggp + 
  ggtitle("Syzygium rubicundum") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,300)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))

grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, c(lapply(plots[2], function(x)
      x + theme(legend.position="none")), list(nrow = 1))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

p = grid_arrange_shared_legend(ggp1,ggpw1)

#, bottom=grid::textGrob("conspecific density (per square-meter)") top=grid::textGrob("seedling mortality from germination to establishment",
#g = gridExtra::arrangeGrob(p,ncol=1) ; 
#grid::grid.newpage() ; 

tiff('C:/Users/ashwinv/Desktop/a.tiff', units="in", width=8, height=6, res=1000)
grid_arrange_shared_legend(ggp1,ggpw1)
dev.off()



## only control

ggpw = ggplot(temp2[temp2$treatment.control == 1,], aes(x = census.start, y = predm, col = as.factor(plotx), fill = as.factor(plotx)))  +
  geom_point(aes(x = census.start, y = newproportion.mortality, col = as.factor(plotx)), size = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F, linewidth = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab(expression(paste(conspecific~density~"/"~m^2))) +
  ylab("per-capita mortality")


ggpw1 = ggpw + 
  ggtitle("Syzygium rubicundum") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,300)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))

ggp = ggplot(temp2[temp2$plotx != "7",], aes(x = census.start, y = predm, col = as.factor(plotx), fill = as.factor(plotx)))  +
  geom_point(aes(x = census.start, y = newproportion.mortality, col = as.factor(plotx)), size = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F, linewidth = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab(expression(paste(conspecific~density~"/"~m^2))) +
  ylab("per-capita mortality")


ggp1 = ggp + 
  ggtitle("Syzygium rubicundum") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,300)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))

grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, c(lapply(plots[2], function(x)
      x + theme(legend.position="none")), list(nrow = 1))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

p = grid_arrange_shared_legend(ggp1,ggpw1)

#, bottom=grid::textGrob("conspecific density (per square-meter)") top=grid::textGrob("seedling mortality from germination to establishment",
#g = gridExtra::arrangeGrob(p,ncol=1) ; 
#grid::grid.newpage() ; 

tiff('C:/Users/ashwinv/Desktop/a.tiff', units="in", width=8, height=6, res=1000)
grid_arrange_shared_legend(ggp1,ggpw1)
dev.off()




## control and fungicide

ggpw = ggplot(temp2[temp2$treatment.insecticide != 1,], aes(x = census.start, y = predm, col = as.factor(plotx), fill = as.factor(plotx)))  +
  geom_point(aes(x = census.start, y = newproportion.mortality, col = as.factor(plotx)), size = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F, linewidth = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab(expression(paste(conspecific~density~"/"~m^2))) +
  ylab("per-capita mortality")


ggpw1 = ggpw + 
  ggtitle("Syzygium rubicundum") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,300)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))

ggp = ggplot(temp2[temp2$plotx != "7",], aes(x = census.start, y = predm, col = as.factor(plotx), fill = as.factor(plotx)))  +
  geom_point(aes(x = census.start, y = newproportion.mortality, col = as.factor(plotx)), size = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F, linewidth = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab(expression(paste(conspecific~density~"/"~m^2))) +
  ylab("per-capita mortality")


ggp1 = ggp + 
  ggtitle("Syzygium rubicundum") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,300)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))

grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, c(lapply(plots[2], function(x)
      x + theme(legend.position="none")), list(nrow = 1))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

p = grid_arrange_shared_legend(ggp1,ggpw1)

#, bottom=grid::textGrob("conspecific density (per square-meter)") top=grid::textGrob("seedling mortality from germination to establishment",
#g = gridExtra::arrangeGrob(p,ncol=1) ; 
#grid::grid.newpage() ; 

tiff('C:/Users/ashwinv/Desktop/a.tiff', units="in", width=8, height=6, res=1000)
grid_arrange_shared_legend(ggp1,ggpw1)
dev.off()



## control and fungicide and insecticide

ggpw = ggplot(temp2[temp2$plotx != "7",], aes(x = census.start, y = predm, col = as.factor(plotx), fill = as.factor(plotx)))  +
  geom_point(aes(x = census.start, y = newproportion.mortality, col = as.factor(plotx)), size = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F, linewidth = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab(expression(paste(conspecific~density~"/"~m^2))) +
  ylab("per-capita mortality")


ggpw1 = ggpw + 
  ggtitle("Syzygium rubicundum") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,300)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))

ggp = ggplot(temp2[temp2$plotx != "7",], aes(x = census.start, y = predm, col = as.factor(plotx), fill = as.factor(plotx)))  +
  geom_point(aes(x = census.start, y = newproportion.mortality, col = as.factor(plotx)), size = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F, linewidth = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab(expression(paste(conspecific~density~"/"~m^2))) +
  ylab("per-capita mortality")


ggp1 = ggp + 
  ggtitle("Syzygium rubicundum") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,300)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))

grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, c(lapply(plots[2], function(x)
      x + theme(legend.position="none")), list(nrow = 1))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

p = grid_arrange_shared_legend(ggp1,ggpw1)

#, bottom=grid::textGrob("conspecific density (per square-meter)") top=grid::textGrob("seedling mortality from germination to establishment",
#g = gridExtra::arrangeGrob(p,ncol=1) ; 
#grid::grid.newpage() ; 

tiff('C:/Users/ashwinv/Desktop/a.tiff', units="in", width=8, height=6, res=1000)
grid_arrange_shared_legend(ggp1,ggpw1)
dev.off()













################ For presentation INSECTS ###################

## empty SR with only insects

ggpw = ggplot(temp2[temp2$treatment.fungicide != 1,], aes(x = census.start, y = predm, col = as.factor(plotx), fill = as.factor(plotx)))  +
  #geom_point(aes(x = census.start, y = newproportion.mortality, col = as.factor(plotx)), size = 0.5) +
  #geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F, linewidth = 0.5) +
  #geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab(expression(paste(conspecific~density~"/"~m^2))) +
  ylab("per-capita mortality")


ggpw1 = ggpw + 
  ggtitle("Syzygium rubicundum") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,200)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  scale_color_manual(values = c("#56B4E9","#009E73","#D55E00","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#009E73","#D55E00","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))

ggp = ggplot(temp2[temp2$treatment.fungicide != 1,], aes(x = census.start, y = predm, col = as.factor(plotx), fill = as.factor(plotx)))  +
  geom_point(aes(x = census.start, y = newproportion.mortality, col = as.factor(plotx)), size = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F, linewidth = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab(expression(paste(conspecific~density~"/"~m^2))) +
  ylab("per-capita mortality")


ggp1 = ggp + 
  ggtitle("Syzygium rubicundum") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,200)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  scale_color_manual(values = c("#56B4E9","#009E73","#D55E00","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#009E73","#D55E00","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))

grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, c(lapply(plots[2], function(x)
      x + theme(legend.position="none")), list(nrow = 1))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

p = grid_arrange_shared_legend(ggp1,ggpw1)

#, bottom=grid::textGrob("conspecific density (per square-meter)") top=grid::textGrob("seedling mortality from germination to establishment",
#g = gridExtra::arrangeGrob(p,ncol=1) ; 
#grid::grid.newpage() ; 

tiff('C:/Users/ashwinv/Desktop/a.tiff', units="in", width=8, height=6, res=1000)
grid_arrange_shared_legend(ggp1,ggpw1)
dev.off()



## only control

ggpw = ggplot(temp2[temp2$treatment.control == 1,], aes(x = census.start, y = predm, col = as.factor(plotx), fill = as.factor(plotx)))  +
  geom_point(aes(x = census.start, y = newproportion.mortality, col = as.factor(plotx)), size = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F, linewidth = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab(expression(paste(conspecific~density~"/"~m^2))) +
  ylab("per-capita mortality")


ggpw1 = ggpw + 
  ggtitle("Syzygium rubicundum") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,200)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  scale_color_manual(values = c("#56B4E9","#009E73","#D55E00","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#009E73","#D55E00","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))

ggp = ggplot(temp2[temp2$treatment.fungicide != 1,], aes(x = census.start, y = predm, col = as.factor(plotx), fill = as.factor(plotx)))  +
  geom_point(aes(x = census.start, y = newproportion.mortality, col = as.factor(plotx)), size = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F, linewidth = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab(expression(paste(conspecific~density~"/"~m^2))) +
  ylab("per-capita mortality")


ggp1 = ggp + 
  ggtitle("Syzygium rubicundum") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,200)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  scale_color_manual(values = c("#56B4E9","#009E73","#D55E00","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#009E73","#D55E00","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))

grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, c(lapply(plots[2], function(x)
      x + theme(legend.position="none")), list(nrow = 1))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

p = grid_arrange_shared_legend(ggp1,ggpw1)

#, bottom=grid::textGrob("conspecific density (per square-meter)") top=grid::textGrob("seedling mortality from germination to establishment",
#g = gridExtra::arrangeGrob(p,ncol=1) ; 
#grid::grid.newpage() ; 

tiff('C:/Users/ashwinv/Desktop/a.tiff', units="in", width=8, height=6, res=1000)
grid_arrange_shared_legend(ggp1,ggpw1)
dev.off()




## control and insecticide

ggpw = ggplot(temp2[temp2$treatment.fungicide != 1,], aes(x = census.start, y = predm, col = as.factor(plotx), fill = as.factor(plotx)))  +
  geom_point(aes(x = census.start, y = newproportion.mortality, col = as.factor(plotx)), size = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F, linewidth = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab(expression(paste(conspecific~density~"/"~m^2))) +
  ylab("per-capita mortality")


ggpw1 = ggpw + 
  ggtitle("Syzygium rubicundum") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,200)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  scale_color_manual(values = c("#56B4E9","#009E73","#D55E00","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#009E73","#D55E00","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))

ggp = ggplot(temp2[temp2$treatment.fungicide != 1,], aes(x = census.start, y = predm, col = as.factor(plotx), fill = as.factor(plotx)))  +
  geom_point(aes(x = census.start, y = newproportion.mortality, col = as.factor(plotx)), size = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F, linewidth = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab(expression(paste(conspecific~density~"/"~m^2))) +
  ylab("per-capita mortality")


ggp1 = ggp + 
  ggtitle("Syzygium rubicundum") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,200)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  scale_color_manual(values = c("#56B4E9","#009E73","#D55E00","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#009E73","#D55E00","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))

grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, c(lapply(plots[2], function(x)
      x + theme(legend.position="none")), list(nrow = 1))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

p = grid_arrange_shared_legend(ggp1,ggpw1)

#, bottom=grid::textGrob("conspecific density (per square-meter)") top=grid::textGrob("seedling mortality from germination to establishment",
#g = gridExtra::arrangeGrob(p,ncol=1) ; 
#grid::grid.newpage() ; 

tiff('C:/Users/ashwinv/Desktop/a.tiff', units="in", width=8, height=6, res=1000)
grid_arrange_shared_legend(ggp1,ggpw1)
dev.off()



########## Spatholobus #########



## control and fungicide and insecticide

ggpw = ggplot(temp3[temp3$plotx != "7",], aes(x = census.start, y = predm, col = as.factor(plotx), fill = as.factor(plotx)))  +
  geom_point(aes(x = census.start, y = newproportion.mortality, col = as.factor(plotx)), size = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F, linewidth = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab(expression(paste(conspecific~density~"/"~m^2))) +
  ylab("per-capita mortality")


ggpw1 = ggpw + 
  ggtitle("Spatholobus purpureus") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,100)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))

ggp = ggplot(temp3[temp3$plotx != "7",], aes(x = census.start, y = predm, col = as.factor(plotx), fill = as.factor(plotx)))  +
  geom_point(aes(x = census.start, y = newproportion.mortality, col = as.factor(plotx)), size = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F, linewidth = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab(expression(paste(conspecific~density~"/"~m^2))) +
  ylab("per-capita mortality")


ggp1 = ggp + 
  ggtitle("Spatholobus purpureus") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,100)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))

grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, c(lapply(plots[2], function(x)
      x + theme(legend.position="none")), list(nrow = 1))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

p = grid_arrange_shared_legend(ggp1,ggpw1)

#, bottom=grid::textGrob("conspecific density (per square-meter)") top=grid::textGrob("seedling mortality from germination to establishment",
#g = gridExtra::arrangeGrob(p,ncol=1) ; 
#grid::grid.newpage() ; 

tiff('C:/Users/ashwinv/Desktop/a.tiff', units="in", width=8, height=6, res=1000)
grid_arrange_shared_legend(ggp1,ggpw1)
dev.off()
