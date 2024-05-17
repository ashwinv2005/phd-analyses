library(ggplot2)
library(ggthemes)
library(tidyverse)
library(vegan)
library(grid)
library(geosphere)
library(mgcv)
library(lme4)
library(lmerTest)
library(Hmisc)

theme_set(theme_tufte())

range(coordalt$alt)

## all fragments ##

temp = dissim
temp$plot57 = as.factor(temp$plot57)
temp$plot67 = as.factor(temp$plot67)
temp$sizem = scale(temp$size, center = F)
temp$distm = scale(temp$dist, center = F)
temp$locationfac1 = paste(as.character(temp$site1),as.character(temp$location1), sep = "")
temp$locationfac2 = paste(as.character(temp$site2),as.character(temp$location2), sep = "")
temp$locationfac1 = as.factor(temp$locationfac1)
temp$locationfac2 = as.factor(temp$locationfac2)
temp$com1 = as.factor(temp$com1)
temp$com2 = as.factor(temp$com2)
temp$smsize = temp$size1
temp[temp$smsize > temp$size2,]$smsize = temp[temp$smsize > temp$size2,]$size2
temp$smsizem = scale(temp$smsize, center = F)
temp$slsize = temp$size1
temp[temp$slsize < temp$size2,]$slsize = temp[temp$slsize < temp$size2,]$size2
temp$slsizem = scale(temp$slsize, center = F)
temp$bray = -(temp$bray)
temp$brayw = -(temp$brayw)
temp$msize = (temp$size1 + temp$size2)/2
temp$msizem = scale(temp$msize, center = F)

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

#fit9c = lmer (bray ~ bray1 + (plot57 + plot67)*distm + (plot57 + plot67)*smsizem + (plot57 + plot67)*(smsizem:sizem) + 
#                (1|com1/plot1) + (1|com2/plot2), 
#              data = temp, contrasts = list(plot57 = cMat, plot67 = cMat))

#fit9c = lmer (brayw ~ bray1w + (plot57 + plot67)*distm + (plot57 + plot67)*smsizem + (plot57 + plot67)*(smsizem:sizem) + 
#                (1|com1/plot1) + (1|com2/plot2), 
#              data = temp, contrasts = list(plot57 = cMat, plot67 = cMat))



#summary(fit9c)

#mod = summary(fit9c)

#write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")



### plotting - dist ###


fit9c = lmer (bray ~ bray1 + (plot57 + plot67)*distm + (plot57 + plot67)*msizem + (plot57 + plot67)*(msizem:sizem) + 
                (1|com1/plot1) + (1|com2/plot2), 
              data = temp, contrasts = list(plot57 = cMat, plot67 = cMat))

library(mgcv)
library(ggplot2)

temp$pred = NA
temp[!is.na(temp$bray),]$pred = predict(fit9c, re.form = NA, allow.new.levels=TRUE)

a = predict(fit9c, re.form = NA)
b = predict(fit9c)
c = b - a
temp$newbray = NA
temp[!is.na(temp$bray),]$newbray = temp[!is.na(temp$bray),]$bray - c


temp1 = temp[!is.na(temp$bray),]

temp1$predm = 0
temp1$sel = 0
temp1$ser = 0


ltemp = temp1

predFun = function(fit) {
  predict(fit,ltemp, re.form = NA, allow.new.levels=TRUE)
}

bb = bootMer(fit9c,nsim=100,FUN=predFun)

for (i in 1:length(temp1$bray))
{
  temp1$predm[i] = median(bb$t[,i])
  temp1$sel[i] = median(bb$t[,i]) - sd(bb$t[,i])
  temp1$ser[i] = median(bb$t[,i]) + sd(bb$t[,i])
}

temp1$plotx = temp1$plot
temp1[temp1$plotx == "2",]$plotx = "1"

ggp = ggplot(temp1, aes(x = dist, y = predm, col = as.factor(plotx)))  +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = F) +
  geom_smooth(aes(x = dist, y = sel, col = as.factor(plotx)), method = "lm", se = F) +
  
  geom_smooth(aes(x = dist, y = ser, col = as.factor(plotx)), method = "lm", se = F)

temp1$plot12 = 0
temp1[temp1$plot == "1" | temp1$plot == "2",]$plot12 = 1

##### 1

selribx = ggplot_build(ggp)$data[[3]]$x[1:80]
selriby = ggplot_build(ggp)$data[[3]]$y[1:80]

serribx = ggplot_build(ggp)$data[[4]]$x[1:80]
serriby = ggplot_build(ggp)$data[[4]]$y[1:80]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot12 == 1,]$bray),replace = T),]
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

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "5",]$bray),replace = T),]
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

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "6",]$bray),replace = T),]
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

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "7",]$bray),replace = T),]
temp1$selribx[temp1$plot == "7"] = srib$selribx
temp1$selriby[temp1$plot == "7"] = srib$selriby
temp1$serriby[temp1$plot == "7"] = srib$serriby

l = min(temp1$selriby)
r = max(temp1$serriby)



temp1$gp = as.numeric(cut(temp1$dist,6))
tx = summarySE(temp1, groupvar = c("gp"), measurevar = "dist")
t1 = summarySE(temp1, groupvar = c("gp","plotx"), measurevar = "newbray")
t1$distb = 0
for (i in unique(temp1$gp))
{
  t1[t1$gp == i,]$distb = tx[tx$gp == i,]$dist[1]
}
pd1 = position_dodge(300)

ggp = ggplot(temp1, aes(x = dist,  col = as.factor(plotx), 
                        fill = as.factor(plotx)))  +
  geom_point(data = t1, aes(x = distb, y = (newbray)), 
             size = 1, position=pd1) +
  geom_errorbar(data = t1, aes(x = distb, ymin = newbray - se,ymax = newbray + se), size = 0.2, 
                width = 200, position = pd1) +
  geom_smooth(aes(x = dist, y = predm), method = "lm", se = F, size = 0.5) +
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
  scale_y_continuous(limits = c(0,0.2)) +
  #scale_y_continuous(limits = c(-0.09,0.28)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined")) 




### plotting - size ###


fit9c = lmer (bray ~ bray1 + (plot57 + plot67)*distm + (plot57 + plot67)*msizem + (plot57 + plot67)*(msizem:sizem) + 
                (1|com1/plot1) + (1|com2/plot2), 
              data = temp, contrasts = list(plot57 = cMat, plot67 = cMat))

library(mgcv)
library(ggplot2)

temp$pred = NA
temp[!is.na(temp$bray),]$pred = predict(fit9c, re.form = NA, allow.new.levels=TRUE)

a = predict(fit9c, re.form = NA)
b = predict(fit9c)
c = b - a
temp$newbray = NA
temp[!is.na(temp$bray),]$newbray = temp[!is.na(temp$bray),]$bray - c


temp2 = temp[!is.na(temp$bray),]

temp2$predm = 0
temp2$sel = 0
temp2$ser = 0


ltemp = temp2

predFun = function(fit) {
  predict(fit,ltemp, re.form = NA, allow.new.levels=TRUE)
}

bb = bootMer(fit9c,nsim=100,FUN=predFun)

for (i in 1:length(temp2$bray))
{
  temp2$predm[i] = median(bb$t[,i])
  temp2$sel[i] = median(bb$t[,i]) - sd(bb$t[,i])
  temp2$ser[i] = median(bb$t[,i]) + sd(bb$t[,i])
}

temp2$plotx = temp2$plot
temp2[temp2$plotx == "2",]$plotx = "1"

ggp = ggplot(temp2, aes(x = msize, y = predm, col = as.factor(plotx)))  +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = F) +
  geom_smooth(aes(x = msize, y = sel, col = as.factor(plotx)), method = "lm", se = F) +
  
  geom_smooth(aes(x = msize, y = ser, col = as.factor(plotx)), method = "lm", se = F)

temp2$plot12 = 0
temp2[temp2$plot == "1" | temp2$plot == "2",]$plot12 = 1

##### 1

selribx = ggplot_build(ggp)$data[[3]]$x[1:80]
selriby = ggplot_build(ggp)$data[[3]]$y[1:80]

serribx = ggplot_build(ggp)$data[[4]]$x[1:80]
serriby = ggplot_build(ggp)$data[[4]]$y[1:80]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp2[temp2$plot12 == 1,]$bray),replace = T),]
temp2$selribx[temp2$plot12 == 1] = srib$selribx
temp2$selriby[temp2$plot12 == 1] = srib$selriby
temp2$serriby[temp2$plot12 == 1] = srib$serriby

##### 2

selribx = ggplot_build(ggp)$data[[3]]$x[81:160]
selriby = ggplot_build(ggp)$data[[3]]$y[81:160]

serribx = ggplot_build(ggp)$data[[4]]$x[81:160]
serriby = ggplot_build(ggp)$data[[4]]$y[81:160]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp2[temp2$plot == "5",]$bray),replace = T),]
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

srib = srib[sample(nrow(srib),length(temp2[temp2$plot == "6",]$bray),replace = T),]
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

srib = srib[sample(nrow(srib),length(temp2[temp2$plot == "7",]$bray),replace = T),]
temp2$selribx[temp2$plot == "7"] = srib$selribx
temp2$selriby[temp2$plot == "7"] = srib$selriby
temp2$serriby[temp2$plot == "7"] = srib$serriby

l = min(temp2$selriby)
r = max(temp2$serriby)

#t2 = summarySE(temp2, groupvar = c("msize","plotx"), measurevar = "newbray")

temp2$gp = as.numeric(cut(temp2$msize,6))
tx = summarySE(temp2, groupvar = c("gp"), measurevar = "msize")
t2 = summarySE(temp2, groupvar = c("gp","plotx"), measurevar = "newbray")
t2$msizeb = 0
for (i in unique(temp2$gp))
{
  t2[t2$gp == i,]$msizeb = tx[tx$gp == i,]$msize[1]
}

pd2 = position_dodge(10)

ggp = ggplot(temp2, aes(x = msize,  col = as.factor(plotx), 
                        fill = as.factor(plotx)))  +
  geom_point(data = t2, aes(x = msizeb, y = (newbray)), 
             size = 1, position=pd2) +
  geom_errorbar(data = t2, aes(x = msizeb, ymin = newbray - se,ymax = newbray + se), size = 0.2, 
                width = 8, position = pd2) +
  geom_smooth(aes(x = msize, y = predm), method = "lm", se = F, size = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab("") +
  ylab("") 
ggp2 = ggp + 
  #ggtitle("all species") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_y_continuous(limits = c(0,0.2)) +
  #scale_y_continuous(limits = c(-0.09,0.28)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined")) 




############## Without those two species - dist #################

fit9c = lmer (brayw ~ bray1w + (plot57 + plot67)*distm + (plot57 + plot67)*msizem + (plot57 + plot67)*(msizem:sizem) + 
                (1|com1/plot1) + (1|com2/plot2), 
              data = temp, contrasts = list(plot57 = cMat, plot67 = cMat))

library(mgcv)
library(ggplot2)

temp$pred = NA
temp[!is.na(temp$brayw),]$pred = predict(fit9c, re.form = NA, allow.new.levels=TRUE)

a = predict(fit9c, re.form = NA)
b = predict(fit9c)
c = b - a
temp$newbrayw = NA
temp[!is.na(temp$brayw),]$newbrayw = temp[!is.na(temp$brayw),]$brayw - c


temp3 = temp[!is.na(temp$brayw),]

temp3$predm = 0
temp3$sel = 0
temp3$ser = 0


ltemp = temp3

predFun = function(fit) {
  predict(fit,ltemp, re.form = NA, allow.new.levels=TRUE)
}

bb = bootMer(fit9c,nsim=100,FUN=predFun)

for (i in 1:length(temp3$brayw))
{
  temp3$predm[i] = median(bb$t[,i])
  temp3$sel[i] = median(bb$t[,i]) - sd(bb$t[,i])
  temp3$ser[i] = median(bb$t[,i]) + sd(bb$t[,i])
}

temp3$plotx = temp3$plot
temp3[temp3$plotx == "2",]$plotx = "1"

ggp = ggplot(temp3, aes(x = dist, y = predm, col = as.factor(plotx)))  +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = F) +
  geom_smooth(aes(x = dist, y = sel, col = as.factor(plotx)), method = "lm", se = F) +
  
  geom_smooth(aes(x = dist, y = ser, col = as.factor(plotx)), method = "lm", se = F)

temp3$plot12 = 0
temp3[temp3$plot == "1" | temp3$plot == "2",]$plot12 = 1

##### 1

selribx = ggplot_build(ggp)$data[[3]]$x[1:80]
selriby = ggplot_build(ggp)$data[[3]]$y[1:80]

serribx = ggplot_build(ggp)$data[[4]]$x[1:80]
serriby = ggplot_build(ggp)$data[[4]]$y[1:80]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp3[temp3$plot12 == 1,]$brayw),replace = T),]
temp3$selribx[temp3$plot12 == 1] = srib$selribx
temp3$selriby[temp3$plot12 == 1] = srib$selriby
temp3$serriby[temp3$plot12 == 1] = srib$serriby

##### 2

selribx = ggplot_build(ggp)$data[[3]]$x[81:160]
selriby = ggplot_build(ggp)$data[[3]]$y[81:160]

serribx = ggplot_build(ggp)$data[[4]]$x[81:160]
serriby = ggplot_build(ggp)$data[[4]]$y[81:160]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp3[temp3$plot == "5",]$brayw),replace = T),]
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

srib = srib[sample(nrow(srib),length(temp3[temp3$plot == "6",]$brayw),replace = T),]
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

srib = srib[sample(nrow(srib),length(temp3[temp3$plot == "7",]$brayw),replace = T),]
temp3$selribx[temp3$plot == "7"] = srib$selribx
temp3$selriby[temp3$plot == "7"] = srib$selriby
temp3$serriby[temp3$plot == "7"] = srib$serriby

l = min(temp3$selriby)
r = max(temp3$serriby)

temp3$gp = as.numeric(cut(temp3$dist,6))
tx = summarySE(temp3, groupvar = c("gp"), measurevar = "dist")
t3 = summarySE(temp3, groupvar = c("gp","plotx"), measurevar = "newbrayw")
t3$distb = 0
for (i in unique(temp3$gp))
{
  t3[t3$gp == i,]$distb = tx[tx$gp == i,]$dist[1]
}
pd3 = position_dodge(300)

ggp = ggplot(temp3, aes(x = dist,  col = as.factor(plotx), 
                        fill = as.factor(plotx)))  +
  geom_point(data = t3, aes(x = distb, y = (newbrayw)), 
             size = 1, position=pd3) +
  geom_errorbar(data = t3, aes(x = distb, ymin = newbrayw - se,ymax = newbrayw + se), size = 0.2, 
                width = 200, position = pd3) +
  geom_smooth(aes(x = dist, y = predm), method = "lm", se = F, size = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab("pairwise distance between plots (m)") +
  ylab("")
ggp3 = ggp + 
  #ggtitle(expression(atop(paste(without~italic("Sy. rubicundum")),paste(and~italic("Sp. purpureus"))))) +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_y_continuous(limits = c(-0.18,0.26)) +
  #scale_y_continuous(limits = c(-0.18,0.38)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined")) 





############## Without those two species - size #################


fit9c = lmer (brayw ~ bray1w + (plot57 + plot67)*distm + (plot57 + plot67)*msizem + (plot57 + plot67)*(msizem:sizem) + 
                (1|com1/plot1) + (1|com2/plot2), 
              data = temp, contrasts = list(plot57 = cMat, plot67 = cMat))

library(mgcv)
library(ggplot2)

temp$pred = NA
temp[!is.na(temp$brayw),]$pred = predict(fit9c, re.form = NA, allow.new.levels=TRUE)

a = predict(fit9c, re.form = NA)
b = predict(fit9c)
c = b - a
temp$newbrayw = NA
temp[!is.na(temp$brayw),]$newbrayw = temp[!is.na(temp$brayw),]$brayw - c


temp4 = temp[!is.na(temp$brayw),]

temp4$predm = 0
temp4$sel = 0
temp4$ser = 0


ltemp = temp4

predFun = function(fit) {
  predict(fit,ltemp, re.form = NA, allow.new.levels=TRUE)
}

bb = bootMer(fit9c,nsim=100,FUN=predFun)

for (i in 1:length(temp4$brayw))
{
  temp4$predm[i] = median(bb$t[,i])
  temp4$sel[i] = median(bb$t[,i]) - sd(bb$t[,i])
  temp4$ser[i] = median(bb$t[,i]) + sd(bb$t[,i])
}

temp4$plotx = temp4$plot
temp4[temp4$plotx == "2",]$plotx = "1"

ggp = ggplot(temp4, aes(x = msize, y = predm, col = as.factor(plotx)))  +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = F) +
  geom_smooth(aes(x = msize, y = sel, col = as.factor(plotx)), method = "lm", se = F) +
  
  geom_smooth(aes(x = msize, y = ser, col = as.factor(plotx)), method = "lm", se = F)

temp4$plot12 = 0
temp4[temp4$plot == "1" | temp4$plot == "2",]$plot12 = 1

##### 1

selribx = ggplot_build(ggp)$data[[3]]$x[1:80]
selriby = ggplot_build(ggp)$data[[3]]$y[1:80]

serribx = ggplot_build(ggp)$data[[4]]$x[1:80]
serriby = ggplot_build(ggp)$data[[4]]$y[1:80]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp4[temp4$plot12 == 1,]$brayw),replace = T),]
temp4$selribx[temp4$plot12 == 1] = srib$selribx
temp4$selriby[temp4$plot12 == 1] = srib$selriby
temp4$serriby[temp4$plot12 == 1] = srib$serriby

##### 2

selribx = ggplot_build(ggp)$data[[3]]$x[81:160]
selriby = ggplot_build(ggp)$data[[3]]$y[81:160]

serribx = ggplot_build(ggp)$data[[4]]$x[81:160]
serriby = ggplot_build(ggp)$data[[4]]$y[81:160]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp4[temp4$plot == "5",]$brayw),replace = T),]
temp4$selribx[temp4$plot == "5"] = srib$selribx
temp4$selriby[temp4$plot == "5"] = srib$selriby
temp4$serriby[temp4$plot == "5"] = srib$serriby

##### 3

selribx = ggplot_build(ggp)$data[[3]]$x[161:240]
selriby = ggplot_build(ggp)$data[[3]]$y[161:240]

serribx = ggplot_build(ggp)$data[[4]]$x[161:240]
serriby = ggplot_build(ggp)$data[[4]]$y[161:240]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp4[temp4$plot == "6",]$brayw),replace = T),]
temp4$selribx[temp4$plot == "6"] = srib$selribx
temp4$selriby[temp4$plot == "6"] = srib$selriby
temp4$serriby[temp4$plot == "6"] = srib$serriby

##### 4

selribx = ggplot_build(ggp)$data[[3]]$x[241:320]
selriby = ggplot_build(ggp)$data[[3]]$y[241:320]

serribx = ggplot_build(ggp)$data[[4]]$x[241:320]
serriby = ggplot_build(ggp)$data[[4]]$y[241:320]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp4[temp4$plot == "7",]$brayw),replace = T),]
temp4$selribx[temp4$plot == "7"] = srib$selribx
temp4$selriby[temp4$plot == "7"] = srib$selriby
temp4$serriby[temp4$plot == "7"] = srib$serriby

l = min(temp4$selriby)
r = max(temp4$serriby)

#t4 = summarySE(temp4, groupvar = c("msize","plotx"), measurevar = "newbrayw")

temp4$gp = as.numeric(cut(temp4$msize,6))
tx = summarySE(temp4, groupvar = c("gp"), measurevar = "msize")
t4 = summarySE(temp4, groupvar = c("gp","plotx"), measurevar = "newbrayw")
t4$msizeb = 0
for (i in unique(temp4$gp))
{
  t4[t4$gp == i,]$msizeb = tx[tx$gp == i,]$msize[1]
}

pd4 = position_dodge(10)

ggp = ggplot(temp4, aes(x = msize,  col = as.factor(plotx), 
                        fill = as.factor(plotx)))  +
  geom_point(data = t4, aes(x = msizeb, y = (newbrayw)), 
             size = 1, position=pd4) +
  geom_errorbar(data = t4, aes(x = msizeb, ymin = newbrayw - se,ymax = newbrayw + se), size = 0.2, 
                width = 8, position = pd4) +
  geom_smooth(aes(x = msize, y = predm), method = "lm", se = F, size = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab("mean fragment size (ha)") +
  ylab("") 
ggp4 = ggp + 
  #ggtitle(expression(atop(paste(without~italic("Sy. rubicundum")),paste(and~italic("Sp. purpureus"))))) +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9), 
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  scale_size(guide = 'none') +
  scale_y_continuous(limits = c(-0.18,0.26)) +
  #scale_y_continuous(limits = c(-0.18,0.38)) +
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
    heights = unit.c(unit(0.93, "npc") - lheight, lheight))
}

grid_arrange_shared_legend2 <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(top = textGrob(expression(paste("without ", italic("Sy. rubicundum"), " and ", italic("Sp. purpureus"))), rot=0, vjust = 0,
                              gp = gpar(fontfamily = "serif", fontsize = 10, col = 'black')),
    do.call(arrangeGrob, c(lapply(plots, function(x)
      x + theme(legend.position="none")), list(nrow = 1))),
    legend,
    ncol = 1,
    heights = unit.c(unit(0.93, "npc") - lheight, lheight))
}


p1 = grid_arrange_shared_legend1(ggp1, ggp2)
p2 = grid_arrange_shared_legend2(ggp3, ggp4)

#, bottom=grid::textGrob("conspecific density (per square-meter)") top=grid::textGrob("seedling mortality from germination to establishment",
g = gridExtra::arrangeGrob(p1,p2,nrow = 2,ncol=1, left=grid::textGrob("change in dissimilarity - germination to establishment", rot=90, 
                                               gp = gpar(fontfamily = "serif", fontsize = 12, col = 'black'))); 
#grid::grid.newpage() ; 

tiff('C:/Users/admin/Desktop/a.tiff', units="in", width=5, height=7, res=1000)
grid::grid.draw(g)
dev.off()









############## presentation control ##############

library(gridExtra)
library(grid)
t4$m1sizeb = t4$msizeb
t4[t4$plotx == "1",]$m1sizeb = t4[t4$plotx == "1",]$msizeb - 6
t4[t4$plotx == "5",]$m1sizeb = t4[t4$plotx == "5",]$msizeb - 2
t4[t4$plotx == "6",]$m1sizeb = t4[t4$plotx == "6",]$msizeb + 2
t4[t4$plotx == "7",]$m1sizeb = t4[t4$plotx == "7",]$msizeb + 6

ggpw = ggplot(temp4[temp4$plotx == "1",], aes(x = msize,  col = as.factor(plotx), 
                         fill = as.factor(plotx)))  +
  geom_point(data = t4[t4$plotx == "1",], aes(x = m1sizeb, y = (newbrayw)), 
             size = 1) +
  geom_errorbar(data = t4[t4$plotx == "1",], aes(x = m1sizeb, ymin = newbrayw - se,ymax = newbrayw + se), size = 0.2, 
                width = 1) +
  geom_smooth(aes(x = msize, y = predm), method = "lm", se = F, size = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab("mean fragment size (ha)") +
  ylab("change in beta diversity") 


ggpw1 = ggpw + 
  ggtitle(expression(paste(without~italic("Sy. rubicundum")~and~italic("Sp. purpureus")))) +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,150)) +
  scale_y_continuous(limits = c(-0.18,0.26)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))


ggp = ggplot(temp4, aes(x = msize,  col = as.factor(plotx), 
                                              fill = as.factor(plotx)))  +
  geom_point(data = t4, aes(x = m1sizeb, y = (newbrayw)), 
             size = 1) +
  geom_errorbar(data = t4, aes(x = m1sizeb, ymin = newbrayw - se,ymax = newbrayw + se), size = 0.2, 
                width = 1) +
  geom_smooth(aes(x = msize, y = predm), method = "lm", se = F, size = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab("mean fragment size (ha)") +
  ylab("change in beta diversity") 


ggp1 = ggp + 
  ggtitle(expression(paste(without~italic("Sy. rubicundum")~and~italic("Sp. purpureus")))) +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,150)) +
  scale_y_continuous(limits = c(-0.18,0.26)) +
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




############## presentation control insecticide ##############

library(gridExtra)
library(grid)

ggpw = ggplot(temp4[temp4$plotx == "1" | temp4$plotx == "6",], aes(x = msize,  col = as.factor(plotx), 
                                              fill = as.factor(plotx)))  +
  geom_point(data = t4[t4$plotx == "1" | t4$plotx == "6",], aes(x = m1sizeb, y = (newbrayw)), 
             size = 1) +
  geom_errorbar(data = t4[t4$plotx == "1" | t4$plotx == "6",], aes(x = m1sizeb, ymin = newbrayw - se,ymax = newbrayw + se), size = 0.2, 
                width = 1) +
  geom_smooth(aes(x = msize, y = predm), method = "lm", se = F, size = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab("mean fragment size (ha)") +
  ylab("change in beta diversity") 


ggpw1 = ggpw + 
  ggtitle(expression(paste(without~italic("Sy. rubicundum")~and~italic("Sp. purpureus")))) +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,150)) +
  scale_y_continuous(limits = c(-0.18,0.26)) +
  scale_color_manual(values = c("#56B4E9","#009E73","#D55E00","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#009E73","#D55E00","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))


ggp = ggplot(temp4, aes(x = msize,  col = as.factor(plotx), 
                        fill = as.factor(plotx)))  +
  geom_point(data = t4, aes(x = m1sizeb, y = (newbrayw)), 
             size = 1) +
  geom_errorbar(data = t4, aes(x = m1sizeb, ymin = newbrayw - se,ymax = newbrayw + se), size = 0.2, 
                width = 1) +
  geom_smooth(aes(x = msize, y = predm), method = "lm", se = F, size = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab("mean fragment size (ha)") +
  ylab("change in beta diversity") 


ggp1 = ggp + 
  ggtitle(expression(paste(without~italic("Sy. rubicundum")~and~italic("Sp. purpureus")))) +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,150)) +
  scale_y_continuous(limits = c(-0.18,0.26)) +
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




############## presentation all ##############

library(gridExtra)
library(grid)

ggpw = ggplot(temp4, aes(x = msize,  col = as.factor(plotx), 
                                                                   fill = as.factor(plotx)))  +
  geom_point(data = t4, aes(x = m1sizeb, y = (newbrayw)), 
             size = 1) +
  geom_errorbar(data = t4, aes(x = m1sizeb, ymin = newbrayw - se,ymax = newbrayw + se), size = 0.2, 
                width = 1) +
  geom_smooth(aes(x = msize, y = predm), method = "lm", se = F, size = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab("mean fragment size (ha)") +
  ylab("change in beta diversity") 


ggpw1 = ggpw + 
  ggtitle(expression(paste(without~italic("Sy. rubicundum")~and~italic("Sp. purpureus")))) +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,150)) +
  scale_y_continuous(limits = c(-0.18,0.26)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))


ggp = ggplot(temp4, aes(x = msize,  col = as.factor(plotx), 
                        fill = as.factor(plotx)))  +
  geom_point(data = t4, aes(x = m1sizeb, y = (newbrayw)), 
             size = 1) +
  geom_errorbar(data = t4, aes(x = m1sizeb, ymin = newbrayw - se,ymax = newbrayw + se), size = 0.2, 
                width = 1) +
  geom_smooth(aes(x = msize, y = predm), method = "lm", se = F, size = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab("mean fragment size (ha)") +
  ylab("change in beta diversity") 


ggp1 = ggp + 
  ggtitle(expression(paste(without~italic("Sy. rubicundum")~and~italic("Sp. purpureus")))) +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,150)) +
  scale_y_continuous(limits = c(-0.18,0.26)) +
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





############## presentation all species ##############

library(gridExtra)
library(grid)
t2$m1sizeb = t2$msizeb
t2[t2$plotx == "1",]$m1sizeb = t2[t2$plotx == "1",]$msizeb - 6
t2[t2$plotx == "5",]$m1sizeb = t2[t2$plotx == "5",]$msizeb - 2
t2[t2$plotx == "6",]$m1sizeb = t2[t2$plotx == "6",]$msizeb + 2
t2[t2$plotx == "7",]$m1sizeb = t2[t2$plotx == "7",]$msizeb + 6

ggpw = ggplot(temp2, aes(x = msize,  col = as.factor(plotx), 
                         fill = as.factor(plotx)))  +
  geom_point(data = t2, aes(x = m1sizeb, y = (newbray)), 
             size = 1) +
  geom_errorbar(data = t2, aes(x = m1sizeb, ymin = newbray - se,ymax = newbray + se), size = 0.2, 
                width = 1) +
  geom_smooth(aes(x = msize, y = predm), method = "lm", se = F, size = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab("mean fragment size (ha)") +
  ylab("change in beta diversity") 


ggpw1 = ggpw + 
  ggtitle("all species") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,150)) +
  scale_y_continuous(limits = c(-0.18,0.26)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))


ggp = ggplot(temp2, aes(x = msize,  col = as.factor(plotx), 
                        fill = as.factor(plotx)))  +
  geom_point(data = t2, aes(x = m1sizeb, y = (newbray)), 
             size = 1) +
  geom_errorbar(data = t2, aes(x = m1sizeb, ymin = newbray - se,ymax = newbray + se), size = 0.2, 
                width = 1) +
  geom_smooth(aes(x = msize, y = predm), method = "lm", se = F, size = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab("mean fragment size (ha)") +
  ylab("change in beta diversity") 


ggp1 = ggp + 
  ggtitle("all species") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(1,150)) +
  scale_y_continuous(limits = c(-0.18,0.26)) +
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
