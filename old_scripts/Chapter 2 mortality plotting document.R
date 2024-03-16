library(survival)
library(coxme)
library(ggplot2)
library(ggthemes)
library(tidyverse)

theme_set(theme_tufte())

##Mortality
##Check by species

mortcalc =  function(dt, c, t)
{
  res = summary(survfit(Surv(dt, c, type = "right")~1),1:t)
  idx =  length(res$surv)
  res =  cbind(surv=1-res$surv, se=res$std.err, 
               lcl48=1-res$surv-res$std.err,
               ucl48=1-res$surv+res$std.err)[idx,]
  return(res)
}

temp = greentime1[greentime1$initial == 1,]
temp$final1 = 1 - temp$final
temp = temp[temp$alignment == "Y" & temp$mechanism != "b" & temp$species != "HC" & temp$species != "DL" & !is.na(temp$survmort),]


temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$frdenm = scale(temp$frden, center = F)
temp$sddenm = scale(temp$sdden, center = F)
temp$sldenm = scale(temp$slden, center = F)

sdldat.wts = temp
sdldat.wts$species = factor(sdldat.wts$species)
sdldat.wts$fragment = factor(sdldat.wts$fragment)

fit.od = coxme(Surv(survmort, final1, type = "right") ~ (treatment + sizem)^2 + (1|fragment), 
                data = subset(temp, species=="OD"))
summary(fit.od)

fit.sc = coxme(Surv(survmort, final1, type = "right") ~ treatment*sizem + (1|fragment), 
                data = subset(temp, species=="SC"))
summary(fit.sc)


fit.sg = coxme(Surv(survmort, final1, type = "right") ~ treatment*sizem +(1|fragment), 
                data = subset(temp, species=="SG"))
summary(fit.sg)

fit.sr = coxme(Surv(survmort, final1, type = "right") ~ (treatment + sizem)^2 +(1|fragment), 
                data = subset(temp, species=="SR"))
summary(fit.sr)

fit.symp = coxme(Surv(survmort, final1, type = "right") ~ (treatment + sizem)^2 +(1|fragment), 
                  data = subset(temp, species=="Symp"))
summary(fit.symp)

fit.tc = coxme(Surv(survmort, final1, type = "right") ~ treatment*sizem +(1|fragment), 
                data = subset(temp, species=="TC"))
summary(fit.tc)

fit.gcc = coxme(Surv(survmort, final1, type = "right") ~ (treatment + sizem)^2 +(1|fragment), 
                data = subset(temp, species=="TC" | species == "SC" | species == "SG"))

summary(fit.gcc)

##

timepoint = 90

temp1 = sdldat.wts[sdldat.wts$species == "SR",]

newdat1 = expand.grid(sizem=with(temp1, 
                                 seq(-5, 5, 0.1)),
                       #weightm=mean(sdldat.wts$weightm),
                       treatment=c('C', 'F'))

predmort_mean = predict(fit.sr, type="risk", newdata=newdat1, 
                         time = timepoint, level=0.48)

newdat1$pred = 1-exp(-predmort_mean$pred)
newdat1$pred.lcl = 1-exp(-predmort_mean$pred.lcl)
newdat1$pred.ucl = 1-exp(-predmort_mean$pred.ucl)

finalmort1 = group_by(temp1, sizem, species, treatment) %>%
  summarise(mortality = mean(final1), n=sum(!is.na(final1)),
            estmort = mortcalc(dt=survmort, c=final1, t=timepoint)["surv"],
            lclmort = mortcalc(dt=survmort, c=final1, t=timepoint)["lcl48"],
            uclmort = mortcalc(dt=survmort, c=final1, t=timepoint)["ucl48"])

timepoint = 100

temp2 = sdldat.wts[sdldat.wts$species == "OD",]

newdat2 = expand.grid(sizem=with(temp2, 
                                 seq(-5, 5, 0.1)),
                       #weightm=mean(sdldat.wts$weightm),
                       treatment=c('C', 'F'))

predmort_mean = predict(fit.od, type="risk", newdata=newdat2, 
                         time = timepoint, level=0.48)

newdat2$pred = 1-exp(-predmort_mean$pred)
newdat2$pred.lcl = 1-exp(-predmort_mean$pred.lcl)
newdat2$pred.ucl = 1-exp(-predmort_mean$pred.ucl)

finalmort2 = group_by(temp2, sizem, species, treatment) %>%
  summarise(mortality = mean(final1), n=sum(!is.na(final1)),
            estmort = mortcalc(dt=survmort, c=final1, t=timepoint)["surv"],
            lclmort = mortcalc(dt=survmort, c=final1, t=timepoint)["lcl48"],
            uclmort = mortcalc(dt=survmort, c=final1, t=timepoint)["ucl48"])

timepoint = 60

temp3 = sdldat.wts[sdldat.wts$species == "Symp",]

newdat3 = expand.grid(sizem=with(temp3, 
                                 seq(-5, 5, 0.1)),
                       #weightm=mean(sdldat.wts$weightm),
                       treatment=c('C', 'F'))

predmort_mean = predict(fit.symp, type="risk", newdata=newdat3, 
                         time = timepoint, level=0.48)

newdat3$pred = 1-exp(-predmort_mean$pred)
newdat3$pred.lcl = 1-exp(-predmort_mean$pred.lcl)
newdat3$pred.ucl = 1-exp(-predmort_mean$pred.ucl)

finalmort3 = group_by(temp3, sizem, species, treatment) %>%
  summarise(mortality = mean(final1), n=sum(!is.na(final1)),
            estmort = mortcalc(dt=survmort, c=final1, t=timepoint)["surv"],
            lclmort = mortcalc(dt=survmort, c=final1, t=timepoint)["lcl48"],
            uclmort = mortcalc(dt=survmort, c=final1, t=timepoint)["ucl48"])

timepoint = 60

temp4 = sdldat.wts[sdldat.wts$species == "SC" | sdldat.wts$species == "SG" | sdldat.wts$species == "TC" ,]

newdat4 = expand.grid(sizem=with(temp4, 
                                  seq(-5, 5, 0.1)),
                       #weightm=mean(sdldat.wts$weightm),
                       treatment=c('C', 'F'))

predmort_mean = predict(fit.gcc, type="risk", newdata=newdat4, 
                         time = timepoint, level=0.48)

newdat4$pred = 1-exp(-predmort_mean$pred)
newdat4$pred.lcl = 1-exp(-predmort_mean$pred.lcl)
newdat4$pred.ucl = 1-exp(-predmort_mean$pred.ucl)

finalmort4 = group_by(temp4, sizem, species, treatment) %>%
  summarise(mortality = mean(final1), n=sum(!is.na(final1)),
            estmort = mortcalc(dt=survmort, c=final1, t=timepoint)["surv"],
            lclmort = mortcalc(dt=survmort, c=final1, t=timepoint)["lcl48"],
            uclmort = mortcalc(dt=survmort, c=final1, t=timepoint)["ucl48"])


newdat1$species = as.factor("Syzygium rubicundum")
newdat2$species = as.factor("Olea dioica")
newdat3$species = as.factor("Symplocos racemosa")
newdat4$species = as.factor("Syz. cumini, Syz. gardneri, T. ciliata")


newdat = rbind(newdat1,newdat2,newdat3,newdat4)
finalmort = rbind(finalmort1,finalmort2,finalmort3,finalmort4)

finalmort$species = as.character(finalmort$species)
finalmort[finalmort$species == "SC" | finalmort$species == "SG" |
            finalmort$species == "TC",]$species = "Syz. cumini, Syz. gardneri, T. ciliata"
finalmort[finalmort$species == "SR",]$species = "Syzygium rubicundum"
finalmort[finalmort$species == "OD",]$species = "Olea dioica"
finalmort[finalmort$species == "Symp",]$species = "Symplocos racemosa"
finalmort$species = as.factor(finalmort$species)

newdat$size = round(newdat$sizem*46.99823,1)
finalmort$size = round(finalmort$sizem*46.99823,1)

newdat$treatment = as.character(newdat$treatment)
finalmort$treatment = as.character(finalmort$treatment)
newdat[newdat$treatment == "C",]$treatment = "control"
newdat[newdat$treatment == "F",]$treatment = "fungicide"
finalmort[finalmort$treatment == "C",]$treatment = "control"
finalmort[finalmort$treatment == "F",]$treatment = "fungicide"
newdat$treatment = as.factor(newdat$treatment)
finalmort$treatment = as.factor(finalmort$treatment)

newdatm = newdat

ggp = ggplot(newdatm, aes(x=size, y=pred, colour=treatment)) + 
  geom_line(size = 0.5) + 
  facet_wrap(~species, nrow = 2, ncol = 2) +
  geom_ribbon(aes(ymin = pred.lcl, ymax=pred.ucl, 
                  fill=treatment, colour=NULL), alpha=0.1) +
  geom_pointrange(data= finalmort, aes(y=estmort, ymin=lclmort, ymax=uclmort,alpha=n),
                  position=position_jitter(width=5, height=0), fatten = 2, size = 0.5) +
  xlab("fragment size (ha)") +
  ylab("probability of mortality")

ggp1 = ggp +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 3, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title=element_blank(), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(-5,155.1), breaks = c(0,50,100,150)) +
  theme(strip.text.x = element_text(size = 10, face = "italic")) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 1),
    alpha = guide_legend(order = 0)
  ) 

library(gridExtra)
library(grid)

grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, c(lapply(plots, function(x)
      x + theme(legend.position="none")), list(nrow = 1))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

grid_arrange_shared_legend(ggp1)

tiff('C:/Users/admin/Desktop/a.tiff', units="in", width=5, height=7, res=1000)
grid_arrange_shared_legend(ggp1)
dev.off()




######## presentation blank ############

newdatSR = newdatm[newdatm$species == "Syzygium rubicundum",]
finalmortSR = finalmort[finalmort$species == "Syzygium rubicundum",]
finalmortSR[finalmortSR$treatment == "fungicide",]$size = finalmortSR[finalmortSR$treatment == "fungicide",]$size - 1

ggpw = ggplot(newdatSR, aes(x=size, y=pred, colour=treatment)) + 
  #geom_line(size = 0.5) + 
  #geom_ribbon(aes(ymin = pred.lcl, ymax=pred.ucl, 
  #                fill=treatment, colour=NULL), alpha=0.1) +
  #geom_pointrange(data= finalmort, aes(y=estmort, ymin=lclmort, ymax=uclmort,alpha=n), fatten = 2, size = 0.5) +
  xlab("fragment size (ha)") +
  ylab("probability of mortality")

ggpw1 = ggpw +
  ggtitle("Syzygium rubicundum") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(0,150), breaks = c(0,50,100,150)) +
  scale_y_continuous(limits = c(0,0.8), breaks = c(0,0.2,0.4,0.6,0.8)) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 1),
    alpha = guide_legend(order = 0)
  ) +
  scale_color_manual(values = c("#56B4E9","#D55E00"),
                     name="",
                     breaks=c("control","fungicide"),
                     labels=c("control","fungicide")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00"),
                    name="",
                    breaks=c("control","fungicide"),
                    labels=c("control","fungicide")) 


ggp = ggplot(newdatSR, aes(x=size, y=pred, colour=treatment)) + 
  geom_line(size = 0.5) + 
  geom_ribbon(aes(ymin = pred.lcl, ymax=pred.ucl, 
                  fill=treatment, colour=NULL), alpha=0.1) +
  geom_pointrange(data= finalmortSR, aes(y=estmort, ymin=lclmort, ymax=uclmort,alpha=n), fatten = 2, size = 0.5) +
  xlab("fragment size (ha)") +
  ylab("probability of mortality")

ggp1 = ggp +
  ggtitle("Syzygium rubicundum") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(0,150), breaks = c(0,50,100,150)) +
  scale_y_continuous(limits = c(0,0.8), breaks = c(0,0.2,0.4,0.6,0.8)) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 1),
    alpha = guide_legend(order = 0)
  ) +
  scale_color_manual(values = c("#56B4E9","#D55E00"),
                     name="",
                     breaks=c("control","fungicide"),
                     labels=c("control","fungicide")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00"),
                    name="",
                    breaks=c("control","fungicide"),
                    labels=c("control","fungicide")) 

library(gridExtra)
library(grid)

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

tiff('C:/Users/ashwinv/Desktop/a.tiff', units="in", width=8, height=6, res=1000)
grid_arrange_shared_legend(ggp1,ggpw1)
dev.off()





########### presentation control ############


ggpw = ggplot(newdatSR[newdatSR$treatment == "control",], aes(x=size, y=pred, colour=treatment)) + 
  geom_line(size = 0.5) + 
  geom_ribbon(aes(ymin = pred.lcl, ymax=pred.ucl, 
                  fill=treatment, colour=NULL), alpha=0.1) +
  geom_pointrange(data= finalmortSR[finalmortSR$treatment == "control",],
                  aes(y=estmort, ymin=lclmort, ymax=uclmort,alpha=n), fatten = 2, size = 0.5) +
  xlab("fragment size (ha)") +
  ylab("probability of mortality")

ggpw1 = ggpw +
  ggtitle("Syzygium rubicundum") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(0,150), breaks = c(0,50,100,150)) +
  scale_y_continuous(limits = c(0,0.8), breaks = c(0,0.2,0.4,0.6,0.8)) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 1),
    alpha = guide_legend(order = 0)
  ) +
  scale_color_manual(values = c("#56B4E9","#D55E00"),
                     name="",
                     breaks=c("control","fungicide"),
                     labels=c("control","fungicide")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00"),
                    name="",
                    breaks=c("control","fungicide"),
                    labels=c("control","fungicide")) 


ggp = ggplot(newdatSR, aes(x=size, y=pred, colour=treatment)) + 
  geom_line(size = 0.5) + 
  geom_ribbon(aes(ymin = pred.lcl, ymax=pred.ucl, 
                  fill=treatment, colour=NULL), alpha=0.1) +
  geom_pointrange(data= finalmortSR, aes(y=estmort, ymin=lclmort, ymax=uclmort,alpha=n), fatten = 2, size = 0.5) +
  xlab("fragment size (ha)") +
  ylab("probability of mortality")

ggp1 = ggp +
  ggtitle("Syzygium rubicundum") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(0,150), breaks = c(0,50,100,150)) +
  scale_y_continuous(limits = c(0,0.8), breaks = c(0,0.2,0.4,0.6,0.8)) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 1),
    alpha = guide_legend(order = 0)
  ) +
  scale_color_manual(values = c("#56B4E9","#D55E00"),
                     name="",
                     breaks=c("control","fungicide"),
                     labels=c("control","fungicide")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00"),
                    name="",
                    breaks=c("control","fungicide"),
                    labels=c("control","fungicide")) 

library(gridExtra)
library(grid)

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

tiff('C:/Users/ashwinv/Desktop/a.tiff', units="in", width=8, height=6, res=1000)
grid_arrange_shared_legend(ggp1,ggpw1)
dev.off()






########### presentation both ############


ggpw = ggplot(newdatSR, aes(x=size, y=pred, colour=treatment)) + 
  geom_line(size = 0.5) + 
  geom_ribbon(aes(ymin = pred.lcl, ymax=pred.ucl, 
                  fill=treatment, colour=NULL), alpha=0.1) +
  geom_pointrange(data= finalmortSR, aes(y=estmort, ymin=lclmort, ymax=uclmort,alpha=n), fatten = 2, size = 0.5) +
  xlab("fragment size (ha)") +
  ylab("probability of mortality")

ggpw1 = ggpw +
  ggtitle("Syzygium rubicundum") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(0,150), breaks = c(0,50,100,150)) +
  scale_y_continuous(limits = c(0,0.8), breaks = c(0,0.2,0.4,0.6,0.8)) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 1),
    alpha = guide_legend(order = 0)
  ) +
  scale_color_manual(values = c("#56B4E9","#D55E00"),
                     name="",
                     breaks=c("control","fungicide"),
                     labels=c("control","fungicide")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00"),
                    name="",
                    breaks=c("control","fungicide"),
                    labels=c("control","fungicide")) 


ggp = ggplot(newdatSR, aes(x=size, y=pred, colour=treatment)) + 
  geom_line(size = 0.5) + 
  geom_ribbon(aes(ymin = pred.lcl, ymax=pred.ucl, 
                  fill=treatment, colour=NULL), alpha=0.1) +
  geom_pointrange(data= finalmortSR, aes(y=estmort, ymin=lclmort, ymax=uclmort,alpha=n), fatten = 2, size = 0.5) +
  xlab("fragment size (ha)") +
  ylab("probability of mortality")

ggp1 = ggp +
  ggtitle("Syzygium rubicundum") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(0,150), breaks = c(0,50,100,150)) +
  scale_y_continuous(limits = c(0,0.8), breaks = c(0,0.2,0.4,0.6,0.8)) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 1),
    alpha = guide_legend(order = 0)
  ) +
  scale_color_manual(values = c("#56B4E9","#D55E00"),
                     name="",
                     breaks=c("control","fungicide"),
                     labels=c("control","fungicide")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00"),
                    name="",
                    breaks=c("control","fungicide"),
                    labels=c("control","fungicide")) 

library(gridExtra)
library(grid)

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

tiff('C:/Users/ashwinv/Desktop/a.tiff', units="in", width=8, height=6, res=1000)
grid_arrange_shared_legend(ggp1,ggpw1)
dev.off()