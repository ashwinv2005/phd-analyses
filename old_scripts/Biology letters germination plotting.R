library(survival)
library(coxme)
library(ggplot2)
library(ggthemes)
library(tidyverse)

theme_set(theme_tufte())

##Germination
##Check by species

germcalc =  function(dt, c, t)
{
  res = summary(survfit(Surv(dt, c, type = "right")~1),1:t)
  idx =  length(res$surv)
  res =  cbind(surv=1-res$surv, se=2*res$std.err, 
               lcl48=1-res$surv-2*res$std.err,
               ucl48=1-res$surv+2*res$std.err)[idx,]
  return(res)
}

temp = greentime[greentime$alignment == "Y" & greentime$species != "HC" & greentime$species != "DL",]

theme_set(theme_tufte())
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)

sdldat.wts = subset(temp, !is.na(weightm))
sdldat.wts$species = factor(sdldat.wts$species)
sdldat.wts$fragment = factor(sdldat.wts$fragment)

fit.od = coxme(Surv(survgerm, initial, type = "right") ~ treatment + (1|fragment), data = temp[temp$species == "OD",])

summary(fit.od)

fit.sc = coxme(Surv(survgerm, initial, type = "right") ~ treatment + weightm + (1|fragment), data = temp[temp$species == "SC",])

summary(fit.sc)

fit.sg = coxme(Surv(survgerm, initial, type = "right") ~ treatment + weightm + (1|fragment), data = temp[temp$species == "SG",])

summary(fit.sg)

fit.sr = coxme(Surv(survgerm, initial, type = "right") ~ (treatment + weightm + sizem)^2 + (1|fragment), data = temp[temp$species == "SR",])

summary(fit.sr)

fit.symp = coxme(Surv(survgerm, initial, type = "right") ~ (treatment + weightm + sizem)^2 + (1|fragment), data = temp[temp$species == "Symp",])

summary(fit.symp)

fit.tc = coxme(Surv(survgerm, initial, type = "right") ~ treatment*sizem + (1|fragment), data = temp[temp$species == "TC",])

summary(fit.tc)



timepoint = 110

temp1 = sdldat.wts[sdldat.wts$species == "SR",]

newdat1 = expand.grid(sizem=median(temp1$sizem),
                      weightm=median(temp1$weightm),
                      treatment=c('C', 'F'))

predgerm_mean = predict(fit.sr, type="risk", newdata=newdat1, 
                        time = timepoint, level=0.48)

newdat1$pred = 1-exp(-predgerm_mean$pred)
newdat1$pred.lcl = 1-exp(-predgerm_mean$pred.lcl)
newdat1$pred.ucl = 1-exp(-predgerm_mean$pred.ucl)

finalgerm1 = group_by(temp1, species, treatment) %>%
  summarise(germality = mean(initial), n=sum(!is.na(initial)),
            estgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["surv"],
            lclgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["lcl48"],
            uclgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["ucl48"])


timepoint = 140

temp2 = sdldat.wts[sdldat.wts$species == "OD",]

newdat2 = expand.grid(sizem=0,
                      weightm=0,
                      treatment=c('C', 'F'))

predgerm_mean = predict(fit.od, type="risk", newdata=newdat2, 
                        time = timepoint, level=0.48)

newdat2$pred = 1-exp(-predgerm_mean$pred)
newdat2$pred.lcl = 1-exp(-predgerm_mean$pred.lcl)
newdat2$pred.ucl = 1-exp(-predgerm_mean$pred.ucl)

finalgerm2 = group_by(temp2, species, treatment) %>%
  summarise(germality = mean(initial), n=sum(!is.na(initial)),
            estgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["surv"],
            lclgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["lcl48"],
            uclgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["ucl48"])

timepoint = 100

temp3 = sdldat.wts[sdldat.wts$species == "Symp",]

newdat3 = expand.grid(sizem=median(temp3$sizem),
                      weightm=median(temp3$weightm),
                      treatment=c('C', 'F'))

predgerm_mean = predict(fit.symp, type="risk", newdata=newdat3, 
                        time = timepoint, level=0.48)

newdat3$pred = 1-exp(-predgerm_mean$pred)
newdat3$pred.lcl = 1-exp(-predgerm_mean$pred.lcl)
newdat3$pred.ucl = 1-exp(-predgerm_mean$pred.ucl)

finalgerm3 = group_by(temp3, species, treatment) %>%
  summarise(germality = mean(initial), n=sum(!is.na(initial)),
            estgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["surv"],
            lclgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["lcl48"],
            uclgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["ucl48"])

timepoint = 100

temp4 = sdldat.wts[sdldat.wts$species == "SC",]

newdat4 = expand.grid(sizem=0,
                      weightm=median(temp4$weightm),
                      treatment=c('C', 'F'))

predgerm_mean = predict(fit.sc, type="risk", newdata=newdat4, 
                        time = timepoint, level=0.48)

newdat4$pred = 1-exp(-predgerm_mean$pred)
newdat4$pred.lcl = 1-exp(-predgerm_mean$pred.lcl)
newdat4$pred.ucl = 1-exp(-predgerm_mean$pred.ucl)

finalgerm4 = group_by(temp4, species, treatment) %>%
  summarise(germality = mean(initial), n=sum(!is.na(initial)),
            estgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["surv"],
            lclgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["lcl48"],
            uclgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["ucl48"])


timepoint = 100

temp5 = sdldat.wts[sdldat.wts$species == "SG",]

newdat5 = expand.grid(sizem=0,
                      weightm=median(temp5$weightm),
                      treatment=c('C', 'F'))

predgerm_mean = predict(fit.sg, type="risk", newdata=newdat5, 
                        time = timepoint, level=0.48)

newdat5$pred = 1-exp(-predgerm_mean$pred)
newdat5$pred.lcl = 1-exp(-predgerm_mean$pred.lcl)
newdat5$pred.ucl = 1-exp(-predgerm_mean$pred.ucl)

finalgerm5 = group_by(temp5, species, treatment) %>%
  summarise(germality = mean(initial), n=sum(!is.na(initial)),
            estgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["surv"],
            lclgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["lcl48"],
            uclgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["ucl48"])


timepoint = 60

temp6 = temp[temp$species == "TC",]

newdat6 = expand.grid(sizem=median(temp6$sizem),
                      weightm=0,
                      treatment=c('C', 'F'))

predgerm_mean = predict(fit.tc, type="risk", newdata=newdat6, 
                        time = timepoint, level=0.48)

newdat6$pred = 1-exp(-predgerm_mean$pred)
newdat6$pred.lcl = 1-exp(-predgerm_mean$pred.lcl)
newdat6$pred.ucl = 1-exp(-predgerm_mean$pred.ucl)

finalgerm6 = group_by(temp6, species, treatment) %>%
  summarise(germality = mean(initial), n=sum(!is.na(initial)),
            estgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["surv"],
            lclgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["lcl48"],
            uclgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["ucl48"])



newdat1$species = as.factor("Syzygium rubicundum")
newdat2$species = as.factor("Olea dioica")
newdat3$species = as.factor("Symplocos racemosa")
newdat4$species = as.factor("Syzygium cumini")
newdat5$species = as.factor("Syzygium gardneri")
newdat6$species = as.factor("Toona ciliata")

newdat = rbind(newdat1,newdat2,newdat3,newdat4,newdat5,newdat6)
finalgerm = rbind(finalgerm1,finalgerm2,finalgerm3,finalgerm4,finalgerm5,finalgerm6)

finalgerm$species = as.character(finalgerm$species)
finalgerm[finalgerm$species == "SC",]$species = "Syzygium\ncumini"
finalgerm[finalgerm$species == "SR",]$species = "Syzygium\nrubicundum"
finalgerm[finalgerm$species == "OD",]$species = "Olea\ndioica"
finalgerm[finalgerm$species == "Symp",]$species = "Symplocos\nracemosa"
finalgerm[finalgerm$species == "SG",]$species = "Syzygium\ngardneri"
finalgerm[finalgerm$species == "TC",]$species = "Toona\nciliata"
finalgerm$species = as.factor(finalgerm$species)

#newdat$size = round(newdat$sizem*46.48985,1)
#finalgerm$size = round(finalgerm$sizem*46.48985,1)

newdat$treatment = as.character(newdat$treatment)
finalgerm$treatment = as.character(finalgerm$treatment)
newdat[newdat$treatment == "C",]$treatment = "control"
newdat[newdat$treatment == "F",]$treatment = "fungicide"
finalgerm[finalgerm$treatment == "C",]$treatment = "control"
finalgerm[finalgerm$treatment == "F",]$treatment = "fungicide"
newdat$treatment = as.factor(newdat$treatment)
finalgerm$treatment = as.factor(finalgerm$treatment)

newdatg = newdat

finalgerm = as.data.frame(finalgerm)
finalgerm = cbind(finalgerm,newdatg[,c(1,2,4,5,6)])
finalgerm$dummy = as.factor("x")
finalgerm$species = factor(finalgerm$species, levels = c("Syzygium\nrubicundum","Olea\ndioica","Symplocos\nracemosa","Syzygium\ncumini","Syzygium\ngardneri","Toona\nciliata"))

pd = position_dodge(0.15)
ggp = ggplot(finalgerm, aes(x=species, y=estgerm, colour=treatment)) + 
  geom_point(
    position = pd, 
    size = 2) +
  geom_errorbar(aes(ymin=lclgerm, ymax=uclgerm), size = 0.5, width = 0.1, 
                position = pd
                ) +
  xlab("tree species") +
  ylab("probability of germination")

ggp1 = ggp +
  theme(axis.title.x = element_text(vjust = -2, size = 10), axis.text.x = element_text(size = 10, face = 'italic'),
        axis.title.y = element_text(vjust = 3, angle = 90, size = 10), axis.text.y = element_text(size = 9)) +
  theme(legend.title=element_blank(), legend.text = element_text(size = 9))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
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

tiff('C:/Users/admin/Desktop/a.tiff', units="in", width=7, height=5, res=1000)
grid_arrange_shared_legend(ggp1)
dev.off()