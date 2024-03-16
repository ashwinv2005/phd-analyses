
temp = greentime1[greentime1$initial == 1,]
temp$final1 = 1 - temp$final
temp = temp[temp$alignment == "Y" & temp$mechanism != "b" & temp$species != "HC" & temp$species != "DL" & !is.na(temp$survmort),]


library(survival)
library(coxme)
library(ggplot2)
library(ggthemes)
library(tidyverse)


theme_set(theme_tufte())
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)

sdldat.wts = temp
#sdldat.wts <- subset(temp, !is.na(weightm)) 
sdldat.wts$species <- factor(sdldat.wts$species)
sdldat.wts$fragment <- factor(sdldat.wts$fragment)
summary(subset(sdldat.wts, species=="SG"))
# with weights
fit0 <-  coxph(Surv(survmort, final1, type = "right") ~ (sizem + treatment + weightm)^2, data = sdldat.wts)

fit1 <-  coxme(Surv(survmort, final1, type = "right") ~ (sizem + treatment + weightm)^2 + (1|species) + (1|fragment), data = sdldat.wts)
summary(fit1)

## without S rubicundum
fit1b <-  coxme(Surv(survmort, final1, type = "right") ~ (sizem + treatment + weightm)^2 + (1|species) + (1|fragment), data = sdldat.wts, subset=species != "SR")
summary(fit1b)
fit1c <-  coxme(Surv(survmort, final1, type = "right") ~ (sizem + treatment + weightm)^2 + (1|species) + (1|fragment), data = sdldat.wts, subset=!(species %in% c("SR", "SG")))

summary(fit1c)
## really seems like the trend is bing driven by S. rubicundum.

# without weights
fit2 <-  coxme(Surv(survmort, final1, type = "right") ~ treatment*sizem + (1|species) + (1|fragment), data = temp[temp$species != "SR",])
summary(fit2)

temp2 <- subset(temp, species != "SR") ## some weird issue with doing subsetting in the function
fit2b <-  coxme(Surv(survmort, final1, type = "right") ~ treatment*sizem + (1|species) + (1|fragment), data = temp2)
anova(fit2b)
summary(fit2b)

summary(fit1)
anova(fit1)


#```{r predictions}

## Time at which to calculate mortality in days
timepoint <- 60

## add the observed mortality probability to the data
## Ashwin - check that this is calculated correctly - trying to get observed 
## mortality for each treatment combination
mortcalc <-  function(dt, c, t)
{
  res <- summary(survfit(Surv(dt, c)~1),1:t)
  idx <-  length(res$surv)
  res <-  cbind(surv=1-res$surv, se=res$std.err, 
                lcl95=1-res$lower,
                ucl95=1-res$upper)[idx,]
  return(res)
}

x <- subset(sdldat.wts, size==9 & species=="SR" & treatment=="C")
mortcalc(x$survmort, x$final1, timepoint)
## calculate the mortality in each treatment at day 60
## you can change this
finalmort <- group_by(sdldat.wts, sizem, species, treatment) %>%
  summarise(mortality = mean(final1), n=sum(!is.na(final1)),
            estmort = mortcalc(dt=survmort, c=final1, t=timepoint)["surv"],
            lclmort = mortcalc(dt=survmort, c=final1, t=timepoint)["lcl95"],
            uclmort = mortcalc(dt=survmort, c=final1, t=timepoint)["ucl95"])

ggplot(data=finalmort, aes(x=mortality, y=estmort, colour=treatment)) +
  geom_point() + geom_smooth(method='lm')

newdat <- expand.grid(sizem=with(sdldat.wts, 
                                 seq(min(sizem), max(sizem), 0.1)),
                      weightm=mean(sdldat.wts$weightm),
                      treatment=c('C', 'F'))

newdat_sp <- expand.grid(sizem=with(sdldat.wts, 
                                    seq(min(sizem), max(sizem), 0.1)),
                         treatment=c('C', 'F'),
                         weightm = mean(sdldat.wts$weightm),
                         species=levels(sdldat.wts$species),
                         enddate=timepoint
)

# expt_length <- summarise(group_by(sdldat.wts, species), enddate=60, 
#                          weightm=mean(weightm)) 
# newdat_sp$enddate <- expt_length$enddate[match(newdat_sp$species, expt_length$species)]
# newdat_sp$weightm <- expt_length$weightm[match(newdat_sp$species, expt_length$species)]
# head(newdat_sp)
## Making a prediction at maximum time of experiment - need to alter if 
## different species had different times.

predmort_mean <- predict(fit1, type="risk", newdata=newdat, 
                         time = timepoint, level=0.66)

newdat$pred <- 1-exp(-predmort_mean$pred)
newdat$pred.lcl <- 1-exp(-predmort_mean$pred.lcl)
newdat$pred.ucl <- 1-exp(-predmort_mean$pred.ucl)

ggplot(newdat, aes(x=sizem, y=pred, colour=treatment)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = pred.lcl, ymax=pred.ucl, 
                  fill=treatment, colour=NULL), alpha=0.1) +
  geom_pointrange(data= finalmort, aes(y=estmort, ymin=lclmort, ymax=uclmort,alpha=n),
                  position=position_jitter(width=0.1, height=0)) 

## you might want to play around with size etc
predmort_sp <- predict(fit1, type="risk", newdata=newdat_sp, 
                       #random.intercept = "species", 
                       time = newdat_sp$enddate, level=0.66)
newdat_sp$pred <- 1-exp(-predmort_sp$pred)
newdat_sp$pred.lcl <- 1-exp(-predmort_sp$pred.lcl)
newdat_sp$pred.ucl <- 1-exp(-predmort_sp$pred.ucl)

ggplot(newdat_sp, aes(x=sizem, y=pred, colour=treatment)) + 
  geom_line() +
  geom_ribbon(aes(ymin = pred.lcl, ymax=pred.ucl, 
                  fill=treatment, colour=NULL), alpha=0.2) +
  facet_wrap(~species, scales = 'free') +
  geom_pointrange(data= finalmort, aes(y=mortality, x=sizem, ymax=uclmort, ymin=lclmort))


ggplot(data=finalmort, aes(y=mortality, x=sizem, colour=treatment)) + 
  geom_point(position=position_dodge(width=0.1)) + geom_smooth(aes(weight=n), method="gam", 
                                                               method.args=list(family="binomial")) +
  facet_wrap(~species)

##Mortality
##Check by species

mortcalc <-  function(dt, c, t)
{
  res <- summary(survfit(Surv(dt, c, type = "right")~1),1:t)
  idx <-  length(res$surv)
  res <-  cbind(surv=1-res$surv, se=res$std.err, 
                lcl95=1-res$upper,
                ucl95=1-res$lower)[idx,]
  return(res)
}

germcalc <-  function(dt, c, t)
{
  res <- summary(survfit(Surv(dt, c, type = "right")~1),1:t)
  idx <-  length(res$surv)
  res <-  cbind(surv=1-res$surv, se=res$std.err, 
                lcl95=1-res$upper,
                ucl95=1-res$lower)[idx,]
  return(res)
}

library(survival)
library(coxme)
library(ggplot2)
library(ggthemes)
library(tidyverse)

temp = greentime1[greentime1$initial == 1,]
temp$final1 = 1 - temp$final
temp = temp[temp$alignment == "Y" & temp$mechanism != "b" & temp$species != "HC" & temp$species != "DL" & !is.na(temp$survmort),]


theme_set(theme_tufte())
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)

sdldat.wts = temp
#sdldat.wts <- subset(temp, !is.na(weightm)) 
sdldat.wts$species <- factor(sdldat.wts$species)
sdldat.wts$fragment <- factor(sdldat.wts$fragment)

fit.od <- coxme(Surv(survmort, final1, type = "right") ~ (treatment + sizem)^2 + (1|fragment), 
                data = subset(temp, species=="OD"))
summary(fit.od)

fit.sc <- coxme(Surv(survmort, final1, type = "right") ~ treatment*sizem + (1|fragment), 
                data = subset(temp, species=="SC"))
summary(fit.sc)


fit.sg <- coxme(Surv(survmort, final1, type = "right") ~ treatment*sizem +(1|fragment), 
                data = subset(temp, species=="SG"))
summary(fit.sg)

fit.sr <- coxme(Surv(survmort, final1, type = "right") ~ (treatment + sizem)^2 +(1|fragment), 
                data = subset(temp, species=="SR"))
summary(fit.sr)

fit.symp <- coxme(Surv(survmort, final1, type = "right") ~ (treatment + sizem)^2 +(1|fragment), 
                  data = subset(temp, species=="Symp"))
summary(fit.symp)

fit.tc <- coxme(Surv(survmort, final1, type = "right") ~ treatment*sizem +(1|fragment), 
                  data = subset(temp, species=="TC"))
summary(fit.tc)

fit.gcc = coxme(Surv(survmort, final1, type = "right") ~ (treatment + sizem)^2 +(1|fragment), 
                data = subset(temp, species=="TC" | species == "SC" | species == "SG"))

summary(fit.gcc)

## Confirms SR is the only species driving the patterns.

timepoint = 90

temp1 = sdldat.wts[sdldat.wts$species == "SR",]

newdat1 <- expand.grid(sizem=with(temp1, 
                                 seq(min(sizem), max(sizem), 0.1)),
                      #weightm=mean(sdldat.wts$weightm),
                      treatment=c('C', 'F'))

predmort_mean <- predict(fit.sr, type="risk", newdata=newdat1, 
                         time = timepoint, level=0.66)

newdat1$pred <- 1-exp(-predmort_mean$pred)
newdat1$pred.lcl <- 1-exp(-predmort_mean$pred.lcl)
newdat1$pred.ucl <- 1-exp(-predmort_mean$pred.ucl)

finalmort1 <- group_by(temp1, sizem, species, treatment) %>%
  summarise(mortality = mean(final1), n=sum(!is.na(final1)),
            estmort = mortcalc(dt=survmort, c=final1, t=timepoint)["surv"],
            lclmort = mortcalc(dt=survmort, c=final1, t=timepoint)["lcl95"],
            uclmort = mortcalc(dt=survmort, c=final1, t=timepoint)["ucl95"])

timepoint = 100

temp2 = sdldat.wts[sdldat.wts$species == "OD",]

newdat2 <- expand.grid(sizem=with(temp2, 
                                  seq(min(sizem), max(sizem), 0.1)),
                       #weightm=mean(sdldat.wts$weightm),
                       treatment=c('C', 'F'))

predmort_mean <- predict(fit.od, type="risk", newdata=newdat2, 
                         time = timepoint, level=0.66)

newdat2$pred <- 1-exp(-predmort_mean$pred)
newdat2$pred.lcl <- 1-exp(-predmort_mean$pred.lcl)
newdat2$pred.ucl <- 1-exp(-predmort_mean$pred.ucl)

finalmort2 <- group_by(temp2, sizem, species, treatment) %>%
  summarise(mortality = mean(final1), n=sum(!is.na(final1)),
            estmort = mortcalc(dt=survmort, c=final1, t=timepoint)["surv"],
            lclmort = mortcalc(dt=survmort, c=final1, t=timepoint)["lcl95"],
            uclmort = mortcalc(dt=survmort, c=final1, t=timepoint)["ucl95"])

timepoint = 60

temp3 = sdldat.wts[sdldat.wts$species == "Symp",]

newdat3 <- expand.grid(sizem=with(temp3, 
                                  seq(min(sizem), max(sizem), 0.1)),
                       #weightm=mean(sdldat.wts$weightm),
                       treatment=c('C', 'F'))

predmort_mean <- predict(fit.symp, type="risk", newdata=newdat3, 
                         time = timepoint, level=0.66)

newdat3$pred <- 1-exp(-predmort_mean$pred)
newdat3$pred.lcl <- 1-exp(-predmort_mean$pred.lcl)
newdat3$pred.ucl <- 1-exp(-predmort_mean$pred.ucl)

finalmort3 <- group_by(temp3, sizem, species, treatment) %>%
  summarise(mortality = mean(final1), n=sum(!is.na(final1)),
            estmort = mortcalc(dt=survmort, c=final1, t=timepoint)["surv"],
            lclmort = mortcalc(dt=survmort, c=final1, t=timepoint)["lcl95"],
            uclmort = mortcalc(dt=survmort, c=final1, t=timepoint)["ucl95"])

timepoint = 60

temp4 = sdldat.wts[sdldat.wts$species == "SC" | sdldat.wts$species == "SG" | sdldat.wts$species == "TC" ,]

newdat4 <- expand.grid(sizem=with(temp4, 
                                  seq(min(sizem), max(sizem), 0.1)),
                       #weightm=mean(sdldat.wts$weightm),
                       treatment=c('C', 'F'))

predmort_mean <- predict(fit.gcc, type="risk", newdata=newdat4, 
                         time = timepoint, level=0.66)

newdat4$pred <- 1-exp(-predmort_mean$pred)
newdat4$pred.lcl <- 1-exp(-predmort_mean$pred.lcl)
newdat4$pred.ucl <- 1-exp(-predmort_mean$pred.ucl)

finalmort4 <- group_by(temp4, sizem, species, treatment) %>%
  summarise(mortality = mean(final1), n=sum(!is.na(final1)),
            estmort = mortcalc(dt=survmort, c=final1, t=timepoint)["surv"],
            lclmort = mortcalc(dt=survmort, c=final1, t=timepoint)["lcl95"],
            uclmort = mortcalc(dt=survmort, c=final1, t=timepoint)["ucl95"])


newdat1$species = as.factor("SR")
newdat2$species = as.factor("OD")
newdat3$species = as.factor("Symp")
newdat4$species = as.factor("Others")


newdat = rbind(newdat1,newdat2,newdat3,newdat4)
finalmort = rbind(finalmort1,finalmort2,finalmort3,finalmort4)

finalmort$species = as.character(finalmort$species)
finalmort[finalmort$species == "SC" | finalmort$species == "SG" | finalmort$species == "TC",]$species = "Others"
finalmort$species = as.factor(finalmort$species)

newdat$size = round(newdat$sizem*46.99823,1)
finalmort$size = round(finalmort$sizem*46.99823,1)


ggplot(newdat, aes(x=size, y=pred, colour=treatment)) + 
  geom_line() + 
  facet_wrap(~species, nrow = 2, ncol = 2) +
  geom_ribbon(aes(ymin = pred.lcl, ymax=pred.ucl, 
                  fill=treatment, colour=NULL), alpha=0.1) +
  geom_pointrange(data= finalmort, aes(y=estmort, ymin=lclmort, ymax=uclmort,alpha=n),
                  position=position_jitter(width=5, height=0)) +
  xlab("Fragment size (ha)") +
  ylab("Per-capita mortality") +
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title=element_blank(), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 1),
    alpha = guide_legend(order = 0)
  ) 

ggplot(newdat, aes(x=size, y=pred, colour=treatment)) + 
  geom_line() + 
  facet_wrap(~species, nrow = 1, ncol = 3, scales = 'free') +
  geom_ribbon(aes(ymin = pred.lcl, ymax=pred.ucl, 
                  fill=treatment, colour=NULL), alpha=0.1) +
  geom_pointrange(data= finalmort, aes(y=estmort, ymin=lclmort, ymax=uclmort,alpha=n),
                  position=position_jitter(width=0.1, height=0)) +
  xlab("Fragment size") +
  ylab("Per-capita mortality") +
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none')




##Germination
##Check by species

library(survival)
library(coxme)
library(ggplot2)
library(ggthemes)
library(tidyverse)

temp = greentime[greentime$alignment == "Y" & greentime$species != "HC" & greentime$species != "DL",]

theme_set(theme_tufte())
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)

sdldat.wts <- subset(temp, !is.na(weightm)) 
sdldat.wts$species <- factor(sdldat.wts$species)
sdldat.wts$fragment <- factor(sdldat.wts$fragment)

fit.od <- coxme(Surv(survgerm, initial, type = "right") ~ (treatment + weightm + sizem)^2 + (1|fragment), data = temp[temp$species == "OD",])

summary(fit.od)

fit.sc <- coxme(Surv(survgerm, initial, type = "right") ~ (treatment + weightm + sizem)^2 + (1|fragment), data = temp[temp$species == "SC",])

summary(fit.sc)


fit.sg <- coxme(Surv(survgerm, initial, type = "right") ~ (treatment + weightm + sizem)^2 + (1|fragment), data = temp[temp$species == "SG",])

summary(fit.sg)

fit.sr <- coxme(Surv(survgerm, initial, type = "right") ~ (treatment + weightm + sizem)^2 + (1|fragment), data = temp[temp$species == "SR",])

summary(fit.sr)

fit.symp <- coxme(Surv(survgerm, initial, type = "right") ~ (treatment + weightm + sizem)^2 + (1|fragment), data = temp[temp$species == "Symp",])

summary(fit.symp)

fit.tc <- coxme(Surv(survgerm, initial, type = "right") ~ treatment*sizem + (1|fragment), data = temp[temp$species == "TC",])

summary(fit.tc)


timepoint = 110

temp1 = sdldat.wts[sdldat.wts$species == "SR",]

newdat1 <- expand.grid(weightm=with(temp1, 
                                  seq(min(weightm), max(weightm), 0.1)),
                       sizem=mean(sdldat.wts$sizem),
                       treatment=c('C', 'F'))

predgerm_mean <- predict(fit.sr, type="risk", newdata=newdat1, 
                         time = timepoint, level=0.66)

newdat1$pred <- 1-exp(-predgerm_mean$pred)
newdat1$pred.lcl <- 1-exp(-predgerm_mean$pred.lcl)
newdat1$pred.ucl <- 1-exp(-predgerm_mean$pred.ucl)

finalgerm1 <- group_by(temp1, weightm, species, treatment) %>%
  summarise(germality = mean(initial), n=sum(!is.na(initial)),
            estgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["surv"],
            lclgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["lcl95"],
            uclgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["ucl95"])

timepoint = 140

temp2 = sdldat.wts[sdldat.wts$species == "OD",]

newdat2 <- expand.grid(weightm=with(temp2, 
                                    seq(min(weightm), max(weightm), 0.1)),
                       sizem=mean(sdldat.wts$sizem),
                       treatment=c('C', 'F'))

predgerm_mean <- predict(fit.od, type="risk", newdata=newdat2, 
                         time = timepoint, level=0.66)

newdat2$pred <- 1-exp(-predgerm_mean$pred)
newdat2$pred.lcl <- 1-exp(-predgerm_mean$pred.lcl)
newdat2$pred.ucl <- 1-exp(-predgerm_mean$pred.ucl)

finalgerm2 <- group_by(temp2, weightm, species, treatment) %>%
  summarise(germality = mean(initial), n=sum(!is.na(initial)),
            estgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["surv"],
            lclgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["lcl95"],
            uclgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["ucl95"])

timepoint = 100

temp3 = sdldat.wts[sdldat.wts$species == "Symp",]

newdat3 <- expand.grid(weightm=with(temp3, 
                                    seq(min(weightm), max(weightm), 0.1)),
                       sizem=mean(sdldat.wts$sizem),
                       treatment=c('C', 'F'))

predgerm_mean <- predict(fit.symp, type="risk", newdata=newdat3, 
                         time = timepoint, level=0.66)

newdat3$pred <- 1-exp(-predgerm_mean$pred)
newdat3$pred.lcl <- 1-exp(-predgerm_mean$pred.lcl)
newdat3$pred.ucl <- 1-exp(-predgerm_mean$pred.ucl)

finalgerm3 <- group_by(temp3, weightm, species, treatment) %>%
  summarise(germality = mean(initial), n=sum(!is.na(initial)),
            estgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["surv"],
            lclgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["lcl95"],
            uclgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["ucl95"])

timepoint = 100

temp4 = sdldat.wts[sdldat.wts$species == "SC",]

newdat4 <- expand.grid(weightm=with(temp4, 
                                    seq(min(weightm), max(weightm), 0.1)),
                       sizem=mean(sdldat.wts$sizem),
                       treatment=c('C', 'F'))

predgerm_mean <- predict(fit.sc, type="risk", newdata=newdat4, 
                         time = timepoint, level=0.66)

newdat4$pred <- 1-exp(-predgerm_mean$pred)
newdat4$pred.lcl <- 1-exp(-predgerm_mean$pred.lcl)
newdat4$pred.ucl <- 1-exp(-predgerm_mean$pred.ucl)

finalgerm4 <- group_by(temp4, weightm, species, treatment) %>%
  summarise(germality = mean(initial), n=sum(!is.na(initial)),
            estgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["surv"],
            lclgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["lcl95"],
            uclgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["ucl95"])

timepoint = 100

temp5 = sdldat.wts[sdldat.wts$species == "SG",]

newdat5 <- expand.grid(weightm=with(temp5, 
                                    seq(min(weightm), max(weightm), 0.1)),
                       sizem=mean(sdldat.wts$sizem),
                       treatment=c('C', 'F'))

predgerm_mean <- predict(fit.sg, type="risk", newdata=newdat5, 
                         time = timepoint, level=0.66)

newdat5$pred <- 1-exp(-predgerm_mean$pred)
newdat5$pred.lcl <- 1-exp(-predgerm_mean$pred.lcl)
newdat5$pred.ucl <- 1-exp(-predgerm_mean$pred.ucl)

finalgerm5 <- group_by(temp5, weightm, species, treatment) %>%
  summarise(germality = mean(initial), n=sum(!is.na(initial)),
            estgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["surv"],
            lclgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["lcl95"],
            uclgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["ucl95"])


newdat1$species = as.factor("SR")
newdat2$species = as.factor("OD")
newdat3$species = as.factor("Symp")
newdat4$species = as.factor("SC")
newdat5$species = as.factor("SG")

newdat = rbind(newdat1,newdat2,newdat3,newdat4,newdat5)
finalgerm = rbind(finalgerm1,finalgerm2,finalgerm3,finalgerm4,finalgerm5)

newdat$weight = round(newdat$weightm*0.2720976,2)
finalgerm$weight = round(finalgerm$weightm*0.2720976,2)


ggplot(newdat, aes(x=weight, y=pred, colour=treatment)) + 
  geom_line() + 
  facet_wrap(~species, nrow = 5, ncol = 1) +
  geom_ribbon(aes(ymin = pred.lcl, ymax=pred.ucl, 
                  fill=treatment, colour=NULL), alpha=0.1) +
  geom_point(data= finalgerm, aes(y=estgerm, alpha=n),position=position_jitter(width=0.001, height=0), size = 2) +
  #geom_pointrange(data= finalgerm, aes(y=estgerm, ymin=estgerm, ymax=estgerm,alpha=n),
  #                position=position_jitter(width=0.001, height=0), size = 2) +
  xlab("Seed mass (g)") +
  ylab("Probability of germination") +
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title=element_blank(), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_y_continuous(breaks = c(0,0.5,1)) +
  scale_size(guide = 'none') +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 1),
    alpha = guide_legend(order = 0)
  )

ggplot(newdat, aes(x=weight, y=pred, colour=treatment)) + 
  geom_line() + 
  facet_wrap(~species, nrow = 2, ncol = 3, scales = 'free') +
  geom_ribbon(aes(ymin = pred.lcl, ymax=pred.ucl, 
                  fill=treatment, colour=NULL), alpha=0.1) +
  geom_pointrange(data= finalgerm, aes(y=estgerm, ymin=lclgerm, ymax=uclgerm,alpha=n),
                  position=position_jitter(width=0.001, height=0)) +
  xlab("Seed mass") +
  ylab("Probability of germination") +
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none')







timepoint = 60

tempt = temp[temp$species == "TC",]

newdatt <- expand.grid(sizem=with(tempt, 
                                    seq(min(sizem), max(sizem), 0.1)),
                       #sizem=mean(sdldat.wts$sizem),
                       treatment=c('C', 'F'))

predgerm_mean <- predict(fit.tc, type="risk", newdata=newdatt, 
                         time = timepoint, level=0.66)

newdatt$pred <- 1-exp(-predgerm_mean$pred)
newdatt$pred.lcl <- 1-exp(-predgerm_mean$pred.lcl)
newdatt$pred.ucl <- 1-exp(-predgerm_mean$pred.ucl)

finalgermt <- group_by(tempt, sizem, species, treatment) %>%
  summarise(germality = mean(initial), n=sum(!is.na(initial)),
            estgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["surv"],
            lclgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["lcl95"],
            uclgerm = germcalc(dt=survgerm, c=initial, t=timepoint)["ucl95"])


ggplot(newdatt, aes(x=size, y=pred, colour=treatment)) + 
  geom_line() + 
  #facet_wrap(~species, nrow = 3, ncol = 2, scales = 'free') +
  geom_ribbon(aes(ymin = pred.lcl, ymax=pred.ucl, 
                  fill=treatment, colour=NULL), alpha=0.1) +
  #geom_pointrange(data= finalgerm, aes(y=estgerm, ymin=lclgerm, ymax=uclgerm,alpha=n),
  #                position=position_jitter(width=0.001, height=0)) +
  geom_point(data= finalgermt, aes(y=estgerm, alpha=n),position=position_jitter(width=0.001, height=0)) +
  xlab("Fragment size (ha)") +
  ylab("Probability of germination") +
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none')

newdatt$size = round(newdatt$sizem*46.48985,1)
finalgermt$size = round(finalgermt$sizem*46.48985,1)
