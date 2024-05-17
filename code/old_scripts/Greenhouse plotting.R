#load("Greenhouse.RData")
temp = greentime1[greentime1$initial == 1,]
temp$final1 = 1 - temp$final
temp = temp[temp$alignment == "Y" & temp$mechanism != "b" & temp$species != "HC" & temp$species != "DL" & !is.na(temp$survmort),]
head(greentime1)



library(survival)
library(coxme)
library(ggplot2)
library(survminer)
library(ggthemes)
library(splines)
library(mgcv)
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)

sdldat.wts <- subset(temp, !is.na(weightm)) 
sdldat.wts <- subset(temp) 

# with weights
fit0 <-  coxph(Surv(survmort, final1, type = "right") ~ (sizem + treatment + weightm)^2, data = sdldat.wts)

fit1 <-  coxme(Surv(survmort, final1, type = "right") ~ (sizem + treatment + weightm)^2 + (1|species) + (1|fragment), data = sdldat.wts)

# without weights
fit2 <-  coxme(Surv(survmort, final1, type = "right") ~ treatment*sizem + (1|species) + (1|fragment), data = temp)

summary(fit2)

fitted <- 1-exp(-predict(fit1, type="risk"))

sdldat.wts$pred <- fitted

ggplot(sdldat.wts, aes(x=sizem, y=pred, colour=treatment)) + 
  geom_point() + facet_wrap(~species)


newdata <- expand.grid(sizem=seq(0, 3, 1),
                       weightm=seq(1,1,1),
                       treatment=c('C', 'F'))
## Making a prediction at maximum time of experiment - need to alter if 
## different species had different times.
predmort <- predict(fit2, type="risk", newdata=newdata, 
                    time = max(sdldat.wts$survmort), level=0.66)
newdata$pred <- 1-exp(-predmort$pred)
newdata$pred.lcl <- 1-exp(-predmort$pred.lcl)
newdata$pred.ucl <- 1-exp(-predmort$pred.ucl)

ggplot(newdata, aes(x=sizem, y=pred)) + 
  geom_line(aes(colour=treatment)) + 
  geom_ribbon(aes(ymin = pred.lcl, ymax=pred.ucl, fill=treatment), alpha=0.1) +
  theme_tufte() +
  geom_point(data= sdldat.wts, aes(x=sizem, y=pred, colour=treatment)) 


ggplot(sdldat.wts, aes(x=size, y=pred, colour=treatment)) + 
  geom_point() + geom_smooth() + facet_wrap(~species, scales = 'free')


ggp = ggplot(sdldat.wts, aes(x = size, y = pred, col = treatment))  +
  #geom_point(size = 0.01) +
  geom_point(size = 1) +
  #geom_jitter(height = 0.05) +
  geom_smooth(method = "gam",
              formula = y ~ s(x, bs = "cs", k = 3)) +
  facet_wrap(~species, scales = 'free') +
  xlab("Fragment size") +
  ylab("Per-capita mortality") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  scale_x_continuous(breaks = c(0,25,50,75,100,125,150), limits = c(0,150)) +
  scale_color_manual(values = c("#D55E00", "#009E73"), name="",
                     breaks=c("C","F"),
                     labels=c("Control", "Fungicide"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))