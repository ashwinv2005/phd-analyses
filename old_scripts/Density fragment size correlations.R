library(survival)
library(coxme)
library(ggplot2)
library(ggthemes)
library(tidyverse)

theme_set(theme_tufte())

fit.sr1 = coxme(Surv(survmort, final1, type = "right") ~ (treatment + sddenm)^2 + (treatment + sizem)^2 +(1|fragment), 
                data = subset(temp, species=="SR"))

fit.sr2 = coxme(Surv(survmort, final1, type = "right") ~ (treatment + sldenm)^2 + (treatment + sizem)^2 +(1|fragment), 
                data = subset(temp, species=="SR"))

fit.sr3 = coxme(Surv(survmort, final1, type = "right") ~ (treatment + frdenm)^2 + (treatment + sizem)^2 +(1|fragment), 
                data = subset(temp, species=="SR"))

summary(fit.sr3)

density$density = as.factor(density$density)
density$type = factor(density$type, levels = c("seed rain","fruiting trees","seedlings"))

ggp = ggplot(density, aes(x = size, y = density))  +
  facet_wrap(~type, nrow = 3, scales="free_y") +
  geom_point(size = 1.5) +
  #geom_errorbar(aes(ymin = div-ci, ymax = div+ci), width = 0.1, size = 0.6) +
  geom_smooth(method = "lm", alpha = 0.05, size = 0.4) +
  xlab("fragment size (ha)") +
  ylab("density index") 
ggp1 = ggp +
  theme(axis.title.x = element_text(vjust = -0.5, size = 14), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(vjust = 3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title=element_blank(), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  theme(strip.text.x = element_text(size = 12))

tiff('C:/Users/admin/Desktop/a.tiff', units="in", width=5, height=7, res=1000)
ggp1
dev.off()

summary(with(density[density$type == "seed rain",],lm(density~size)))
summary(with(density[density$type == "fruiting trees",],lm(density~size)))
summary(with(density[density$type == "seedlings",],lm(density~size)))


        