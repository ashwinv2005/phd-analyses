library(ggplot2)
library(ggthemes)
library(tidyverse)
library(mgcv)

theme_set(theme_tufte())


###### Plots #####


plots$sgslmeanagg = transform(sgslabundallm, sum=rowSums(sgslabundallm))$sum/transform(saabundallm, sum=rowSums(saabundallm))$sum
plots[plots$sgslmeanagg == Inf,]$sgslmeanagg = NA


temp = data.frame(rep(1:8,6))
names(temp) = "site"
temp$size = rep(plots$size,6)
temp$site = rep(plots$site,6)
temp$species = rep(c("all species","Psychotria nigra","Dichapetalum gelonioides","Psychotria macrocarpa","Litsea floribunda","Nothopegia travancorica"), each = 8)
temp$species = as.factor(temp$species)
temp$agg = c(plots$sgslmeanagg,plots$sgslPsni,plots$sgslDige,plots$sgslPsma,plots$sgslLifl,plots$sgslNotr)

temp1 = temp

temp1$lsize = log(temp$size)

temp2 = data.frame(rep(seq(0,5,0.1),6))
names(temp2) = "lsize"
temp2$size = exp(temp2$lsize)
temp2$species = rep(c("all species","Psychotria nigra","Dichapetalum gelonioides","Psychotria macrocarpa","Litsea floribunda","Nothopegia travancorica"), each = 51)
temp2$species = as.factor(temp2$species)

temp2$pred = 0
temp2$cil = 0
temp2$cir = 0

ltemp1 = temp1[temp1$species == "all species",]
ltemp2 = temp1[temp1$species == "Psychotria nigra",]
ltemp3 = temp1[temp1$species == "Dichapetalum gelonioides",]
ltemp4 = temp1[temp1$species == "Psychotria macrocarpa",]
ltemp5 = temp1[temp1$species == "Litsea floribunda",]
ltemp6 = temp1[temp1$species == "Nothopegia travancorica",]  



library(matrixStats)
y = transform(saabundallm, sum=rowSums(saabundallm))
ltemp1$nagg = y$sum
ltemp1$agg1 = round(ltemp1$agg)

ltemp2$nagg = saabundallm$Psni
ltemp2$agg1 = round(ltemp2$agg)

ltemp3$nagg = saabundallm$Dige
ltemp3$agg1 = round(ltemp3$agg)

ltemp4$nagg = saabundallm$Psma
ltemp4$agg1 = round(ltemp4$agg)

ltemp5$nagg = saabundallm$Lifl
ltemp5$agg1 = round(ltemp5$agg)

ltemp6$nagg = saabundallm$Notr
ltemp6$agg1 = round(ltemp6$agg)


b1 = gam(agg1~s(lsize, k = 3), offset = log(nagg), family = nb(link = "sqrt"), data=ltemp1,method="REML")

b2 = gam(agg1~s(lsize, k = 3), offset = log(nagg), family = nb, data=ltemp2,method="REML")

b3 = gam(agg1~s(lsize, k = 3), offset = log(nagg), family = nb, data=ltemp3,method="REML")

b4 = gam(agg1~s(lsize, k = 3), offset = log(nagg), family = nb(link = "sqrt"), data=ltemp4,method="REML")

b5 = gam(agg1~s(lsize, k = 3), offset = log(nagg), family = nb(link = "sqrt"), data=ltemp5,method="REML")

b6 = gam(agg1~s(lsize, k = 3), offset = log(nagg), family = nb, data=ltemp6,method="REML")


d1 = predict(b1, newdata = data.frame(lsize = seq(0,5,0.1)), type = "response", se = T)

fit = d1$fit
upr = d1$fit + (2 * d1$se.fit)
lwr = d1$fit - (2 * d1$se.fit)

temp2$pred[1:51] = fit
temp2$cir[1:51] = upr
temp2$cil[1:51] = lwr


d2 = predict(b2,newdata = data.frame(lsize = seq(0,5,0.1)), type = "response", se = T)

fit = d2$fit
upr = d2$fit + (2 * d2$se.fit)
lwr = d2$fit - (2 * d2$se.fit)

temp2$pred[52:102] = b2$family$linkinv(fit)
temp2$cir[52:102] = b2$family$linkinv(upr)
temp2$cil[52:102] = b2$family$linkinv(lwr)


d3 = predict(b3,newdata = data.frame(lsize = seq(0,5,0.1)), type = "link", se = T)

fit = d3$fit
upr = d3$fit + (2 * d3$se.fit)
lwr = d3$fit - (2 * d3$se.fit)

temp2$pred[103:153] = b3$family$linkinv(fit)
temp2$cir[103:153] = b3$family$linkinv(upr)
temp2$cil[103:153] = b3$family$linkinv(lwr)


d4 = predict(b4,newdata = data.frame(lsize = seq(0,5,0.1)), type = "response", se = T)

fit = d4$fit
upr = d4$fit + (2 * d4$se.fit)
lwr = d4$fit - (2 * d4$se.fit)

temp2$pred[154:204] = fit
temp2$cir[154:204] = upr
temp2$cil[154:204] = lwr


d5 = predict(b5,newdata = data.frame(lsize = seq(0,5,0.1)), type = "response", se = T)

fit = d5$fit
upr = d5$fit + (2 * d5$se.fit)
lwr = d5$fit - (2 * d5$se.fit)

temp2$pred[205:255] = fit
temp2$cir[205:255] = upr
temp2$cil[205:255] = lwr


d6 = predict(b6,newdata = data.frame(lsize = seq(0,5,0.1)), type = "link", se = T)

fit = d6$fit
upr = d6$fit + (2 * d6$se.fit)
lwr = d6$fit - (2 * d6$se.fit)

temp2$pred[256:306] = b6$family$linkinv(fit)
temp2$cir[256:306] = b6$family$linkinv(upr)
temp2$cil[256:306] = b6$family$linkinv(lwr)


temp2$species = factor(temp2$species,levels = c("all species","Psychotria nigra","Dichapetalum gelonioides","Psychotria macrocarpa","Litsea floribunda","Nothopegia travancorica"))

tempp1 = temp2
tempp2 = temp1

tempp1[tempp1$species == "Dichapetalum gelonioides" & tempp1$pred > 13,]$pred = NA
tempp1[tempp1$species == "Dichapetalum gelonioides" & tempp1$cir > 13,]$cir = 13
tempp1[tempp1$species == "Nothopegia travancorica" & tempp1$pred > 23,]$pred = NA
tempp1[tempp1$species == "Nothopegia travancorica" & tempp1$cir > 23,]$cir = 23

ggp = ggplot(tempp1, aes(x = size, y = pred))  +
  facet_wrap(~species, scale = "free_y") +
  geom_point(data = tempp2, aes(x = size, y = agg), size = 1) +
  geom_line(col = "red") +
  geom_ribbon(aes(x = size, ymin = cil, ymax=cir), alpha=0.1) +
  #geom_smooth(method = "lm", alpha = 0.1) +
  xlab("fragment size (ha)") +
  ylab("saplings per adult") 
ggp1 = ggp + 
  theme(axis.title.x = element_text(vjust = -0.5, size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(vjust = 3, angle = 90, size = 16), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 14))+
  theme(strip.text.x = element_text(size = 12, face = "italic")) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 1),
    alpha = guide_legend(order = 0)
  ) +
  #coord_cartesian(ylim=c(0, 23)) +
  scale_size(guide = 'none') +
  theme(legend.position = "none") +
  scale_x_log10()


############################


library(gridExtra)
library(grid)

tiff('C:/Users/ashwinv/Desktop/a.tiff', units="in", width=8, height=6, res=1000)
ggp1
dev.off()

