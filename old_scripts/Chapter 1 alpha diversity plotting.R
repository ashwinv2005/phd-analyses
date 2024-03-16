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

temp = sggro2
temp$plot57 = 0
temp[temp$plot == "5" | temp$plot == "7",]$plot57 = 1
temp$plot67 = 0
temp[temp$plot == "6" | temp$plot == "7",]$plot67 = 1
temp$plot57 = as.factor(temp$plot57)
temp$plot67 = as.factor(temp$plot67)
temp$sizem = scale(temp$size, center = F)
temp[95,]$slope = 5
temp$slope = temp$slope/100
temp$slope = scale(temp$slope, center = F)


mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

### all species ###

temp1 = temp[temp$shannonp != 0,]

fit9c = lmer (trans ~ shannonp + (plot57 + plot67)*sizem + slope + (1|site/location), 
              data = temp1, contrasts = list(plot57 = cMat, plot67 = cMat))

summary(fit9c)



### no SR and SP ###

temp2 = temp[temp$shannonIp != 0,]

fit9d = lmer (transI ~ shannonIp + (plot57 + plot67)*sizem + slope + (1|site/location), 
              data = temp2, contrasts = list(plot57 = cMat, plot67 = cMat))

summary(fit9d)





### plotting - all species

temp1$pred = NA
temp1[!is.na(temp1$trans),]$pred = predict(fit9c, re.form = NA, allow.new.levels=TRUE)

a = predict(fit9c, re.form = NA)
b = predict(fit9c)
c = b - a
temp1$newtrans = NA
temp1[!is.na(temp1$trans),]$newtrans = temp1[!is.na(temp1$trans),]$trans - c


temp1 = temp1[!is.na(temp1$trans),]

temp1$predm = 0
temp1$sel = 0
temp1$ser = 0


ltemp = temp1

predFun = function(fit) {
  predict(fit,ltemp, re.form = NA, allow.new.levels=TRUE)
}

bb = bootMer(fit9c,nsim=100,FUN=predFun)

for (i in 1:length(temp1$trans))
{
  temp1$predm[i] = median(bb$t[,i])
  temp1$sel[i] = median(bb$t[,i]) - sd(bb$t[,i])
  temp1$ser[i] = median(bb$t[,i]) + sd(bb$t[,i])
}

temp1$plotx = temp1$plot
temp1[temp1$plotx == "2",]$plotx = "1"

ggp = ggplot(temp1, aes(x = shannonp, y = predm, col = as.factor(plotx)))  +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = F) +
  geom_smooth(aes(x = shannonp, y = sel, col = as.factor(plotx)), method = "lm", se = F) +
  
  geom_smooth(aes(x = shannonp, y = ser, col = as.factor(plotx)), method = "lm", se = F)

temp1$plot12 = 0
temp1[temp1$plot == "1" | temp1$plot == "2",]$plot12 = 1

##### 1

selribx = ggplot_build(ggp)$data[[3]]$x[1:80]
selriby = ggplot_build(ggp)$data[[3]]$y[1:80]

serribx = ggplot_build(ggp)$data[[4]]$x[1:80]
serriby = ggplot_build(ggp)$data[[4]]$y[1:80]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp1[temp1$plot12 == 1,]$trans),replace = T),]
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

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "5",]$trans),replace = T),]
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

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "6",]$trans),replace = T),]
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

srib = srib[sample(nrow(srib),length(temp1[temp1$plot == "7",]$trans),replace = T),]
temp1$selribx[temp1$plot == "7"] = srib$selribx
temp1$selriby[temp1$plot == "7"] = srib$selriby
temp1$serriby[temp1$plot == "7"] = srib$serriby

l = min(temp1$selriby)
r = max(temp1$serriby)

ggp = ggplot(temp1, aes(x = shannonp, y = predm, col = as.factor(plotx), fill = as.factor(plotx)))  +
  geom_point(aes(x = shannonp, y = newtrans, col = as.factor(plotx)), size = 1) +
  geom_smooth(method = "lm", se = F, size = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab("") +
  ylab("") 
ggp1 = ggp + 
  ggtitle("all species") +
  theme(plot.title = element_text(hjust = 0.5, size = 9)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 8)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  scale_size(guide = 'none') +
  #scale_y_continuous(limits = c(0.04,0.20)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))





### plotting - no SR and SP

temp2$pred = NA
temp2[!is.na(temp2$transI),]$pred = predict(fit9d, re.form = NA, allow.new.levels=TRUE)

a = predict(fit9d, re.form = NA)
b = predict(fit9d)
c = b - a
temp2$newtransI = NA
temp2[!is.na(temp2$transI),]$newtransI = temp2[!is.na(temp2$transI),]$transI - c


temp2 = temp2[!is.na(temp2$transI),]

temp2$predm = 0
temp2$sel = 0
temp2$ser = 0


ltemp = temp2

predFun = function(fit) {
  predict(fit,ltemp, re.form = NA, allow.new.levels=TRUE)
}

bb = bootMer(fit9d,nsim=100,FUN=predFun)

for (i in 1:length(temp2$transI))
{
  temp2$predm[i] = median(bb$t[,i])
  temp2$sel[i] = median(bb$t[,i]) - sd(bb$t[,i])
  temp2$ser[i] = median(bb$t[,i]) + sd(bb$t[,i])
}

temp2$plotx = temp2$plot
temp2[temp2$plotx == "2",]$plotx = "1"

ggp = ggplot(temp2, aes(x = shannonIp, y = predm, col = as.factor(plotx)))  +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = F) +
  geom_smooth(aes(x = shannonIp, y = sel, col = as.factor(plotx)), method = "lm", se = F) +
  
  geom_smooth(aes(x = shannonIp, y = ser, col = as.factor(plotx)), method = "lm", se = F)

temp2$plot12 = 0
temp2[temp2$plot == "1" | temp2$plot == "2",]$plot12 = 1

##### 1

selribx = ggplot_build(ggp)$data[[3]]$x[1:80]
selriby = ggplot_build(ggp)$data[[3]]$y[1:80]

serribx = ggplot_build(ggp)$data[[4]]$x[1:80]
serriby = ggplot_build(ggp)$data[[4]]$y[1:80]

srib = cbind(selribx,selriby,serriby)
srib = as.data.frame(srib)

srib = srib[sample(nrow(srib),length(temp2[temp2$plot12 == 1,]$transI),replace = T),]
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

srib = srib[sample(nrow(srib),length(temp2[temp2$plot == "5",]$transI),replace = T),]
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

srib = srib[sample(nrow(srib),length(temp2[temp2$plot == "6",]$transI),replace = T),]
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

srib = srib[sample(nrow(srib),length(temp2[temp2$plot == "7",]$transI),replace = T),]
temp2$selribx[temp2$plot == "7"] = srib$selribx
temp2$selriby[temp2$plot == "7"] = srib$selriby
temp2$serriby[temp2$plot == "7"] = srib$serriby

l = min(temp2$selriby)
r = max(temp2$serriby)

ggp = ggplot(temp2, aes(x = shannonIp, y = predm, col = as.factor(plotx), fill = as.factor(plotx)))  +
  geom_point(aes(x = shannonIp, y = newtransI, col = as.factor(plotx)), size = 1) +
  geom_smooth(method = "lm", se = F, size = 0.5) +
  geom_ribbon(aes(x = selribx, ymin = selriby,ymax = serriby, linetype = NA), alpha=0.1) +
  xlab("initial diversity") +
  ylab("") 
ggp2 = ggp + 
  ggtitle(expression(paste(without~italic("Sy. rubicundum")~and~italic("Sp. purpureus")))) +
  theme(plot.title = element_text(hjust = 0.5, size = 9)) +
  theme(axis.title.x = element_text(vjust = 0.5, size = 8), axis.text.x = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 8)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  scale_size(guide = 'none') +
  #scale_y_continuous(limits = c(0.04,0.20)) +
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

grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, c(lapply(plots, function(x)
      x + theme(legend.position="none")), list(nrow = 2))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

p = grid_arrange_shared_legend(ggp1, ggp2)

#, bottom=grid::textGrob("conspecific density (per square-meter)") top=grid::textGrob("seedling mortality from germination to establishment",
g = gridExtra::arrangeGrob(p,ncol=1, 
                           left=grid::textGrob("change in alpha diversity - germination to establishment", rot=90, gp = gpar(fontfamily = "serif", fontsize = 8, col = 'black'))) ; 
#grid::grid.newpage() ; 

tiff('C:/Users/ashwinv/Desktop/a.tiff', units="in", width=8, height=6, res=1000)
grid::grid.draw(g)
dev.off()






#############


mod = summary(fit9d)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")
fit8 = fit9c
fit8 = fit9d

influence = influence(fit8, obs = T)
cooks=cooks.distance(influence, sort = T)
dfbetas(influence, sort=TRUE, to.sort="plot7:statusm", abs=FALSE)

hist(ranef(fit8)[[1]][,1], xlab="Intercept", main="", breaks = 20) # random effects
hist(ranef(fit8)[[2]][,1], xlab="Intercept", main="", breaks = 20) # random effects


plot(predict(fit8), resid(fit8), xlab = "Fitted", ylab = "Residuals") # residulas vs. fitted both transformed
plot(na.omit(temp$slope), resid(fit8)) # residuals vs. predictor 1

qqnorm(resid(fit8))