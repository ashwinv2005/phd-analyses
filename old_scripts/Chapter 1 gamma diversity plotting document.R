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

########### all species ##########

temp = div12567
temp$plot57 = 0
temp[temp$plot == "5" | temp$plot == "7",]$plot57 = 1
temp$plot67 = 0
temp[temp$plot == "6" | temp$plot == "7",]$plot67 = 1
temp$plot57 = as.factor(temp$plot57)
temp$plot67 = as.factor(temp$plot67)


temp1 = temp[temp$size == "Small",]
temp2 = temp[temp$size == "Medium",]
temp3 = temp[temp$size == "Large",]

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit9a = lm (trans ~ initial + plot57 + plot67,
              data = temp1, contrasts = list(plot57 = cMat, plot67 = cMat))

fit9b = lm (trans ~ initial + plot57 + plot67,
            data = temp2, contrasts = list(plot57 = cMat, plot67 = cMat))

fit9c = lm (trans ~ initial + plot57 + plot67,
            data = temp3, contrasts = list(plot57 = cMat, plot67 = cMat))

df1 = data.frame(initial = rep(mean(temp1$initial),4), plot57 = as.factor(c(0,1,0,1)),
                 plot67 = as.factor(c(0,0,1,1)))
pred1 = predict (fit9a, df1, se = T)

df2 = data.frame(initial = rep(mean(temp2$initial),4), plot57 = as.factor(c(0,1,0,1)),
                 plot67 = as.factor(c(0,0,1,1)))
pred2 = predict (fit9b, df1, se = T)

df3 = data.frame(initial = rep(mean(temp3$initial),4), plot57 = as.factor(c(0,1,0,1)),
                 plot67 = as.factor(c(0,0,1,1)))
pred3 = predict (fit9c, df1, se = T)

########### No SR and SP ##########

temp = Idiv12567
temp$plot57 = 0
temp[temp$plot == "5" | temp$plot == "7",]$plot57 = 1
temp$plot67 = 0
temp[temp$plot == "6" | temp$plot == "7",]$plot67 = 1
temp$plot57 = as.factor(temp$plot57)
temp$plot67 = as.factor(temp$plot67)


temp4 = temp[temp$size == "Small",]
temp5 = temp[temp$size == "Medium",]
temp6 = temp[temp$size == "Large",]

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit9d = lm (trans ~ initial + plot57 + plot67,
            data = temp4, contrasts = list(plot57 = cMat, plot67 = cMat))

fit9e = lm (trans ~ initial + plot57 + plot67,
            data = temp5, contrasts = list(plot57 = cMat, plot67 = cMat))

fit9f = lm (trans ~ initial + plot57 + plot67,
            data = temp6, contrasts = list(plot57 = cMat, plot67 = cMat))

df4 = data.frame(initial = rep(mean(temp4$initial),4), plot57 = as.factor(c(0,1,0,1)),
                 plot67 = as.factor(c(0,0,1,1)))
pred4 = predict (fit9d, df1, se = T)

df5 = data.frame(initial = rep(mean(temp5$initial),4), plot57 = as.factor(c(0,1,0,1)),
                 plot67 = as.factor(c(0,0,1,1)))
pred5 = predict (fit9e, df1, se = T)

df6 = data.frame(initial = rep(mean(temp6$initial),4), plot57 = as.factor(c(0,1,0,1)),
                 plot67 = as.factor(c(0,0,1,1)))
pred6 = predict (fit9f, df1, se = T)


####### plotting - small all #######


temp1$plotx = temp1$plot
temp1[temp1$plotx == "2",]$plotx = "1"

t1 = summarySE(temp1, groupvar = "plotx", measurevar = "trans")
t1 = t1[,-c(2,4,5,6)]
t1$size = as.factor("Small")
t1$lci = 0
t1$rci = 0
for (i in 1:4)
{
  t1$trans[i] = pred1$fit[i]
  t1$lci[i] = t1$trans[i] - 1.96*pred1$se.fit[i]
  t1$rci[i] = t1$trans[i] + 1.96*pred1$se.fit[i]
}

##

pd = position_dodge(0.3)

ggp = ggplot(t1, aes(x = size, y = trans, col = as.factor(plotx)))  +
  #geom_violin(data = temp1, aes(x = size, y = trans, col = as.factor(plotx)), size = 0.5, position = pd) +
  geom_errorbar(data = t1, aes(ymin = lci,ymax = rci, col = as.factor(plotx)), size = 1, width = 0.2, 
                position = pd) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlab("") +
  ylab("") 
ggp1 = ggp + 
  #ggtitle("all species") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 10)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  scale_size(guide = 'none') +
  scale_y_continuous(limits = c(-0.3,0.5)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) 

####### plotting - medium all #######


temp2$plotx = temp2$plot
temp2[temp2$plotx == "2",]$plotx = "1"

t2 = summarySE(temp2, groupvar = "plotx", measurevar = "trans")
t2 = t2[,-c(2,4,5,6)]
t2$size = as.factor("Medium")
t2$lci = 0
t2$rci = 0
for (i in 1:4)
{
  t2$trans[i] = pred2$fit[i]
  t2$lci[i] = t2$trans[i] - 1.96*pred2$se.fit[i]
  t2$rci[i] = t2$trans[i] + 1.96*pred2$se.fit[i]
}

##

pd = position_dodge(0.3)

ggp = ggplot(t2, aes(x = size, y = trans, col = as.factor(plotx)))  +
  #geom_violin(data = temp2, aes(x = size, y = trans, col = as.factor(plotx)), size = 0.5, position = pd) +
  geom_errorbar(data = t2, aes(ymin = lci,ymax = rci, col = as.factor(plotx)), size = 1, width = 0.2, 
                position = pd) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlab("") +
  ylab("") 
ggp2 = ggp + 
  #ggtitle("all species") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  scale_size(guide = 'none') +
  scale_y_continuous(limits = c(-0.3,0.5)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) 

####### plotting - large all #######


temp3$plotx = temp3$plot
temp3[temp3$plotx == "2",]$plotx = "1"

t3 = summarySE(temp3, groupvar = "plotx", measurevar = "trans")
t3 = t3[,-c(2,4,5,6)]
t3$size = as.factor("Large")
t3$lci = 0
t3$rci = 0
for (i in 1:4)
{
  t3$trans[i] = pred3$fit[i]
  t3$lci[i] = t3$trans[i] - 1.96*pred3$se.fit[i]
  t3$rci[i] = t3$trans[i] + 1.96*pred3$se.fit[i]
}

##

pd = position_dodge(0.3)

ggp = ggplot(t3, aes(x = size, y = trans, col = as.factor(plotx)))  +
  #geom_violin(data = temp3, aes(x = size, y = trans, col = as.factor(plotx)), size = 0.5, position = pd) +
  geom_errorbar(data = t3, aes(ymin = lci,ymax = rci, col = as.factor(plotx)), size = 1, width = 0.2, 
                position = pd) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlab("") +
  ylab("") 
ggp3 = ggp + 
  #ggtitle("all species") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(axis.title.x = element_text(vjust = 0.5, size = 9), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  scale_size(guide = 'none') +
  scale_y_continuous(limits = c(-0.3,0.5)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) 

####### plotting - small no SR SP #######


temp4$plotx = temp4$plot
temp4[temp4$plotx == "2",]$plotx = "1"

t4 = summarySE(temp4, groupvar = "plotx", measurevar = "trans")
t4 = t4[,-c(2,4,5,6)]
t4$size = as.factor("Small")
t4$lci = 0
t4$rci = 0
for (i in 1:4)
{
  t4$trans[i] = pred4$fit[i]
  t4$lci[i] = t4$trans[i] - 1.96*pred4$se.fit[i]
  t4$rci[i] = t4$trans[i] + 1.96*pred4$se.fit[i]
}

##

pd = position_dodge(0.3)
t4$size = "small fragments (<10 ha)"
temp4$size = "small fragments (<10 ha)"

ggp = ggplot(t4, aes(x = size, y = trans, col = as.factor(plotx)))  +
  #geom_violin(data = temp4, aes(x = size, y = trans, col = as.factor(plotx)), size = 0.5, position = pd) +
  geom_errorbar(data = t4, aes(ymin = lci,ymax = rci, col = as.factor(plotx)), size = 1, width = 0.2, 
                position = pd) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlab("small fragments") +
  ylab("") 
ggp4 = ggp + 
  #ggtitle(expression(atop(paste(without~italic("Sy. rubicundum")),paste(and~italic("Sp. purpureus"))))) +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 9),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 10)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  scale_size(guide = 'none') +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) 

####### plotting - medium no SR SP #######


temp5$plotx = temp5$plot
temp5[temp5$plotx == "2",]$plotx = "1"

t5 = summarySE(temp5, groupvar = "plotx", measurevar = "trans")
t5 = t5[,-c(2,4,5,6)]
t5$size = as.factor("Medium")
t5$lci = 0
t5$rci = 0
for (i in 1:4)
{
  t5$trans[i] = pred5$fit[i]
  t5$lci[i] = t5$trans[i] - 1.96*pred5$se.fit[i]
  t5$rci[i] = t5$trans[i] + 1.96*pred5$se.fit[i]
}

##

pd = position_dodge(0.3)
t5$size = "medium fragments (10-60 ha)"
temp5$size = "medium fragments (10-60 ha)"

ggp = ggplot(t5, aes(x = size, y = trans, col = as.factor(plotx)))  +
  #geom_violin(data = temp5, aes(x = size, y = trans, col = as.factor(plotx)), size = 0.5, position = pd) +
  geom_errorbar(data = t5, aes(ymin = lci,ymax = rci, col = as.factor(plotx)), size = 1, width = 0.2, 
                position = pd) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlab("medium fragments") +
  ylab("") 
ggp5 = ggp + 
  #ggtitle(expression(atop(paste(without~italic("Sy. rubicundum")),paste(and~italic("Sp. purpureus"))))) +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 9),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  scale_size(guide = 'none') +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined"))

####### plotting - large no SR SP #######


temp6$plotx = temp6$plot
temp6[temp6$plotx == "2",]$plotx = "1"

t6 = summarySE(temp6, groupvar = "plotx", measurevar = "trans")
t6 = t6[,-c(2,4,5,6)]
t6$size = as.factor("Large")
t6$lci = 0
t6$rci = 0
for (i in 1:4)
{
  t6$trans[i] = pred6$fit[i]
  t6$lci[i] = t6$trans[i] - 1.96*pred6$se.fit[i]
  t6$rci[i] = t6$trans[i] + 1.96*pred6$se.fit[i]
}

##

pd = position_dodge(0.3)
t6$size = "large fragments (>60 ha)"
temp6$size = "large fragments (>60 ha)"

ggp = ggplot(t6, aes(x = size, y = trans, col = as.factor(plotx)))  +
  #geom_violin(data = temp6, aes(x = size, y = trans, col = as.factor(plotx)), size = 0.5, position = pd) +
  geom_errorbar(data = t6, aes(ymin = lci,ymax = rci, col = as.factor(plotx)), size = 1, width = 0.2, 
                position = pd) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlab("large fragments") +
  ylab("") 
ggp6 = ggp + 
  #ggtitle(expression(atop(paste(without~italic("Sy. rubicundum")),paste(and~italic("Sp. purpureus"))))) +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 9),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  scale_size(guide = 'none') +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
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

p1 = grid_arrange_shared_legend1(ggp1, ggp2, ggp3)
p2 = grid_arrange_shared_legend2(ggp4, ggp5, ggp6)

#, bottom=grid::textGrob("conspecific density (per square-meter)") top=grid::textGrob("seedling mortality from germination to establishment",
g = gridExtra::arrangeGrob(p1,p2,nrow=2,ncol=1, 
                           left=grid::textGrob("change in gamma diversity - germination to establishment", rot=90, gp = gpar(fontfamily = "serif", fontsize = 12, col = 'black'))) ; 
#grid::grid.newpage() ; 

tiff('C:/Users/admin/Desktop/a.tiff', units="in", width=5, height=7, res=1000)
grid::grid.draw(g)
dev.off()


















########### presentation control ##################

library(gridExtra)
library(grid)
t3$treat = 1:4
temp3$treat = c(rep(1,600),rep(2,300),rep(3,300),rep(4,300))


ggpw = ggplot(t3[t3$plotx == "1",], aes(x = treat, y = trans, col = as.factor(plotx)))  +
  geom_violin(data = temp3[temp3$plotx == "1",], aes(x = treat, y = trans, col = as.factor(plotx)), size = 0.5) +
  #geom_errorbar(aes(ymin = lci,ymax = rci), size = 5, width = 0, position = pd, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlab("") +
  ylab("change in gamma diversity") 


ggpw1 = ggpw + 
  ggtitle("large fragments - all species") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(0,4), breaks = c(1,3)) +
  scale_y_continuous(limits = c(-0.55,1.3)) +
  scale_color_manual(values = c("#56B4E9","#009E73","#D55E00","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#009E73","#D55E00","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))


ggp = ggplot(t3[t3$plotx == "1" | t3$plotx == "6",], aes(x = treat, y = trans, col = as.factor(plotx)))  +
  geom_violin(data = temp3[temp3$plotx == "1" | temp3$plotx == "6",], aes(x = treat, y = trans, col = as.factor(plotx)), size = 0.5) +
  #geom_errorbar(aes(ymin = lci,ymax = rci), size = 5, width = 0, position = pd, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlab("") +
  ylab("change in gamma diversity") 


ggp1 = ggp + 
  ggtitle("large fragments - all species") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(0,4), breaks = c(1,3)) +
  scale_y_continuous(limits = c(-0.55,1.3)) +
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







########### presentation control insecticide ##################

library(gridExtra)
library(grid)
t3$treat = 1:4
temp3$treat = c(rep(1,600),rep(2,300),rep(3,300),rep(4,300))


ggpw = ggplot(t3[t3$plotx == "1" | t3$plotx == "6",], aes(x = treat, y = trans, col = as.factor(plotx)))  +
  geom_violin(data = temp3[temp3$plotx == "1" | temp3$plotx == "6",], aes(x = treat, y = trans, col = as.factor(plotx)), size = 0.5) +
  #geom_errorbar(aes(ymin = lci,ymax = rci), size = 5, width = 0, position = pd, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlab("") +
  ylab("change in gamma diversity") 


ggpw1 = ggpw + 
  ggtitle("large fragments - all species") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(0,4), breaks = c(1,3)) +
  scale_y_continuous(limits = c(-0.55,1.3)) +
  scale_color_manual(values = c("#56B4E9","#009E73","#D55E00","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#009E73","#D55E00","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))


ggp = ggplot(t3[t3$plotx == "1" | t3$plotx == "6",], aes(x = treat, y = trans, col = as.factor(plotx)))  +
  geom_violin(data = temp3[temp3$plotx == "1" | temp3$plotx == "6",], aes(x = treat, y = trans, col = as.factor(plotx)), size = 0.5) +
  #geom_errorbar(aes(ymin = lci,ymax = rci), size = 5, width = 0, position = pd, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlab("") +
  ylab("change in gamma diversity") 


ggp1 = ggp + 
  ggtitle("large fragments - all species") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(0,4), breaks = c(1,3)) +
  scale_y_continuous(limits = c(-0.55,1.3)) +
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




########### presentation control insecticide without SR SP ##################

library(gridExtra)
library(grid)
t6$treat = 1:4
temp6$treat = c(rep(1,600),rep(2,300),rep(3,300),rep(4,300))


ggpw = ggplot(t6[t6$plotx == "1" | t6$plotx == "6",], aes(x = treat, y = trans, col = as.factor(plotx)))  +
  geom_violin(data = temp6[temp6$plotx == "1" | temp6$plotx == "6",], aes(x = treat, y = trans, col = as.factor(plotx)), size = 0.5) +
  #geom_errorbar(aes(ymin = lci,ymax = rci), size = 5, width = 0, position = pd, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlab("") +
  ylab("change in gamma diversity") 


ggpw1 = ggpw + 
  ggtitle(expression(paste(large~fragments~-~without~italic("Sy. rubicundum")~and~italic("Sp. purpureus")))) +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(0,4), breaks = c(1,3)) +
  scale_y_continuous(limits = c(-0.55,1.3)) +
  scale_color_manual(values = c("#56B4E9","#009E73","#D55E00","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#009E73","#D55E00","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))


ggp = ggplot(t6[t6$plotx == "1" | t6$plotx == "6",], aes(x = treat, y = trans, col = as.factor(plotx)))  +
  geom_violin(data = temp6[temp6$plotx == "1" | temp6$plotx == "6",], aes(x = treat, y = trans, col = as.factor(plotx)), size = 0.5) +
  #geom_errorbar(aes(ymin = lci,ymax = rci), size = 5, width = 0, position = pd, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlab("") +
  ylab("change in gamma diversity") 


ggp1 = ggp + 
  ggtitle(expression(paste(large~fragments~without~-~italic("Sy. rubicundum")~and~italic("Sp. purpureus")))) +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(0,4), breaks = c(1,3)) +
  scale_y_continuous(limits = c(-0.55,1.3)) +
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






########### presentation small control insecticide ##################

library(gridExtra)
library(grid)
t1$treat = 1:4
temp1$treat = c(rep(1,600),rep(2,300),rep(3,300),rep(4,300))


ggpw = ggplot(t1[t1$plotx == "1" | t1$plotx == "6",], aes(x = treat, y = trans, col = as.factor(plotx)))  +
  geom_violin(data = temp1[temp1$plotx == "1" | temp1$plotx == "6",], aes(x = treat, y = trans, col = as.factor(plotx)), size = 0.5) +
  #geom_errorbar(aes(ymin = lci,ymax = rci), size = 5, width = 0, position = pd, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlab("") +
  ylab("change in gamma diversity") 


ggpw1 = ggpw + 
  ggtitle("small fragments - all species") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(0,4), breaks = c(1,3)) +
  scale_y_continuous(limits = c(-0.55,1.3)) +
  scale_color_manual(values = c("#56B4E9","#009E73","#D55E00","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#009E73","#D55E00","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))


ggp = ggplot(t1[t1$plotx == "1" | t1$plotx == "6",], aes(x = treat, y = trans, col = as.factor(plotx)))  +
  geom_violin(data = temp1[temp1$plotx == "1" | temp1$plotx == "6",], aes(x = treat, y = trans, col = as.factor(plotx)), size = 0.5) +
  #geom_errorbar(aes(ymin = lci,ymax = rci), size = 5, width = 0, position = pd, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlab("") +
  ylab("change in gamma diversity") 


ggp1 = ggp + 
  ggtitle("small fragments - all species") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(0,4), breaks = c(1,3)) +
  scale_y_continuous(limits = c(-0.55,1.3)) +
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







############################################ fungi ####################################




########### presentation control fungicide ##################

library(gridExtra)
library(grid)
t3$treat = c(1,3,2,4)
temp3$treat = c(rep(1,600),rep(3,300),rep(2,300),rep(4,300))


ggpw = ggplot(t3[t3$plotx == "1" | t3$plotx == "5",], aes(x = treat, y = trans, col = as.factor(plotx)))  +
  geom_violin(data = temp3[temp3$plotx == "1" | temp3$plotx == "5",], aes(x = treat, y = trans, col = as.factor(plotx)), size = 0.5) +
  #geom_errorbar(aes(ymin = lci,ymax = rci), size = 5, width = 0, position = pd, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlab("") +
  ylab("change in gamma diversity") 


ggpw1 = ggpw + 
  ggtitle("large fragments - all species") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(0,4), breaks = c(1,3)) +
  scale_y_continuous(limits = c(-0.55,1.3)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))


ggp = ggplot(t3[t3$plotx == "1" | t3$plotx == "5",], aes(x = treat, y = trans, col = as.factor(plotx)))  +
  geom_violin(data = temp3[temp3$plotx == "1" | temp3$plotx == "5",], aes(x = treat, y = trans, col = as.factor(plotx)), size = 0.5) +
  #geom_errorbar(aes(ymin = lci,ymax = rci), size = 5, width = 0, position = pd, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlab("") +
  ylab("change in gamma diversity") 


ggp1 = ggp + 
  ggtitle("large fragments - all species") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(0,4), breaks = c(1,3)) +
  scale_y_continuous(limits = c(-0.55,1.3)) +
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




########### presentation control fungicide without SR SP ##################

library(gridExtra)
library(grid)
t6$treat = c(1,3,2,4)
temp6$treat = c(rep(1,600),rep(3,300),rep(2,300),rep(4,300))


ggpw = ggplot(t6[t6$plotx == "1" | t6$plotx == "5",], aes(x = treat, y = trans, col = as.factor(plotx)))  +
  geom_violin(data = temp6[temp6$plotx == "1" | temp6$plotx == "5",], aes(x = treat, y = trans, col = as.factor(plotx)), size = 0.5) +
  #geom_errorbar(aes(ymin = lci,ymax = rci), size = 5, width = 0, position = pd, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlab("") +
  ylab("change in gamma diversity") 


ggpw1 = ggpw + 
  ggtitle(expression(paste(large~fragments~-~without~italic("Sy. rubicundum")~and~italic("Sp. purpureus")))) +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(0,4), breaks = c(1,3)) +
  scale_y_continuous(limits = c(-0.55,1.3)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))


ggp = ggplot(t6[t6$plotx == "1" | t6$plotx == "5",], aes(x = treat, y = trans, col = as.factor(plotx)))  +
  geom_violin(data = temp6[temp6$plotx == "1" | temp6$plotx == "5",], aes(x = treat, y = trans, col = as.factor(plotx)), size = 0.5) +
  #geom_errorbar(aes(ymin = lci,ymax = rci), size = 5, width = 0, position = pd, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlab("") +
  ylab("change in gamma diversity") 


ggp1 = ggp + 
  ggtitle(expression(paste(large~fragments~without~-~italic("Sy. rubicundum")~and~italic("Sp. purpureus")))) +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'italic')) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(0,4), breaks = c(1,3)) +
  scale_y_continuous(limits = c(-0.55,1.3)) +
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






########### presentation small control insecticide ##################

library(gridExtra)
library(grid)
t1$treat = c(1,3,2,4)
temp1$treat = c(rep(1,600),rep(3,300),rep(2,300),rep(4,300))


ggpw = ggplot(t1[t1$plotx == "1" | t1$plotx == "5",], aes(x = treat, y = trans, col = as.factor(plotx)))  +
  geom_violin(data = temp1[temp1$plotx == "1" | temp1$plotx == "5",], aes(x = treat, y = trans, col = as.factor(plotx)), size = 0.5) +
  #geom_errorbar(aes(ymin = lci,ymax = rci), size = 5, width = 0, position = pd, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlab("") +
  ylab("change in gamma diversity") 


ggpw1 = ggpw + 
  ggtitle("small fragments - all species") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(0,4), breaks = c(1,3)) +
  scale_y_continuous(limits = c(-0.55,1.3)) +
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                     name="",
                     breaks=c("1","6","5","7"),
                     labels=c("control","insecticide","fungicide","combined")) +
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),
                    name="",
                    breaks=c("1","6","5","7"),
                    labels=c("control","insecticide","fungicide","combined"))


ggp = ggplot(t1[t1$plotx == "1" | t1$plotx == "5",], aes(x = treat, y = trans, col = as.factor(plotx)))  +
  geom_violin(data = temp1[temp1$plotx == "1" | temp1$plotx == "5",], aes(x = treat, y = trans, col = as.factor(plotx)), size = 0.5) +
  #geom_errorbar(aes(ymin = lci,ymax = rci), size = 5, width = 0, position = pd, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlab("") +
  ylab("change in gamma diversity") 


ggp1 = ggp + 
  ggtitle("small fragments - all species") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  theme(axis.title.x = element_text(vjust = -2, size = 12), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(vjust = 1, angle = 90, size = 12), axis.text.y = element_text(size = 9)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_size(guide = 'none') +
  scale_x_continuous(limits = c(0,4), breaks = c(1,3)) +
  scale_y_continuous(limits = c(-0.55,1.3)) +
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