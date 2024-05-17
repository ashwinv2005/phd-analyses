library(ggplot2)
library(ggthemes)
library(tidyverse)
library(vegan)
library(grid)
library(viridis)

theme_set(theme_tufte())

### Seeds ###

abund_table3 = sdabundallm
x = sort(log(seedfs$size))
meta_table3 = data.frame(x)
names(meta_table3) = "fragment size"

grouping_info = data.frame(x)
names(grouping_info) = "fragment size"
sol3 = metaMDS(abund_table3,distance = "bray", k = 2, trymax = 50)
NMDS3=data.frame(x=sol3$point[,1],y=sol3$point[,2],size=grouping_info[,1])


NMDS.mean3=aggregate(NMDS3[,1:2],list(group=NMDS3$size),mean)

bio.fit3 <- envfit(sol3, abund_table3, perm = 999)
env.fit3 <- envfit(sol3, meta_table3, perm = 999)
df3<-scores(sol3,display=c("sites"))
df3<-data.frame(df3,Type=grouping_info[rownames(df3),1])

a3 = colSums(sdabundallm)
b3 = names(a3[c(2,7,8,10,27)])

df_biofit3<-scores(bio.fit3,display=c("vectors"))
df_biofit3<-df_biofit3*vegan:::ordiArrowMul(df_biofit3)
df_biofit3<-as.data.frame(df_biofit3)
df_biofit3 = df_biofit3[rownames(df_biofit3) %in% b3,]

df_envfit3<-scores(env.fit3,display=c("vectors"))
#df_envfit3<-df_envfit3*vegan:::ordiArrowMul(df_envfit3)
df_envfit3<-as.data.frame(df_envfit3)

#cbPalette <- c("#0072B2","#0072B2","#0072B2","#0072B2", "#CC79A7","#CC79A7", "#CC79A7", "#CC79A7")


ggp = ggplot(data=NMDS3,aes(x,y)) +
  #ggtitle("seeds") +
  geom_point(shape = 16, aes(size = size), alpha = 0.6) +
  #geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2), size=1, linetype=2) +
  geom_segment(data=df_biofit3, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="#808080",alpha=0.5) +
  geom_text(data=as.data.frame(df_biofit3*1.1),aes(NMDS1, NMDS2, label = rownames(df_biofit3)),
            color="#808080",alpha=1) +
  geom_segment(data=df_envfit3, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                              arrow = arrow(length = unit(0.2, "cm")),color="black", size = 0.5) +
  geom_text(data=as.data.frame(df_envfit3*1.1),aes(NMDS1, NMDS2, 
                                                        label = rownames(df_envfit3)),color="black",size = 4) 

ggp2 = ggp + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = 'italic')) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 14, face = 'bold')) +
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  #scale_shape_discrete(solid=F) +
  scale_size(name = "fragment size (ha)",breaks = c(1,2,3,4,5),labels = c(3,10,25,50,150)) +
  scale_x_continuous(limits = c(-0.6,0.5)) +
  scale_y_continuous(limits = c(-0.9,0.4), breaks = seq(-0.8,1,0.3)) 


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

tiff('C:/Users/ashwinv/Desktop/a.tiff', units="in", width=10, height=6, res=1000)
grid_arrange_shared_legend(ggp2)
dev.off()


### all together, without seeds ###

abund_table = rbind(saabundallm,sgabundallm,slabundallm)
x = rep(log(plots$size),3)
y = as.factor(rep(c("ad","sg","sl"), each = 8))
z = specnumber(abund_table)
u = x*z
l = c(rep(1,8),rep(0,16))
m = c(rep(0,8),rep(1,8),rep(0,8))
n = c(rep(0,16),rep(1,8))
l = l*x
m = m*x
n = n*x
meta_table = data.frame(x,y,z,u,l,m,n)
names(meta_table) = c("fragsize","stage","spec","int","adults","youngsaplings","oldsaplings")



grouping_info = data.frame(x,y,z,u,l,m,n)
names(grouping_info) = c("fragsize","stage","spec","int","adults","youngsaplings","oldsaplings")
sol6 = metaMDS(abund_table,distance = "bray", k = 4, trymax = 50)
NMDS6=data.frame(x=sol6$point[,1],y=sol6$point[,2],size=grouping_info[,1],stage=grouping_info[,2],
                 adults=grouping_info[,5],youngsaplings=grouping_info[,6],
                 oldsaplings=grouping_info[,7])


NMDS.mean6=aggregate(NMDS6[,1:2],list(group=NMDS6$size),mean)

MDS_res6=metaMDS(abund_table, distance = nmethod, k = 4, trymax = 50)
bio.fit6 <- envfit(MDS_res6, abund_table, perm = 999)
env.fit6 <- envfit(MDS_res6 ~ adults + youngsaplings + oldsaplings, meta_table, perm = 999)
df6<-scores(MDS_res6,display=c("sites"))
df6<-data.frame(df6,Type=grouping_info[rownames(df6),1])

a6 = colSums(abundallm)
b6 = names(a6[c(1,3:6,8,11,24,63)])

df_biofit6<-scores(bio.fit6,display=c("vectors"))
df_biofit6<-df_biofit6*vegan:::ordiArrowMul(df_biofit6)
df_biofit6<-as.data.frame(df_biofit6)
df_biofit6 = df_biofit6[rownames(df_biofit6) %in% b6,]

df_envfit6<-scores(env.fit6,display=c("vectors"))
df_envfit6<-df_envfit6*vegan:::ordiArrowMul(df_envfit6)
df_envfit6<-as.data.frame(df_envfit6)

#cbPalette <- c("#0072B2","#0072B2","#0072B2","#0072B2", "#CC79A7","#CC79A7", "#CC79A7", "#CC79A7")

ggp = ggplot(data=NMDS6,aes(x,y)) +
  #ggtitle("saplings") +
  geom_point(aes(size = size,shape=stage), alpha = 0.6) +
  #geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2), size=1, linetype=2) +
  geom_segment(data=df_biofit6, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="#808080",alpha=0.5) +
  geom_text(data=as.data.frame(df_biofit6*1.1),aes(NMDS1, NMDS2, label = rownames(df_biofit6)),
            color="#808080",alpha=1) +
  geom_segment(data=df_envfit6, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),
               #col = c("#56B4E9","#D55E00","#CC79A7"),
               size = 0.5) +
  geom_text(data=as.data.frame(df_envfit6*1.1),aes(NMDS1, NMDS2, 
                                                   label = c("fragment size - adults","fragment size - young","fragment size - old")), 
            #col = c("#56B4E9","#D55E00","#CC79A7"),
            size = 4) 
#annotate("text",x=NMDS.mean$x,y=NMDS.mean$y,label=NMDS.mean$group,size=4, col = cbPalette,fontface =2) 
ggp7 = ggp + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = 'italic')) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 14, face = 'bold')) +
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  #scale_shape_discrete(solid=F) +
  scale_size(name = "fragment size (ha)",breaks = c(1,2,3,4,5),labels = c(3,10,25,50,150)) +
  scale_shape_manual(values = c(15,16,17),
                      name = "life stage", breaks = c("ad","sg","sl"), 
                      labels = c("adults","young saplings","old saplings")) +
  
  #scale_colour_manual(values = c("#56B4E9","#D55E00","#CC79A7"),
  #                    name = "life stage", breaks = c("ad","sg","sl"), 
  #                    labels = c("adults","young saplings","old saplings")) +
  scale_x_continuous(limits = c(-1.4,0.9), breaks = seq(-1,0.8,0.3)) +
  scale_y_continuous(limits = c(-1.5,0.9), breaks = seq(-0.8,1,0.3)) 




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

grid_arrange_shared_legend(ggp7)

tiff('C:/Users/admin/Desktop/a.tiff', units="in", width=10, height=6, res=1000)
grid_arrange_shared_legend(ggp7)
dev.off()
