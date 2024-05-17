## Seed compositions ##

grps = c("S2L1G2","S8L1G2","S19L1G2","S18L1G2","S12L2G3","S5L2G2","S13L1G2","S3L1G2")
sgrosub = sgro[sgro$groupfac %in% grps,]

sum = colSums(seedm)

names(seedm)[27] = "Lifl"

rm(sum,sgrosubm,sgrosub)

library(ggplot2)
library(ggthemes)
library(tidyverse)
library(vegan)
library(grid)

theme_set(theme_tufte())

x = seedfs$size
meta_table = data.frame(x)
names(meta_table) = "Fragment Size"

grouping_info = data.frame(x)
names(grouping_info) = "Fragment Size"
sol = metaMDS(seedm,distance = "bray", k = 2, trymax = 50)
NMDS=data.frame(x=sol$point[,1],y=sol$point[,2],Size=as.factor(grouping_info[,1]))
plot.new()
ord<-ordiellipse(sol, as.factor(grouping_info[,1]) ,display = "sites", kind ="sd", conf = 0.95, label = T)
dev.off()

NMDS.mean=aggregate(NMDS[,1:2],list(group=NMDS$Size),mean)

MDS_res=metaMDS(seedm, distance = nmethod, k = 2, trymax = 50)
bio.fit <- envfit(MDS_res, seedm, perm = 999)
env.fit <- envfit(MDS_res, meta_table, perm = 999)
df<-scores(MDS_res,display=c("sites"))
df<-data.frame(df,Type=grouping_info[rownames(df),1])

a = colSums(seedm)
b = names(a[a>10])

df_biofit<-scores(bio.fit,display=c("vectors"))
df_biofit<-df_biofit*vegan:::ordiArrowMul(df_biofit)
df_biofit<-as.data.frame(df_biofit)
df_biofit = df_biofit[rownames(df_biofit) %in% b,]

df_envfit<-scores(env.fit,display=c("vectors"))
df_envfit<-df_envfit*vegan:::ordiArrowMul(df_envfit)
df_envfit<-as.data.frame(df_envfit)

cbPalette <- c("#0072B2","#0072B2","#0072B2","#0072B2", "#CC79A7","#CC79A7", "#CC79A7", "#CC79A7")

df_envfit1 = df_envfit

ggpd = ggplot(data=NMDS,aes(x,y,colour=Size))  +
  ggtitle("Seeds") +
  geom_point(shape = 1, size = 12) +
  #geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2), size=1, linetype=2) +
  geom_segment(data=df_biofit, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="#808080",alpha=0.5) +
  geom_text(data=as.data.frame(df_biofit*1.1),aes(NMDS1, NMDS2, label = rownames(df_biofit)),
            color="#808080",alpha=1) +
  geom_segment(data=df_envfit, aes(x = 0, y = 0, xend = NMDS1/4, yend = NMDS2/4),
               arrow = arrow(length = unit(0.2, "cm")),color="black", size = 1) +
  geom_text(data=as.data.frame(df_envfit*1.1),aes(NMDS1, NMDS2, 
                                                  label = rownames(df_envfit)),color="black",size = 4) +
  annotate("text",x=NMDS.mean$x,y=NMDS.mean$y,label=NMDS.mean$group,size=4, col = cbPalette,fontface =2) 
gg2 = ggpd + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = 'italic')) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_shape_discrete(solid=F) +
  scale_color_manual(values = cbPalette) +
  scale_x_continuous(limits = c(-1,0.8), breaks = seq(-1,0.8,0.3)) +
  scale_y_continuous(limits = c(-0.8,1), breaks = seq(-0.8,1,0.3)) +
  scale_size(guide = 'none') +
  theme(legend.position = "none")

seedm = read.csv("C:/Users/ashwinv/Desktop/seeds.csv")
