library(ggplot2)
library(ggthemes)
library(tidyverse)
library(vegan)
library(grid)

theme_set(theme_tufte())

tempa = saabundm
tempa[tempa>0] = 1

abund_tablea = saabundm[rowSums(tempa) >= 2,]
x = quadrats[rowSums(tempa) >= 2,]$size
meta_tablea = data.frame(x)
names(meta_tablea) = "fragment size"

grouping_info = data.frame(x)
names(grouping_info) = "fragment size"
sola = metaMDS(abund_tablea,distance = "bray", k = 4, trymax = 50)
NMDSa=data.frame(x=sola$point[,1],y=sola$point[,2],Size=as.factor(grouping_info[,1]))
plot.new()
orda<-ordiellipse(sola, as.factor(grouping_info[,1]) ,display = "sites", kind ="sd", conf = 0.95, label = T)
dev.off()

df_ella <- data.frame()
for(g in levels(NMDSa$Size)){
  if(g!="" && (g %in% names(orda))){
    
    df_ella <- rbind(df_ella, cbind(as.data.frame(with(NMDSa[NMDSa$Size==g,],
                                                     veganCovEllipse(orda[[g]]$cov,orda[[g]]$center,orda[[g]]$scale)))
                                  ,Size=g))
  }
}

NMDS.meana=aggregate(NMDSa[,1:2],list(group=NMDSa$Size),mean)

bio.fita <- envfit(sola, abund_tablea, perm = 999)
env.fita <- envfit(sola, meta_tablea, perm = 999)
dfa<-scores(sola,display=c("sites"))
dfa<-data.frame(dfa,Type=grouping_info[rownames(dfa),1])

aa = colSums(abundallm)
ba = names(aa[aa>119])

df_biofita<-scores(bio.fita,display=c("vectors"))
df_biofita<-df_biofita*vegan:::ordiArrowMul(df_biofita)
df_biofita<-as.data.frame(df_biofita)
df_biofita = df_biofita[rownames(df_biofita) %in% ba,]

df_envfita<-scores(env.fita,display=c("vectors"))
df_envfita<-df_envfita*vegan:::ordiArrowMul(df_envfita)
df_envfita<-as.data.frame(df_envfita)

cbPalette <- c("#0072B2","#0072B2","#0072B2","#0072B2", "#CC79A7","#CC79A7", "#CC79A7", "#CC79A7")

df_envfit1a = df_envfita
df_envfit1a[,2] = df_envfit1a[,2]*1.5

ggp5 = ggplot(data=NMDSa,aes(x,y,colour=Size))  +
  ggtitle("Adults") +
  geom_path(data=df_ella, aes(x=NMDS1, y=NMDS2), size=1, linetype=2) +
  geom_segment(data=df_biofita, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="#808080",alpha=0.5) +
  geom_text(data=as.data.frame(df_biofita*1.1),aes(NMDS1, NMDS2, label = rownames(df_biofita)),
            color="#808080",alpha=1) +
  geom_segment(data=df_envfita, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="black", size = 0.5) +
  geom_text(data=as.data.frame(df_envfit1a),aes(NMDS1, NMDS2, 
                                                  label = rownames(df_envfita)),color="black",size = 4) +
  annotate("text",x=NMDS.meana$x,y=NMDS.meana$y,label=NMDS.meana$group,size=4, col = cbPalette,fontface =2)
ggpa5 = ggp5 + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = 'italic')) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_color_manual(values = cbPalette) +
  scale_size(guide = 'none') +
  theme(legend.position = "none")


abund_table1 = sgslabundm
x = quadrats$size
meta_table1 = data.frame(x)
names(meta_table1) = "fragment size"

grouping_info = data.frame(x)
names(grouping_info) = "fragment size"
sol1 = metaMDS(abund_table1,distance = "bray", k = 4, trymax = 50)
NMDS1=data.frame(x=sol1$point[,1],y=sol1$point[,2],z=sol1$point[,3],
                 Size=as.factor(grouping_info[,1]))
plot.new()
ord1<-ordiellipse(sol1, as.factor(grouping_info[,1]) ,display = "sites", kind ="sd", conf = 0.95, label = T)
dev.off()

df_ell1 <- data.frame()
for(g in levels(NMDS1$Size)){
  if(g!="" && (g %in% names(ord1))){
    
    df_ell1 <- rbind(df_ell1, cbind(as.data.frame(with(NMDS1[NMDS1$Size==g,],
                                                     veganCovEllipse(ord1[[g]]$cov,ord1[[g]]$center,ord1[[g]]$scale)))
                                  ,Size=g))
  }
}

NMDS.mean1=aggregate(NMDS1[,1:3],list(group=NMDS1$Size),mean)

bio.fit1 <- envfit(sol1, abund_table1, perm = 999)
env.fit1 <- envfit(sol1, meta_table1, perm = 999)
df1<-scores(sol1,display=c("sites"))
df1<-data.frame(df1,Type=grouping_info[rownames(df1),1])

a1 = colSums(abundallm)
b1 = names(a1[c(1,3:6,11,24,63)])

df_biofit1<-scores(bio.fit1,display=c("vectors"))
df_biofit1<-df_biofit1*vegan:::ordiArrowMul(df_biofit1)
df_biofit1<-as.data.frame(df_biofit1)
df_biofit1 = df_biofit1[rownames(df_biofit1) %in% b1,]

df_envfit1<-scores(env.fit1,display=c("vectors"))
df_envfit1<-df_envfit1*vegan:::ordiArrowMul(df_envfit1)
df_envfit1<-as.data.frame(df_envfit1)

cbPalette <- c("#0072B2","#0072B2","#0072B2","#0072B2", "#CC79A7","#CC79A7", "#CC79A7", "#CC79A7")


ggp1 = ggplot(data=NMDS1,aes(x,y,colour=Size))  +
  ggtitle("Saplings") +
  geom_path(data=df_ell1, aes(x=NMDS1, y=NMDS2), size=1, linetype=2) +
  geom_segment(data=df_biofit1*1.3, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="#808080",alpha=0.5) +
  geom_text(data=as.data.frame(df_biofit1*1.4),aes(NMDS1, NMDS2, label = rownames(df_biofit1)),
            color="#808080",alpha=1) +
  geom_segment(data=df_envfit1, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="black", size = 1) +
  geom_text(data=as.data.frame(df_envfit1*1.03),aes(NMDS1, NMDS2, 
                                                  label = rownames(df_envfit1)),color="black",size = 4) +
  annotate("text",x=NMDS.mean1$x,y=NMDS.mean1$y,label=NMDS.mean1$group,size=4, col = cbPalette,fontface =2)
ggpb = ggp1 + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = 'italic')) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_x_continuous(limits = c(-1.8,1.1)) +
  scale_y_continuous(limits = c(-1,1), breaks = seq(-0.8,1,0.3)) +
  scale_color_manual(values = cbPalette) +
  scale_size(guide = 'none') +
  theme(legend.position = "none")


#multiplot(gg1,gg2)


## plots ##



abund_table2 = saabundallm
x = plots$size
meta_table2 = data.frame(x)
names(meta_table2) = "fragment size"

grouping_info = data.frame(x)
names(grouping_info) = "fragment size"
sol2 = metaMDS(abund_table2,distance = "bray", k = 2, trymax = 50)
NMDS2=data.frame(x=sol2$point[,1],y=sol2$point[,2],Size=as.factor(grouping_info[,1]))


NMDS.mean2=aggregate(NMDS2[,1:2],list(group=NMDS2$Size),mean)

bio.fit2 <- envfit(sol2, abund_table2, perm = 999)
env.fit2 <- envfit(sol2, meta_table2, perm = 999)
df2<-scores(sol2,display=c("sites"))
df2<-data.frame(df2,Type=grouping_info[rownames(df2),1])

a2 = colSums(abundallm)
b2 = names(a2[c(1,3:6,11,24,63)])

df_biofit2<-scores(bio.fit2,display=c("vectors"))
df_biofit2<-df_biofit2*vegan:::ordiArrowMul(df_biofit2)
df_biofit2<-as.data.frame(df_biofit2)
df_biofit2 = df_biofit2[rownames(df_biofit2) %in% b2,]

df_envfit2<-scores(env.fit2,display=c("vectors"))
df_envfit2<-df_envfit2*vegan:::ordiArrowMul(df_envfit2)
df_envfit2<-as.data.frame(df_envfit2)

cbPalette <- c("#0072B2","#0072B2","#0072B2","#0072B2", "#CC79A7","#CC79A7", "#CC79A7", "#CC79A7")


ggpc = ggplot(data=NMDS2,aes(x,y,colour=Size)) +
  ggtitle("Adults") +
  geom_point(shape = 1, size = 12) +
  #geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2), size=1, linetype=2) +
  geom_segment(data=df_biofit2, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="#808080",alpha=0.5) +
  geom_text(data=as.data.frame(df_biofit2*1.1),aes(NMDS1, NMDS2, label = rownames(df_biofit2)),
            color="#808080",alpha=1) +
  geom_segment(data=df_envfit2*1.1, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="black", size = 0.5) +
  geom_text(data=as.data.frame(df_envfit2*1.25),aes(NMDS1, NMDS2, 
                                                  label = rownames(df_envfit2)),color="black",size = 4) +
  annotate("text",x=NMDS.mean2$x,y=NMDS.mean2$y,label=NMDS.mean2$group,size=4, col = cbPalette,fontface =2)
gg1 = ggpc + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = 'italic')) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_shape_discrete(solid=F) +
  scale_color_manual(values = cbPalette) +
  scale_x_continuous(limits = c(-0.8,0.8)) +
  scale_y_continuous(limits = c(-0.8,1), breaks = seq(-0.8,1,0.3)) +
  scale_size(guide = 'none') +
  theme(legend.position = "none")


abund_table = sgslabundallm
x = plots$size
meta_table = data.frame(x)
names(meta_table) = "fragment size"



grouping_info = data.frame(x)
names(grouping_info) = "fragment size"
sol = metaMDS(abund_table,distance = "bray", k = 2, trymax = 50)
NMDS=data.frame(x=sol$point[,1],y=sol$point[,2],Size=as.factor(grouping_info[,1]))


NMDS.mean=aggregate(NMDS[,1:2],list(group=NMDS$Size),mean)

MDS_res=metaMDS(abund_table, distance = nmethod, k = 2, trymax = 50)
bio.fit <- envfit(MDS_res, abund_table, perm = 999)
env.fit <- envfit(MDS_res, meta_table, perm = 999)
df<-scores(MDS_res,display=c("sites"))
df<-data.frame(df,Type=grouping_info[rownames(df),1])

a = colSums(abundallm)
b = names(a[a>119])

df_biofit<-scores(bio.fit,display=c("vectors"))
df_biofit<-df_biofit*vegan:::ordiArrowMul(df_biofit)
df_biofit<-as.data.frame(df_biofit)
df_biofit = df_biofit[rownames(df_biofit) %in% b,]

df_envfit<-scores(env.fit,display=c("vectors"))
df_envfit<-df_envfit*vegan:::ordiArrowMul(df_envfit)
df_envfit<-as.data.frame(df_envfit)

cbPalette <- c("#0072B2","#0072B2","#0072B2","#0072B2", "#CC79A7","#CC79A7", "#CC79A7", "#CC79A7")


ggpd = ggplot(data=NMDS,aes(x,y,colour=Size))  +
  ggtitle("Saplings") +
  geom_point(shape = 1, size = 12) +
  #geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2), size=1, linetype=2) +
  geom_segment(data=df_biofit, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="#808080",alpha=0.5) +
  geom_text(data=as.data.frame(df_biofit*1.1),aes(NMDS1, NMDS2, label = rownames(df_biofit)),
            color="#808080",alpha=1) +
  geom_segment(data=df_envfit, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
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
  scale_x_continuous(limits = c(-0.8,0.8), breaks = seq(-1,0.8,0.3)) +
  scale_y_continuous(limits = c(-0.8,1), breaks = seq(-0.8,1,0.3)) +
  scale_size(guide = 'none') +
  theme(legend.position = "none")



### seeds ###

seedm1 = seedm
seedm1[2,] = seedm[4,]
seedm1[3,] = seedm[8,]
seedm1[4,] = seedm[7,]
seedm1[6,] = seedm[3,]
seedm1[7,] = seedm[6,]
seedm1[8,] = seedm[2,]

sdabundallm = seedm1

abund_table3 = seedm1
x = sort(seedfs$size)
meta_table3 = data.frame(x)
names(meta_table3) = "Fragment Size"

grouping_info = data.frame(x)
names(grouping_info) = "Fragment Size"
sol3 = metaMDS(abund_table,distance = "bray", k = 2, trymax = 50)
NMDS3=data.frame(x=sol3$point[,1],y=sol3$point[,2],Size=as.factor(grouping_info[,1]))


NMDS.mean3=aggregate(NMDS3[,1:2],list(group=NMDS3$Size),mean)

bio.fit3 <- envfit(sol3, abund_table3, perm = 999)
env.fit3 <- envfit(sol3, meta_table3, perm = 999)
df3<-scores(sol3,display=c("sites"))
df3<-data.frame(df3,Type=grouping_info[rownames(df),1])

a3 = colSums(seedm1)
b3 = names(a3[c(2,7,8,10,27)])

df_biofit3<-scores(bio.fit3,display=c("vectors"))
df_biofit3<-df_biofit3*vegan:::ordiArrowMul(df_biofit3)
df_biofit3<-as.data.frame(df_biofit3)
df_biofit3 = df_biofit3[rownames(df_biofit3) %in% b3,]

df_envfit3<-scores(env.fit3,display=c("vectors"))
df_envfit3<-df_envfit3*vegan:::ordiArrowMul(df_envfit3)
df_envfit3<-as.data.frame(df_envfit3)

cbPalette <- c("#0072B2","#0072B2","#0072B2","#0072B2", "#CC79A7","#CC79A7", "#CC79A7", "#CC79A7")


ggpe = ggplot(data=NMDS3,aes(x,y,colour=Size)) +
  ggtitle("Seeds") +
  geom_point(shape = 1, size = 12) +
  #geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2), size=1, linetype=2) +
  geom_segment(data=df_biofit3, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="#808080",alpha=0.5) +
  geom_text(data=as.data.frame(df_biofit3*1.1),aes(NMDS1, NMDS2, label = rownames(df_biofit3)),
            color="#808080",alpha=1) +
  geom_segment(data=df_envfit3, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="black", size = 0.5) +
  geom_text(data=as.data.frame(df_envfit3*1.15),aes(NMDS1, NMDS2, 
                                                  label = rownames(df_envfit3)),color="black",size = 4) +
  annotate("text",x=NMDS.mean3$x,y=NMDS.mean3$y,label=NMDS.mean3$group,size=4, col = cbPalette,fontface =2)
gg3 = ggpe + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = 'italic')) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_shape_discrete(solid=F) +
  scale_color_manual(values = cbPalette) +
  scale_x_continuous(limits = c(-0.8,0.8)) +
  scale_y_continuous(limits = c(-0.9,0.9), breaks = seq(-0.8,1,0.3)) +
  scale_size(guide = 'none') +
  theme(legend.position = "none")


multiplot(gg1,gg3)
