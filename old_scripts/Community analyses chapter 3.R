library(ggplot2)
library(ggthemes)
library(tidyverse)
library(vegan)
library(grid)
library(viridis)

theme_set(theme_tufte())

abund_table2 = saabundallm
x = plots$size
meta_table2 = data.frame(x)
names(meta_table2) = "fragment size"

grouping_info = data.frame(x)
names(grouping_info) = "fragment size"
sol2 = metaMDS(abund_table2,distance = "bray", k = 2, trymax = 50)
NMDS2=data.frame(x=sol2$point[,1],y=sol2$point[,2],size=grouping_info[,1])


NMDS.mean2=aggregate(NMDS2[,1:2],list(group=NMDS2$size),mean)

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

#cbPalette <- c("#0072B2","#0072B2","#0072B2","#0072B2", "#CC79A7","#CC79A7", "#CC79A7", "#CC79A7")


ggp = ggplot(data=NMDS2,aes(x,y,colour=log(size))) +
  ggtitle("adults") +
  geom_point(shape = 16, size = 10, alpha = 0.6) +
  #geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2), size=1, linetype=2) +
  geom_segment(data=df_biofit2, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="#808080",alpha=0.5) +
  geom_text(data=as.data.frame(df_biofit2*1.1),aes(NMDS1, NMDS2, label = rownames(df_biofit2)),
            color="#808080",alpha=1) +
  geom_segment(data=df_envfit2*1.1, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="black", size = 0.5) +
  geom_text(data=as.data.frame(df_envfit2*1.25),aes(NMDS1, NMDS2, 
                                                    label = rownames(df_envfit2)),color="black",size = 4) 
  #annotate("text",x=NMDS.mean2$x,y=NMDS.mean2$y,label=NMDS.mean2$group,size=4,fontface =2)
ggp1 = ggp + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = 'italic')) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  theme(legend.title = element_blank()) +
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_shape_discrete(solid=F) +
  scale_colour_viridis(breaks = c(1,2,3,4,5), labels = c(3,10,25,50,150)) +
  scale_x_continuous(limits = c(-0.8,0.8)) +
  scale_y_continuous(limits = c(-0.8,1), breaks = seq(-0.8,1,0.3)) 


### Seeds ###

abund_table3 = sdabundallm
x = sort(seedfs$size)
meta_table3 = data.frame(x)
names(meta_table3) = "fragment size"

grouping_info = data.frame(x)
names(grouping_info) = "fragment size"
sol3 = metaMDS(abund_table3,distance = "bray", k = 3, trymax = 50)
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
df_envfit3<-df_envfit3*vegan:::ordiArrowMul(df_envfit3)
df_envfit3<-as.data.frame(df_envfit3)

#cbPalette <- c("#0072B2","#0072B2","#0072B2","#0072B2", "#CC79A7","#CC79A7", "#CC79A7", "#CC79A7")


ggp = ggplot(data=NMDS3,aes(x,y,colour=log(size))) +
  ggtitle("seeds") +
  geom_point(shape = 16, size = 10, alpha = 0.6) +
  #geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2), size=1, linetype=2) +
  geom_segment(data=df_biofit3, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="#808080",alpha=0.5) +
  geom_text(data=as.data.frame(df_biofit3*1.1),aes(NMDS1, NMDS2, label = rownames(df_biofit3)),
            color="#808080",alpha=1) +
  geom_segment(data=df_envfit3, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="black", size = 0.5) +
  geom_text(data=as.data.frame(df_envfit3*1.15),aes(NMDS1, NMDS2, 
                                                    label = rownames(df_envfit3)),color="black",size = 4) 
  #annotate("text",x=NMDS.mean3$x,y=NMDS.mean3$y,label=NMDS.mean3$group,size=4, col = cbPalette,fontface =2)
ggp2 = ggp + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = 'italic')) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  theme(legend.title = element_blank()) +
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_shape_discrete(solid=F) +
  scale_colour_viridis(breaks = c(1,2,3,4,5), labels = c(3,10,25,50,150)) +
  scale_x_continuous(limits = c(-0.8,0.8)) +
  scale_y_continuous(limits = c(-0.9,0.9), breaks = seq(-0.8,1,0.3)) 


### young saplings ###


abund_table = ysabundallm
x = plots$size
meta_table = data.frame(x)
names(meta_table) = "fragment size"



grouping_info = data.frame(x)
names(grouping_info) = "fragment size"
sol = metaMDS(abund_table,distance = "bray", k = 2, trymax = 50)
NMDS=data.frame(x=sol$point[,1],y=sol$point[,2],size=grouping_info[,1])


NMDS.mean=aggregate(NMDS[,1:2],list(group=NMDS$size),mean)

MDS_res=metaMDS(abund_table, distance = nmethod, k = 2, trymax = 50)
bio.fit <- envfit(MDS_res, abund_table, perm = 999)
env.fit <- envfit(MDS_res, meta_table, perm = 999)
df<-scores(MDS_res,display=c("sites"))
df<-data.frame(df,Type=grouping_info[rownames(df),1])

a = colSums(abundallm)
b = names(a[c(1,3:6,11,24,63)])

df_biofit<-scores(bio.fit,display=c("vectors"))
df_biofit<-df_biofit*vegan:::ordiArrowMul(df_biofit)
df_biofit<-as.data.frame(df_biofit)
df_biofit = df_biofit[rownames(df_biofit) %in% b,]

df_envfit<-scores(env.fit,display=c("vectors"))
df_envfit<-df_envfit*vegan:::ordiArrowMul(df_envfit)
df_envfit<-as.data.frame(df_envfit)

#cbPalette <- c("#0072B2","#0072B2","#0072B2","#0072B2", "#CC79A7","#CC79A7", "#CC79A7", "#CC79A7")

df_envfitx = df_envfit
df_envfitx[2] = -0.01
ggp = ggplot(data=NMDS,aes(x,y,colour=log(size))) +
  ggtitle("young saplings") +
  geom_point(shape = 16, size = 10, alpha = 0.6) +
  #geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2), size=1, linetype=2) +
  geom_segment(data=df_biofit, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="#808080",alpha=0.5) +
  geom_text(data=as.data.frame(df_biofit*1.1),aes(NMDS1, NMDS2, label = rownames(df_biofit)),
            color="#808080",alpha=1) +
  geom_segment(data=df_envfit, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="black", size = 0.5) +
  geom_text(data=as.data.frame(df_envfitx),aes(NMDS1, NMDS2, 
                                                  label = rownames(df_envfitx)),color="black",size = 4) 
  #annotate("text",x=NMDS.mean$x,y=NMDS.mean$y,label=NMDS.mean$group,size=4, col = cbPalette,fontface =2) 
ggp3 = ggp + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = 'italic')) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  theme(legend.title = element_blank()) +
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_shape_discrete(solid=F) +
  scale_colour_viridis(breaks = c(1,2,3,4,5), labels = c(3,10,25,50,150)) +
  scale_x_continuous(limits = c(-0.8,0.8), breaks = seq(-1,0.8,0.3)) +
  scale_y_continuous(limits = c(-0.8,1), breaks = seq(-0.8,1,0.3)) 


### old saplings ###

abund_table = osabundallm
x = plots$size
meta_table = data.frame(x)
names(meta_table) = "fragment size"



grouping_info = data.frame(x)
names(grouping_info) = "fragment size"
sol1 = metaMDS(abund_table,distance = "bray", k = 2, trymax = 50)
NMDS1=data.frame(x=sol1$point[,1],y=sol1$point[,2],size=grouping_info[,1])


NMDS.mean1=aggregate(NMDS1[,1:2],list(group=NMDS1$size),mean)

MDS_res1=metaMDS(abund_table, distance = nmethod, k = 2, trymax = 50)
bio.fit1 <- envfit(MDS_res1, abund_table, perm = 999)
env.fit1 <- envfit(MDS_res1, meta_table, perm = 999)
df1<-scores(MDS_res1,display=c("sites"))
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

#cbPalette <- c("#0072B2","#0072B2","#0072B2","#0072B2", "#CC79A7","#CC79A7", "#CC79A7", "#CC79A7")


ggp = ggplot(data=NMDS1,aes(x,y,colour=log(size)))  +
  ggtitle("older saplings") +
  geom_point(shape = 16, size = 10, alpha = 0.6) +
  #geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2), size=1, linetype=2) +
  geom_segment(data=df_biofit1, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="#808080",alpha=0.5) +
  geom_text(data=as.data.frame(df_biofit1*1.1),aes(NMDS1, NMDS2, label = rownames(df_biofit1)),
            color="#808080",alpha=1) +
  geom_segment(data=df_envfit1, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="black", size = 0.5) +
  geom_text(data=as.data.frame(df_envfit1*1.1),aes(NMDS1, NMDS2, 
                                                  label = rownames(df_envfit1)),color="black",size = 4) 
  #annotate("text",x=NMDS.mean$x,y=NMDS.mean$y,label=NMDS.mean$group,size=4, col = cbPalette,fontface =2) 
ggp4 = ggp + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = 'italic')) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  theme(legend.title = element_blank()) +
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_shape_discrete(solid=F) +
  scale_colour_viridis(breaks = c(1,2,3,4,5), labels = c(3,10,25,50,150)) +
  scale_x_continuous(limits = c(-0.8,0.8), breaks = seq(-1,0.8,0.3)) +
  scale_y_continuous(limits = c(-0.8,1), breaks = seq(-0.8,1,0.3)) 


### seedlings ###

abund_table = sgabundallm
x = plots$size
meta_table = data.frame(x)
names(meta_table) = "fragment size"



grouping_info = data.frame(x)
names(grouping_info) = "fragment size"
sol4 = metaMDS(abund_table,distance = "bray", k = 2, trymax = 50)
NMDS4=data.frame(x=sol4$point[,1],y=sol4$point[,2],size=grouping_info[,1])


NMDS.mean4=aggregate(NMDS4[,1:2],list(group=NMDS4$size),mean)

MDS_res4=metaMDS(abund_table, distance = nmethod, k = 2, trymax = 50)
bio.fit4 <- envfit(MDS_res4, abund_table, perm = 999)
env.fit4 <- envfit(MDS_res4, meta_table, perm = 999)
df4<-scores(MDS_res4,display=c("sites"))
df4<-data.frame(df4,Type=grouping_info[rownames(df4),1])

a4 = colSums(abundallm)
b4 = names(a4[c(1,3:6,11,24,63)])

df_biofit4<-scores(bio.fit4,display=c("vectors"))
df_biofit4<-df_biofit4*vegan:::ordiArrowMul(df_biofit4)
df_biofit4<-as.data.frame(df_biofit4)
df_biofit4 = df_biofit4[rownames(df_biofit4) %in% b4,]

df_envfit4<-scores(env.fit4,display=c("vectors"))
df_envfit4<-df_envfit4*vegan:::ordiArrowMul(df_envfit4)
df_envfit4<-as.data.frame(df_envfit4)

#cbPalette <- c("#0072B2","#0072B2","#0072B2","#0072B2", "#CC79A7","#CC79A7", "#CC79A7", "#CC79A7")

ggp = ggplot(data=NMDS4,aes(x,y,colour=log(size))) +
  ggtitle("seedlings") +
  geom_point(shape = 16, size = 10, alpha = 0.6) +
  #geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2), size=1, linetype=2) +
  geom_segment(data=df_biofit4, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="#808080",alpha=0.5) +
  geom_text(data=as.data.frame(df_biofit4*1.1),aes(NMDS1, NMDS2, label = rownames(df_biofit4)),
            color="#808080",alpha=1) +
  geom_segment(data=df_envfit4, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="black", size = 1) +
  geom_text(data=as.data.frame(df_envfit4*1.05),aes(NMDS1, NMDS2, 
                                               label = rownames(df_envfit4)),color="black",size = 4) 
#annotate("text",x=NMDS.mean$x,y=NMDS.mean$y,label=NMDS.mean$group,size=4, col = cbPalette,fontface =2) 
ggp5 = ggp + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = 'italic')) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  theme(legend.title = element_blank()) +
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_shape_discrete(solid=F) +
  scale_colour_viridis(breaks = c(1,2,3,4,5), labels = c(3,10,25,50,150)) +
  scale_x_continuous(limits = c(-0.8,0.9), breaks = seq(-1,0.8,0.3)) +
  scale_y_continuous(limits = c(-1.1,0.8), breaks = seq(-0.8,1,0.3)) 


### saplings ###

abund_table = slabundallm
x = plots$size
meta_table = data.frame(x)
names(meta_table) = "fragment size"



grouping_info = data.frame(x)
names(grouping_info) = "fragment size"
sol5 = metaMDS(abund_table,distance = "bray", k = 2, trymax = 50)
NMDS5=data.frame(x=sol5$point[,1],y=sol5$point[,2],size=grouping_info[,1])


NMDS.mean5=aggregate(NMDS5[,1:2],list(group=NMDS5$size),mean)

MDS_res5=metaMDS(abund_table, distance = nmethod, k = 2, trymax = 50)
bio.fit5 <- envfit(MDS_res5, abund_table, perm = 999)
env.fit5 <- envfit(MDS_res5, meta_table, perm = 999)
df5<-scores(MDS_res5,display=c("sites"))
df5<-data.frame(df5,Type=grouping_info[rownames(df5),1])

a5 = colSums(abundallm)
b5 = names(a5[c(1,3:6,11,24,63)])

df_biofit5<-scores(bio.fit5,display=c("vectors"))
df_biofit5<-df_biofit5*vegan:::ordiArrowMul(df_biofit5)
df_biofit5<-as.data.frame(df_biofit5)
df_biofit5 = df_biofit5[rownames(df_biofit5) %in% b5,]

df_envfit5<-scores(env.fit5,display=c("vectors"))
df_envfit5<-df_envfit5*vegan:::ordiArrowMul(df_envfit5)
df_envfit5<-as.data.frame(df_envfit5)

#cbPalette <- c("#0072B2","#0072B2","#0072B2","#0072B2", "#CC79A7","#CC79A7", "#CC79A7", "#CC79A7")

ggp = ggplot(data=NMDS5,aes(x,y,colour=log(size))) +
  ggtitle("saplings") +
  geom_point(shape = 16, size = 10, alpha = 0.6) +
  #geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2), size=1, linetype=2) +
  geom_segment(data=df_biofit5, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="#808080",alpha=0.5) +
  geom_text(data=as.data.frame(df_biofit5*1.1),aes(NMDS1, NMDS2, label = rownames(df_biofit5)),
            color="#808080",alpha=1) +
  geom_segment(data=df_envfit5, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="black", size = 0.5) +
  geom_text(data=as.data.frame(df_envfit5*1.05),aes(NMDS1, NMDS2, 
                                                    label = rownames(df_envfit5)),color="black",size = 4) 
#annotate("text",x=NMDS.mean$x,y=NMDS.mean$y,label=NMDS.mean$group,size=4, col = cbPalette,fontface =2) 
ggp6 = ggp + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = 'italic')) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  theme(legend.title = element_blank()) +
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_shape_discrete(solid=F) +
  scale_colour_viridis(breaks = c(1,2,3,4,5), labels = c(3,10,25,50,150)) +
  scale_x_continuous(limits = c(-0.8,0.6), breaks = seq(-1,0.8,0.3)) +
  scale_y_continuous(limits = c(-0.5,0.8), breaks = seq(-0.8,1,0.3)) 



### all together, with seeds ###

seabundallm = saabundallm
seabundallm[,] = 0
fl = 0
sl = 0
pnt = 1:27
lnt = 1:119

for (i in 1:length(names(sdabundallm)))
{
  if (names(sdabundallm)[i] %in% names(seabundallm))
  {
    seabundallm[,names(sdabundallm)[i] == names(seabundallm)] = sdabundallm[,i]
    fl = c(fl,i)
    sl = c(sl,lnt[names(sdabundallm)[i] == names(seabundallm)])
  }
}

fl = fl[-1]
sl = sl[-1]
fl = pnt[-fl]
sl = lnt[-sl]
set = sample(sl,length(fl),replace = F)

for (i in 1:length(fl))
{
  seabundallm[,set[i]] = sdabundallm[,fl[i]]
}


abund_table = rbind(saabundallm,seabundallm,sgabundallm,slabundallm)
x = rep(plots$size,4)
y = as.factor(rep(c("ad","sd","sg","sl"), each = 8))
z = specnumber(abund_table)
u = x*z
l = c(rep(1,8),rep(0,24))
m = c(rep(0,8),rep(1,8),rep(0,16))
n = c(rep(0,16),rep(1,8),rep(0,8))
o = c(rep(0,24),rep(1,8))
l = l*x
m = m*x
n = n*x
o = o*x
meta_table = data.frame(x,y,z,u,l,m,n,o)
names(meta_table) = c("fragsize","stage","spec","int","adults","seeds","youngsaplings","oldsaplings")



grouping_info = data.frame(x,y,z,u,l,m,n,o)
names(grouping_info) = c("fragsize","stage","spec","int","adults","seeds","youngsaplings","oldsaplings")
sol6 = metaMDS(abund_table,distance = "bray", k = 4, trymax = 50)
NMDS6=data.frame(x=sol6$point[,1],y=sol6$point[,2],size=grouping_info[,1],stage=grouping_info[,2],
                 adults=grouping_info[,5],seeds=grouping_info[,6],youngsaplings=grouping_info[,7],
                 oldsaplings=grouping_info[,8])


NMDS.mean6=aggregate(NMDS6[,1:2],list(group=NMDS6$size),mean)

MDS_res6=metaMDS(abund_table, distance = nmethod, k = 4, trymax = 50)
bio.fit6 <- envfit(MDS_res6, abund_table, perm = 999)
env.fit6 <- envfit(MDS_res6 ~ adults + youngsaplings + oldsaplings, meta_table, perm = 999)
df6<-scores(MDS_res6,display=c("sites"))
df6<-data.frame(df6,Type=grouping_info[rownames(df6),1])

a6 = colSums(abundallm)
b6 = names(a6[c(1,4:6,24)])

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
  geom_point(shape = 16, aes(size = log(size),colour=stage), alpha = 0.6) +
  #geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2), size=1, linetype=2) +
  geom_segment(data=df_biofit6, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="#808080",alpha=0.5) +
  geom_text(data=as.data.frame(df_biofit6*1.1),aes(NMDS1, NMDS2, label = rownames(df_biofit6)),
            color="#808080",alpha=1) +
  geom_segment(data=df_envfit6, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")), col = c(1,2,3,4), size = 0.5) +
  geom_text(data=as.data.frame(df_envfit6*1.1),aes(NMDS1, NMDS2, 
              label = c("fragment size","fragment size","fragment size","fragment size")),color="black",size = 4) 
#annotate("text",x=NMDS.mean$x,y=NMDS.mean$y,label=NMDS.mean$group,size=4, col = cbPalette,fontface =2) 
ggp7 = ggp + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = 'italic')) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  #theme(legend.title = element_blank()) +
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  #scale_shape_discrete(solid=F) +
  scale_size(name = "fragment size",breaks = c(1,2,3,4,5),labels = c(3,10,25,50,150)) +
  scale_colour_discrete(name = "life stage", breaks = c("ad","sd","sg","sl"), labels = c("adults","seeds","young saplings","old saplings (> 1cm girth)")) +
  scale_x_continuous(limits = c(-1.4,0.9), breaks = seq(-1,0.8,0.3)) +
  scale_y_continuous(limits = c(-1.8,0.9), breaks = seq(-0.8,1,0.3)) 


### all together, without seeds ###

abund_table = rbind(saabundallm,sgabundallm,slabundallm)
x = rep(plots$size,3)
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
env.fit6 <- envfit(MDS_res6 ~ stage + adults + youngsaplings + oldsaplings, meta_table, perm = 999)
df6<-scores(MDS_res6,display=c("sites"))
df6<-data.frame(df6,Type=grouping_info[rownames(df6),1])

a6 = colSums(abundallm)
b6 = names(a6[c(1,3:6,11,24,63)])

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
  geom_point(shape = 16, aes(size = log(size),colour=stage), alpha = 0.6) +
  #geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2), size=1, linetype=2) +
  geom_segment(data=df_biofit6, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),color="#808080",alpha=0.5) +
  geom_text(data=as.data.frame(df_biofit6*1.1),aes(NMDS1, NMDS2, label = rownames(df_biofit6)),
            color="#808080",alpha=1) +
  geom_segment(data=df_envfit6, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")), col = c("#56B4E9","#D55E00","#CC79A7"), size = 0.5) +
  geom_text(data=as.data.frame(df_envfit6*1.1),aes(NMDS1, NMDS2, 
                              label = c("fragment size","fragment size","fragment size")), 
                              col = c("#56B4E9","#D55E00","#CC79A7"),size = 4) 
#annotate("text",x=NMDS.mean$x,y=NMDS.mean$y,label=NMDS.mean$group,size=4, col = cbPalette,fontface =2) 
ggp7 = ggp + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = 'italic')) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  theme(legend.text = element_text(size = 8), legend.title = element_text(size = 8, face = 'bold')) +
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  #scale_shape_discrete(solid=F) +
  scale_size(name = "fragment size (ha)",breaks = c(1,2,3,4,5),labels = c(3,10,25,50,150)) +

  scale_colour_manual(values = c("#56B4E9","#D55E00","#CC79A7"),
                        name = "life stage", breaks = c("ad","sg","sl"), 
                        labels = c("adults","young saplings","old saplings (> 1cm girth)")) +
  scale_x_continuous(limits = c(-1.4,0.9), breaks = seq(-1,0.8,0.3)) +
  scale_y_continuous(limits = c(-1.8,0.9), breaks = seq(-0.8,1,0.3)) 




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

tiff('C:/Users/ashwinv/Desktop/a.tiff', units="in", width=7, height=6, res=1000)
grid_arrange_shared_legend(ggp7)
dev.off()
