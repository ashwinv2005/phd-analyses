library(ggplot2)
library(ggthemes)
library(tidyverse)
library(vegan)
library(grid)
library(geosphere)
library(mgcv)
library(lme4)

theme_set(theme_tufte())

dissim = data.frame(rep(sggro1$groupfac,each = 555))
names(dissim)[1] = "com1"
dissim$com1 = as.character(dissim$com1)
dissim$com2 = rep(sggro1$groupfac,555)
dissim$size1 = rep(sggro1$size,each = 555)
dissim$size2 = rep(sggro1$size,555)
dissim$site1 = rep(sggro1$site,each = 555)
dissim$site2 = rep(sggro1$site,555)
dissim$location1 = rep(sggro1$location,each = 555)
dissim$location2 = rep(sggro1$location,555)
dissim$group1 = rep(sggro1$group,each = 555)
dissim$group2 = rep(sggro1$group,555)
dissim$plot1 = rep(sggro1$plot,each = 555)
dissim$plot2 = rep(sggro1$plot,555)

dissim = dissim[dissim$com1 != dissim$com2,]
dissim = dissim[dissim$plot1 == dissim$plot2,]

dissim$lat1 = dissim$long1 = dissim$lat2 = dissim$long2 = 0

for (i in 1:length(dissim$com1))
{
  dissim$lat1[i] = coordalt[coordalt$groupfac == dissim$com1[i],]$xs[1]
  dissim$long1[i] = coordalt[coordalt$groupfac == dissim$com1[i],]$ys[1]
  dissim$lat2[i] = coordalt[coordalt$groupfac == dissim$com2[i],]$xs[1]
  dissim$long2[i] = coordalt[coordalt$groupfac == dissim$com2[i],]$ys[1]
}

dissim$dist = 0

for (i in 1:length(dissim$com1))
{
  dissim$dist[i] = distVincentyEllipsoid(p1 = c(dissim$lat1[i],dissim$long1[i]), p2 = c(dissim$lat2[i],dissim$long2[i]))
}

f = rep(0,length(dissim$com1)/2)
ct = 0

dissim$plot = dissim$plot1

for (i in 1:length(dissim$com1))
{
  d = (dissim$plot == dissim$plot[i] & dissim$com2 == dissim$com1[i] & dissim$com1 == dissim$com2[i])
  x = 1:length(dissim$com1)
  x = x[d]
  if (!(i %in% f))
  {
    ct = ct + 1
    f[ct] = x
  }
  print(i)
}

dissim1 = dissim
dissim = dissim[-f,]

dissim$size = abs(dissim$size1-dissim$size2)

gfac = sggro1$groupfac
gplot = sggro1$plot

dissim$dis1 = dissim$dis2 = 0
l = 1:length(gfac)

for (i in 1:length(dissim$com1))
{
  x = l[((gfac == dissim$com1[i] | gfac == dissim$com2[i]) & gplot == dissim$plot[i])]
  dissim$dis1[i] = vegdist(sggro1m[x,], "euclidian")
  dissim$dis2[i] = vegdist(sggro2m[x,], "euclidian")
}

dissim$diff = dissim$dis1 - dissim$dis2

## without two species

gfac = sggro1$groupfac
gplot = sggro1$plot

dissim$dis1w = dissim$dis2w = 0
l = 1:length(gfac)

for (i in 1:length(dissim$com1))
{
  x = l[((gfac == dissim$com1[i] | gfac == dissim$com2[i]) & gplot == dissim$plot[i])]
  dissim$dis1w[i] = vegdist(sggro1m[x,-c(2,6)], "euclidian")
  dissim$dis2w[i] = vegdist(sggro2m[x,-c(2,6)], "euclidian")
}

dissim$diffw = dissim$dis1w - dissim$dis2w

### Bray-Curtis

gfac = sggro1$groupfac
gplot = sggro1$plot

dissim$bray1 = dissim$bray2 = 0
l = 1:length(gfac)

for (i in 1:length(dissim$com1))
{
  x = l[((gfac == dissim$com1[i] | gfac == dissim$com2[i]) & gplot == dissim$plot[i])]
  dissim$bray1[i] = vegdist(decostand(sggro1m[x,],"hell"), "bray")
  dissim$bray2[i] = vegdist(decostand(sggro2m[x,],"hell"), "bray")
}

dissim$bray = dissim$bray1 - dissim$bray2

### Bray without two species

gfac = sggro1$groupfac
gplot = sggro1$plot

dissim$bray1w = dissim$bray2w = 0
l = 1:length(gfac)

for (i in 1:length(dissim$com1))
{
  x = l[((gfac == dissim$com1[i] | gfac == dissim$com2[i]) & gplot == dissim$plot[i])]
  dissim$bray1w[i] = vegdist(decostand(sggro1m[x,-c(2,6)],"hell"), "bray")
  dissim$bray2w[i] = vegdist(decostand(sggro2m[x,-c(2,6)],"hell"), "bray")
}

dissim$brayw = dissim$bray1w - dissim$bray2w

### Standardized euclidian

gfac = sggro1$groupfac
gplot = sggro1$plot

dissim$euc1 = dissim$euc2 = 0
l = 1:length(gfac)

for (i in 1:length(dissim$com1))
{
  x = l[((gfac == dissim$com1[i] | gfac == dissim$com2[i]) & gplot == dissim$plot[i])]
  dissim$euc1[i] = vegdist(decostand(sggro1m[x,],"hell"), "euclidian")
  dissim$euc2[i] = vegdist(decostand(sggro2m[x,],"hell"), "euclidian")
}

dissim$euc = dissim$euc1 - dissim$euc2

### Standardized euc without two species

gfac = sggro1$groupfac
gplot = sggro1$plot

dissim$euc1w = dissim$euc2w = 0
l = 1:length(gfac)

for (i in 1:length(dissim$com1))
{
  x = l[((gfac == dissim$com1[i] | gfac == dissim$com2[i]) & gplot == dissim$plot[i])]
  dissim$euc1w[i] = vegdist(decostand(sggro1m[x,-c(2,6)],"hell"), "euclidian")
  dissim$euc2w[i] = vegdist(decostand(sggro2m[x,-c(2,6)],"hell"), "euclidian")
}

dissim$eucw = dissim$euc1w - dissim$euc2w

dissim1 = dissim

for (i in 1:length(dissim1$com1))
{
  if (!is.na(dissim1$bray1[i]))
  {
    if (dissim1$bray1[i] == max(na.omit(dissim$bray1)))
    {
      dissim1$bray[i] = NA
    }
  }
  if (!is.na(dissim1$bray1w[i]))
  {
    if (dissim1$bray1w[i] == max(na.omit(dissim$bray1w)))
    {
      dissim1$brayw[i] = NA
    }
  }
  if (dissim1$euc1[i] == max(na.omit(dissim$euc1)))
  {
    dissim1$euc[i] = NA
  }
  if (dissim1$euc1w[i] == max(na.omit(dissim$euc1w)))
  {
    dissim1$eucw[i] = NA
  }
}

dissim = dissim1

dissim$plot57 = 0
dissim$plot67 = 0
dissim[dissim$plot == "5" | dissim$plot == "7",]$plot57 = 1
dissim[dissim$plot == "6" | dissim$plot == "7",]$plot67 = 1


with(dissim[dissim$plot == "7",], plot(diff~dist))

rm(dissim1,a,gfac,gplot,i,l,x)

