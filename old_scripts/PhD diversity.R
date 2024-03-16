library(vegan)
data("BCI")

sgro = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/sgro.csv")
sggro1 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/sggro1.csv")
sggro2 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/sggro2.csv")

sloc = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/sloc.csv")
sgloc1 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/sgloc1.csv")
sgloc2 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/sgloc2.csv")

sfrag = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/sfrag.csv")
sgfrag1 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/sgfrag1.csv")
sgfrag2 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/sgfrag2.csv")

sgrom = sgro[,6:97]
slocm = sloc[,5:96]
sfragm = sfrag[,4:95]

sggro1m = sggro1[,10:115]
sgloc1m = sgloc1[,6:111]
sgfrag1m = sgfrag1[,5:110]

sggro2m = sggro2[,10:115]
sgloc2m = sgloc2[,6:111]
sgfrag2m = sgfrag2[,5:110]

##### Shannon diversity #######

sgro$shannon = diversity(sgrom,"shannon")
sloc$shannon = diversity(slocm,"shannon")
sfrag$shannon = diversity(sfragm,"shannon")

sggro1$shannon = diversity(sggro1m,"shannon")
sgloc1$shannon = diversity(sgloc1m,"shannon")
sgfrag1$shannon = diversity(sgfrag1m,"shannon")

sggro2$shannon = diversity(sggro2m,"shannon")
sgloc2$shannon = diversity(sgloc2m,"shannon")
sgfrag2$shannon = diversity(sgfrag2m,"shannon")

##### Simpson diversity #######

sgro$simpson = diversity(sgrom,"simpson")
sloc$simpson = diversity(slocm,"simpson")
sfrag$simpson = diversity(sfragm,"simpson")

sggro1$simpson = diversity(sggro1m,"simpson")
sgloc1$simpson = diversity(sgloc1m,"simpson")
sgfrag1$simpson = diversity(sgfrag1m,"simpson")

sggro2$simpson = diversity(sggro2m,"simpson")
sgloc2$simpson = diversity(sgloc2m,"simpson")
sgfrag2$simpson = diversity(sgfrag2m,"simpson")

##### Inverse Simpson diversity #######

sgro$invsimpson = diversity(sgrom,"invsimpson")
sloc$invsimpson = diversity(slocm,"invsimpson")
sfrag$invsimpson = diversity(sfragm,"invsimpson")

sggro1$invsimpson = diversity(sggro1m,"invsimpson")
sgloc1$invsimpson = diversity(sgloc1m,"invsimpson")
sgfrag1$invsimpson = diversity(sgfrag1m,"invsimpson")

sggro2$invsimpson = diversity(sggro2m,"invsimpson")
sgloc2$invsimpson = diversity(sgloc2m,"invsimpson")
sgfrag2$invsimpson = diversity(sgfrag2m,"invsimpson")

##### Species richness #######

sgro$rich = specnumber(sgrom)
sloc$rich = specnumber(slocm)
sfrag$rich = specnumber(sfragm)

sggro1$rich = specnumber(sggro1m)
sgloc1$rich = specnumber(sgloc1m)
sgfrag1$rich = specnumber(sgfrag1m)

sggro2$rich = specnumber(sggro2m)
sgloc2$rich = specnumber(sgloc2m)
sgfrag2$rich = specnumber(sgfrag2m)


##### Evenness ######

sgro$even = sgro$shannon/log(sgro$rich)
sloc$even = sloc$shannon/log(sloc$rich)
sfrag$even = sfrag$shannon/log(sfrag$rich)

sggro1$even = sggro1$shannon/log(sggro1$rich)
sgloc1$even = sgloc1$shannon/log(sgloc1$rich)
sgfrag1$even = sgfrag1$shannon/log(sgfrag1$rich)

sggro2$even = sggro2$shannon/log(sggro2$rich)
sgloc2$even = sgloc2$shannon/log(sgloc2$rich)
sgfrag2$even = sgfrag2$shannon/log(sgfrag2$rich)

###### Fisher's alpha #######

sgro$alpha = fisher.alpha(sgrom)
sloc$alpha = fisher.alpha(slocm)
sfrag$alpha = fisher.alpha(sfragm)

sggro1$alpha = fisher.alpha(sggro1m)
sgloc1$alpha = fisher.alpha(sgloc1m)
sgfrag1$alpha = fisher.alpha(sgfrag1m)

sggro2$alpha = fisher.alpha(sggro2m)
sgloc2$alpha = fisher.alpha(sgloc2m)
sgfrag2$alpha = fisher.alpha(sgfrag2m)

###### proportion diversity translation with shannon #######

for (i in 1:5)
{
  for (j in 1:111)
  {
    sggro1$divratio[(i-1)*111+j] = sggro1$shannon[(i-1)*111+j]/sgro$shannon[j]
    sggro2$divratio[(i-1)*111+j] = sggro2$shannon[(i-1)*111+j]/sggro1$shannon[(i-1)*111+j]
  }
}

for (i in 1:5)
{
  for (j in 1:37)
  {
    sgloc1$divratio[(i-1)*37+j] = sgloc1$shannon[(i-1)*37+j]/sloc$shannon[j]
    sgloc2$divratio[(i-1)*37+j] = sgloc2$shannon[(i-1)*37+j]/sgloc1$shannon[(i-1)*37+j]
  }
}

for (i in 1:5)
{
  for (j in 1:21)
  {
    sgfrag1$divratio[(i-1)*21+j] = sgfrag1$shannon[(i-1)*21+j]/sfrag$shannon[j]
    sgfrag2$divratio[(i-1)*21+j] = sgfrag2$shannon[(i-1)*21+j]/sgfrag1$shannon[(i-1)*21+j]
  }
}

###### seeds with error bars shannon #######

a = summarySE(sgro, measurevar="shannon", groupvars=c("site"))
a$site = as.character(a$site)
sfrag$shannongro = 0
sfrag$shannongroSE = 0

for (i in 1:21)
{
  sfrag[sfrag$site == a$site[i],]$shannongro[1] = a$shannon[i]
  sfrag[sfrag$site == a$site[i],]$shannongroSE[1] = a$se[i]
}

b = summarySE(sloc, measurevar="shannon", groupvars=c("site"))
b$site = as.character(b$site)
sfrag$shannonloc = 0
sfrag$shannonlocSE = 0

for (i in 1:21)
{
  sfrag[sfrag$site == b$site[i],]$shannonloc[1] = b$shannon[i]
  sfrag[sfrag$site == b$site[i],]$shannonlocSE[1] = b$se[i]
}

###### seeds with error bars simpson #######

a = summarySE(sgro, measurevar="simpson", groupvars=c("site"))
a$site = as.character(a$site)
sfrag$simpsongro = 0
sfrag$simpsongroSE = 0

for (i in 1:21)
{
  sfrag[sfrag$site == a$site[i],]$simpsongro[1] = a$simpson[i]
  sfrag[sfrag$site == a$site[i],]$simpsongroSE[1] = a$se[i]
}

b = summarySE(sloc, measurevar="simpson", groupvars=c("site"))
b$site = as.character(b$site)
sfrag$simpsonloc = 0
sfrag$simpsonlocSE = 0

for (i in 1:21)
{
  sfrag[sfrag$site == b$site[i],]$simpsonloc[1] = b$simpson[i]
  sfrag[sfrag$site == b$site[i],]$simpsonlocSE[1] = b$se[i]
}

###### seeds with error bars inverse simpson #######

a = summarySE(sgro, measurevar="invsimpson", groupvars=c("site"))
a$site = as.character(a$site)
sfrag$invsimpsongro = 0
sfrag$invsimpsongroSE = 0

for (i in 1:21)
{
  sfrag[sfrag$site == a$site[i],]$invsimpsongro[1] = a$invsimpson[i]
  sfrag[sfrag$site == a$site[i],]$invsimpsongroSE[1] = a$se[i]
}

b = summarySE(sloc, measurevar="invsimpson", groupvars=c("site"))
b$site = as.character(b$site)
sfrag$invsimpsonloc = 0
sfrag$invsimpsonlocSE = 0

for (i in 1:21)
{
  sfrag[sfrag$site == b$site[i],]$invsimpsonloc[1] = b$invsimpson[i]
  sfrag[sfrag$site == b$site[i],]$invsimpsonlocSE[1] = b$se[i]
}

###### seeds with error bars rich #######

a = summarySE(sgro, measurevar="rich", groupvars=c("site"))
a$site = as.character(a$site)
sfrag$richgro = 0
sfrag$richgroSE = 0

for (i in 1:21)
{
  sfrag[sfrag$site == a$site[i],]$richgro[1] = a$rich[i]
  sfrag[sfrag$site == a$site[i],]$richgroSE[1] = a$se[i]
}

b = summarySE(sloc, measurevar="rich", groupvars=c("site"))
b$site = as.character(b$site)
sfrag$richloc = 0
sfrag$richlocSE = 0

for (i in 1:21)
{
  sfrag[sfrag$site == b$site[i],]$richloc[1] = b$rich[i]
  sfrag[sfrag$site == b$site[i],]$richlocSE[1] = b$se[i]
}

###### seeds with error bars even #######

a = summarySE(sgro, measurevar="even", groupvars=c("site"))
a$site = as.character(a$site)
sfrag$evengro = 0
sfrag$evengroSE = 0

for (i in 1:21)
{
  sfrag[sfrag$site == a$site[i],]$evengro[1] = a$even[i]
  sfrag[sfrag$site == a$site[i],]$evengroSE[1] = a$se[i]
}

b = summarySE(sloc, measurevar="even", groupvars=c("site"))
b$site = as.character(b$site)
sfrag$evenloc = 0
sfrag$evenlocSE = 0

for (i in 1:21)
{
  sfrag[sfrag$site == b$site[i],]$evenloc[1] = b$even[i]
  sfrag[sfrag$site == b$site[i],]$evenlocSE[1] = b$se[i]
}

###### seeds with error bars alpha #######

a = summarySE(sgro, measurevar="alpha", groupvars=c("site"))
a$site = as.character(a$site)
sfrag$alphagro = 0
sfrag$alphagroSE = 0

for (i in 1:21)
{
  sfrag[sfrag$site == a$site[i],]$alphagro[1] = a$alpha[i]
  sfrag[sfrag$site == a$site[i],]$alphagroSE[1] = a$se[i]
}

b = summarySE(sloc, measurevar="alpha", groupvars=c("site"))
b$site = as.character(b$site)
sfrag$alphaloc = 0
sfrag$alphalocSE = 0

for (i in 1:21)
{
  sfrag[sfrag$site == b$site[i],]$alphaloc[1] = b$alpha[i]
  sfrag[sfrag$site == b$site[i],]$alphalocSE[1] = b$se[i]
}

###### seedlings census 1 with error bars shannon #########

a = summarySE(sggro1, measurevar="shannon", groupvars=c("site","plot"))
a$site = as.character(a$site)
sgfrag1$shannongro = 0
sgfrag1$shannongroSE = 0

for (i in 1:105)
{
  sgfrag1[sgfrag1$site == a$site[i] & sgfrag1$plot == a$plot[i],]$shannongro[1] = a$shannon[i]
  sgfrag1[sgfrag1$site == a$site[i] & sgfrag1$plot == a$plot[i],]$shannongroSE[1] = a$se[i]
}

b = summarySE(sgloc1, measurevar="shannon", groupvars=c("site","plot"))
b$site = as.character(b$site)
sgfrag1$shannonloc = 0
sgfrag1$shannonlocSE = 0

for (i in 1:105)
{
  sgfrag1[sgfrag1$site == b$site[i] & sgfrag1$plot == b$plot[i],]$shannonloc[1] = b$shannon[i]
  sgfrag1[sgfrag1$site == b$site[i] & sgfrag1$plot == b$plot[i],]$shannonlocSE[1] = b$se[i]
}

###### seedlings census 1 with error bars simpson #########

a = summarySE(sggro1, measurevar="simpson", groupvars=c("site","plot"))
a$site = as.character(a$site)
sgfrag1$simpsongro = 0
sgfrag1$simpsongroSE = 0

for (i in 1:105)
{
  sgfrag1[sgfrag1$site == a$site[i] & sgfrag1$plot == a$plot[i],]$simpsongro[1] = a$simpson[i]
  sgfrag1[sgfrag1$site == a$site[i] & sgfrag1$plot == a$plot[i],]$simpsongroSE[1] = a$se[i]
}

b = summarySE(sgloc1, measurevar="simpson", groupvars=c("site","plot"))
b$site = as.character(b$site)
sgfrag1$simpsonloc = 0
sgfrag1$simpsonlocSE = 0

for (i in 1:105)
{
  sgfrag1[sgfrag1$site == b$site[i] & sgfrag1$plot == b$plot[i],]$simpsonloc[1] = b$simpson[i]
  sgfrag1[sgfrag1$site == b$site[i] & sgfrag1$plot == b$plot[i],]$simpsonlocSE[1] = b$se[i]
}

###### seedlings census 1 with error bars inverse simpson #########

a = summarySE(sggro1, measurevar="invsimpson", groupvars=c("site","plot"))
a$site = as.character(a$site)
sgfrag1$invsimpsongro = 0
sgfrag1$invsimpsongroSE = 0

for (i in 1:105)
{
  sgfrag1[sgfrag1$site == a$site[i] & sgfrag1$plot == a$plot[i],]$invsimpsongro[1] = a$invsimpson[i]
  sgfrag1[sgfrag1$site == a$site[i] & sgfrag1$plot == a$plot[i],]$invsimpsongroSE[1] = a$se[i]
}

b = summarySE(sgloc1, measurevar="invsimpson", groupvars=c("site","plot"))
b$site = as.character(b$site)
sgfrag1$invsimpsonloc = 0
sgfrag1$invsimpsonlocSE = 0

for (i in 1:105)
{
  sgfrag1[sgfrag1$site == b$site[i] & sgfrag1$plot == b$plot[i],]$invsimpsonloc[1] = b$invsimpson[i]
  sgfrag1[sgfrag1$site == b$site[i] & sgfrag1$plot == b$plot[i],]$invsimpsonlocSE[1] = b$se[i]
}

###### seedlings census 1 with error bars rich #########

a = summarySE(sggro1, measurevar="rich", groupvars=c("site","plot"))
a$site = as.character(a$site)
sgfrag1$richgro = 0
sgfrag1$richgroSE = 0

for (i in 1:105)
{
  sgfrag1[sgfrag1$site == a$site[i] & sgfrag1$plot == a$plot[i],]$richgro[1] = a$rich[i]
  sgfrag1[sgfrag1$site == a$site[i] & sgfrag1$plot == a$plot[i],]$richgroSE[1] = a$se[i]
}

b = summarySE(sgloc1, measurevar="rich", groupvars=c("site","plot"))
b$site = as.character(b$site)
sgfrag1$richloc = 0
sgfrag1$richlocSE = 0

for (i in 1:105)
{
  sgfrag1[sgfrag1$site == b$site[i] & sgfrag1$plot == b$plot[i],]$richloc[1] = b$rich[i]
  sgfrag1[sgfrag1$site == b$site[i] & sgfrag1$plot == b$plot[i],]$richlocSE[1] = b$se[i]
}

###### seedlings census 1 with error bars even #########

a = summarySE(sggro1, measurevar="even", groupvars=c("site","plot"))
a$site = as.character(a$site)
sgfrag1$evengro = 0
sgfrag1$evengroSE = 0

for (i in 1:105)
{
  sgfrag1[sgfrag1$site == a$site[i] & sgfrag1$plot == a$plot[i],]$evengro[1] = a$even[i]
  sgfrag1[sgfrag1$site == a$site[i] & sgfrag1$plot == a$plot[i],]$evengroSE[1] = a$se[i]
}

b = summarySE(sgloc1, measurevar="even", groupvars=c("site","plot"))
b$site = as.character(b$site)
sgfrag1$evenloc = 0
sgfrag1$evenlocSE = 0

for (i in 1:105)
{
  sgfrag1[sgfrag1$site == b$site[i] & sgfrag1$plot == b$plot[i],]$evenloc[1] = b$even[i]
  sgfrag1[sgfrag1$site == b$site[i] & sgfrag1$plot == b$plot[i],]$evenlocSE[1] = b$se[i]
}

###### seedlings census 1 with error bars alpha #########

a = summarySE(sggro1, measurevar="alpha", groupvars=c("site","plot"))
a$site = as.character(a$site)
sgfrag1$alphagro = 0
sgfrag1$alphagroSE = 0

for (i in 1:105)
{
  sgfrag1[sgfrag1$site == a$site[i] & sgfrag1$plot == a$plot[i],]$alphagro[1] = a$alpha[i]
  sgfrag1[sgfrag1$site == a$site[i] & sgfrag1$plot == a$plot[i],]$alphagroSE[1] = a$se[i]
}

b = summarySE(sgloc1, measurevar="alpha", groupvars=c("site","plot"))
b$site = as.character(b$site)
sgfrag1$alphaloc = 0
sgfrag1$alphalocSE = 0

for (i in 1:105)
{
  sgfrag1[sgfrag1$site == b$site[i] & sgfrag1$plot == b$plot[i],]$alphaloc[1] = b$alpha[i]
  sgfrag1[sgfrag1$site == b$site[i] & sgfrag1$plot == b$plot[i],]$alphalocSE[1] = b$se[i]
}


###### seedlings census 1 with error bars divratio #########

a = summarySE(sggro1, measurevar="divratio", groupvars=c("site","plot"))
a$site = as.character(a$site)
sgfrag1$divratiogro = 0
sgfrag1$divratiogroSE = 0

for (i in 1:105)
{
  sgfrag1[sgfrag1$site == a$site[i] & sgfrag1$plot == a$plot[i],]$divratiogro[1] = a$divratio[i]
  sgfrag1[sgfrag1$site == a$site[i] & sgfrag1$plot == a$plot[i],]$divratiogroSE[1] = a$se[i]
}

b = summarySE(sgloc1, measurevar="divratio", groupvars=c("site","plot"))
b$site = as.character(b$site)
sgfrag1$divratioloc = 0
sgfrag1$divratiolocSE = 0

for (i in 1:105)
{
  sgfrag1[sgfrag1$site == b$site[i] & sgfrag1$plot == b$plot[i],]$divratioloc[1] = b$divratio[i]
  sgfrag1[sgfrag1$site == b$site[i] & sgfrag1$plot == b$plot[i],]$divratiolocSE[1] = b$se[i]
}


###### seedlings census 2 with error bars shannon #########

a = summarySE(sggro2, measurevar="shannon", groupvars=c("site","plot"))
a$site = as.character(a$site)
sgfrag2$shannongro = 0
sgfrag2$shannongroSE = 0

for (i in 1:105)
{
  sgfrag2[sgfrag2$site == a$site[i] & sgfrag2$plot == a$plot[i],]$shannongro[1] = a$shannon[i]
  sgfrag2[sgfrag2$site == a$site[i] & sgfrag2$plot == a$plot[i],]$shannongroSE[1] = a$se[i]
}

b = summarySE(sgloc2, measurevar="shannon", groupvars=c("site","plot"))
b$site = as.character(b$site)
sgfrag2$shannonloc = 0
sgfrag2$shannonlocSE = 0

for (i in 1:105)
{
  sgfrag2[sgfrag2$site == b$site[i] & sgfrag2$plot == b$plot[i],]$shannonloc[1] = b$shannon[i]
  sgfrag2[sgfrag2$site == b$site[i] & sgfrag2$plot == b$plot[i],]$shannonlocSE[1] = b$se[i]
}

###### seedlings census 2 with error bars simpson #########

a = summarySE(sggro2, measurevar="simpson", groupvars=c("site","plot"))
a$site = as.character(a$site)
sgfrag2$simpsongro = 0
sgfrag2$simpsongroSE = 0

for (i in 1:105)
{
  sgfrag2[sgfrag2$site == a$site[i] & sgfrag2$plot == a$plot[i],]$simpsongro[1] = a$simpson[i]
  sgfrag2[sgfrag2$site == a$site[i] & sgfrag2$plot == a$plot[i],]$simpsongroSE[1] = a$se[i]
}

b = summarySE(sgloc2, measurevar="simpson", groupvars=c("site","plot"))
b$site = as.character(b$site)
sgfrag2$simpsonloc = 0
sgfrag2$simpsonlocSE = 0

for (i in 1:105)
{
  sgfrag2[sgfrag2$site == b$site[i] & sgfrag2$plot == b$plot[i],]$simpsonloc[1] = b$simpson[i]
  sgfrag2[sgfrag2$site == b$site[i] & sgfrag2$plot == b$plot[i],]$simpsonlocSE[1] = b$se[i]
}

###### seedlings census 2 with error bars inverse simpson #########

a = summarySE(sggro2, measurevar="invsimpson", groupvars=c("site","plot"))
a$site = as.character(a$site)
sgfrag2$invsimpsongro = 0
sgfrag2$invsimpsongroSE = 0

for (i in 1:105)
{
  sgfrag2[sgfrag2$site == a$site[i] & sgfrag2$plot == a$plot[i],]$invsimpsongro[1] = a$invsimpson[i]
  sgfrag2[sgfrag2$site == a$site[i] & sgfrag2$plot == a$plot[i],]$invsimpsongroSE[1] = a$se[i]
}

b = summarySE(sgloc2, measurevar="invsimpson", groupvars=c("site","plot"))
b$site = as.character(b$site)
sgfrag2$invsimpsonloc = 0
sgfrag2$invsimpsonlocSE = 0

for (i in 1:105)
{
  sgfrag2[sgfrag2$site == b$site[i] & sgfrag2$plot == b$plot[i],]$invsimpsonloc[1] = b$invsimpson[i]
  sgfrag2[sgfrag2$site == b$site[i] & sgfrag2$plot == b$plot[i],]$invsimpsonlocSE[1] = b$se[i]
}

###### seedlings census 2 with error bars rich #########

a = summarySE(sggro2, measurevar="rich", groupvars=c("site","plot"))
a$site = as.character(a$site)
sgfrag2$richgro = 0
sgfrag2$richgroSE = 0

for (i in 1:105)
{
  sgfrag2[sgfrag2$site == a$site[i] & sgfrag2$plot == a$plot[i],]$richgro[1] = a$rich[i]
  sgfrag2[sgfrag2$site == a$site[i] & sgfrag2$plot == a$plot[i],]$richgroSE[1] = a$se[i]
}

b = summarySE(sgloc2, measurevar="rich", groupvars=c("site","plot"))
b$site = as.character(b$site)
sgfrag2$richloc = 0
sgfrag2$richlocSE = 0

for (i in 1:105)
{
  sgfrag2[sgfrag2$site == b$site[i] & sgfrag2$plot == b$plot[i],]$richloc[1] = b$rich[i]
  sgfrag2[sgfrag2$site == b$site[i] & sgfrag2$plot == b$plot[i],]$richlocSE[1] = b$se[i]
}

###### seedlings census 2 with error bars even #########

a = summarySE(sggro2, measurevar="even", groupvars=c("site","plot"))
a$site = as.character(a$site)
sgfrag2$evengro = 0
sgfrag2$evengroSE = 0

for (i in 1:105)
{
  sgfrag2[sgfrag2$site == a$site[i] & sgfrag2$plot == a$plot[i],]$evengro[1] = a$even[i]
  sgfrag2[sgfrag2$site == a$site[i] & sgfrag2$plot == a$plot[i],]$evengroSE[1] = a$se[i]
}

b = summarySE(sgloc2, measurevar="even", groupvars=c("site","plot"))
b$site = as.character(b$site)
sgfrag2$evenloc = 0
sgfrag2$evenlocSE = 0

for (i in 1:105)
{
  sgfrag2[sgfrag2$site == b$site[i] & sgfrag2$plot == b$plot[i],]$evenloc[1] = b$even[i]
  sgfrag2[sgfrag2$site == b$site[i] & sgfrag2$plot == b$plot[i],]$evenlocSE[1] = b$se[i]
}

###### seedlings census 2 with error bars alpha #########

a = summarySE(sggro2, measurevar="alpha", groupvars=c("site","plot"))
a$site = as.character(a$site)
sgfrag2$alphagro = 0
sgfrag2$alphagroSE = 0

for (i in 1:105)
{
  sgfrag2[sgfrag2$site == a$site[i] & sgfrag2$plot == a$plot[i],]$alphagro[1] = a$alpha[i]
  sgfrag2[sgfrag2$site == a$site[i] & sgfrag2$plot == a$plot[i],]$alphagroSE[1] = a$se[i]
}

b = summarySE(sgloc2, measurevar="alpha", groupvars=c("site","plot"))
b$site = as.character(b$site)
sgfrag2$alphaloc = 0
sgfrag2$alphalocSE = 0

for (i in 1:105)
{
  sgfrag2[sgfrag2$site == b$site[i] & sgfrag2$plot == b$plot[i],]$alphaloc[1] = b$alpha[i]
  sgfrag2[sgfrag2$site == b$site[i] & sgfrag2$plot == b$plot[i],]$alphalocSE[1] = b$se[i]
}


###### seedlings census 2 with error bars divratio, also for misc changes #########

a = summarySE(sggro2, measurevar="shannonsgsgtrans", groupvars=c("site","plot"))
a$site = as.character(a$site)
sgfrag2$sgsggro = 0
sgfrag2$sgsggroSE = 0

for (i in 1:105)
{
  sgfrag2[sgfrag2$site == a$site[i] & sgfrag2$plot == a$plot[i],]$sgsggro[1] = a$shannonsgsgtrans[i]
  sgfrag2[sgfrag2$site == a$site[i] & sgfrag2$plot == a$plot[i],]$sgsggroSE[1] = a$se[i]
}

b = summarySE(sgloc2, measurevar="shannonsgsgtrans", groupvars=c("site","plot"))
b$site = as.character(b$site)
sgfrag2$sgsgloc = 0
sgfrag2$sgsglocSE = 0

for (i in 1:105)
{
  sgfrag2[sgfrag2$site == b$site[i] & sgfrag2$plot == b$plot[i],]$sgsgloc[1] = b$shannonsgsgtrans[i]
  sgfrag2[sgfrag2$site == b$site[i] & sgfrag2$plot == b$plot[i],]$sgsglocSE[1] = b$se[i]
}

head(sgfrag2)

####### plotting #########

head(sgfrag1)

sgfrag1$plot = as.factor(sgfrag1$plot)
sgfrag2$plot = as.factor(sgfrag2$plot)

pd = position_dodge(.04)

library(ggplot2)

ggp = ggplot(sgfrag2, aes(x = size, y = sgsggro, group = plot, shape = plot, col = plot))  +
  #facet_grid(name ~ ., scale="free_y")+
  geom_errorbar(aes(ymin=sgsggro-sgsggroSE, ymax=sgsggro+sgsggroSE, col = plot), position = pd, width=0.1, size = 0.1) +
  geom_line(position = pd, size = 0.6) +
  geom_point(position = pd, size = 2) +
  xlab("Fragment size") +
  ylab("First seedling seedling trans group") +
  #ylab(expression(paste(Seed~arrival~density~(no.~of~seeds~m^-2)))) +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 16), axis.text.x = element_text(size = 16), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 16), axis.text.y = element_text(size = 16)) +
  ##opts(plot.title = theme_text(vjust = 1.5, size = 18, face = "bold"))+
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(legend.justification=c(1,1), legend.position=c(1,0.99)) +
  #scale_x_continuous(limits = c(0,350), breaks = c(0,31,59,90,120,151,181,212,243,273,304,334),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  theme(strip.text.y = element_text(size = 12, angle = 0)) +
  scale_x_log10()+
  scale_shape_manual(values = c(15,16,24,17,18), name="",
                     breaks=c("1","2","5","6","7"),
                     labels=c("C1", "C2", "F", "I", "FI"))+
  scale_color_manual(values = c(2,3,4,6,9), name="",
                     breaks=c("1","2","5","6","7"),
                     labels=c("C1", "C2", "F", "I", "FI"))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))

head(sgfrag2)

forcompsgro = rbind(sgro,sgro,sgro,sgro,sgro)
forcompsloc = rbind(sloc,sloc,sloc,sloc,sloc)
forcompsfrag = rbind(sfrag,sfrag,sfrag,sfrag,sfrag)

sggro1$shannonsdsgtrans = sggro1$shannon - forcompsgro$shannon
sgloc1$shannonsdsgtrans = sgloc1$shannon - forcompsloc$shannon
sgfrag1$shannonsdsgtrans = sgfrag1$shannon - forcompsfrag$shannon

sggro2$shannonsdsgtrans = sggro2$shannon - forcompsgro$shannon
sgloc2$shannonsdsgtrans = sgloc2$shannon - forcompsloc$shannon
sgfrag2$shannonsdsgtrans = sgfrag2$shannon - forcompsfrag$shannon

sggro2$shannonsgsgtrans = sggro2$shannon - sggro1$shannon
sgloc2$shannonsgsgtrans = sgloc2$shannon - sgloc1$shannon
sgfrag2$shannonsgsgtrans = sgfrag2$shannon - sgfrag1$shannon

########### Beta diversity ##############

data(sipoo)
head(sggro1m)

sgloc1$siteplot = paste(as.character(sgloc1$site),as.character(sgloc1$plot),sep = "-")
sgloc2$siteplot = paste(as.character(sgloc2$site),as.character(sgloc2$plot),sep = "-")
sloc$siteplot = paste(as.character(sloc$site),as.character(sloc$plot),sep = "-")

sggro1$siteplot = paste(as.character(sggro1$site),as.character(sggro1$plot),sep = "-")
sggro2$siteplot = paste(as.character(sggro2$site),as.character(sggro2$plot),sep = "-")
sgro$siteplot = paste(as.character(sgro$site),as.character(sgro$plot),sep = "-")

plot(sgfrag1$size[1:21], ylim = c(0,150))


######## To add first category ############

sgro$group1 = " "
sgro[sgro$size < 6,]$group1 = "a"
sgro[sgro$size >= 6 & sgro$size < 15,]$group1 = "b"
sgro[sgro$size >= 15 & sgro$size < 55,]$group1 = "c"
sgro[sgro$size >= 55,]$group1 = "d"

sggro1$group1 = " "
sggro1[sggro1$size < 6,]$group1 = "a"
sggro1[sggro1$size >= 6 & sggro1$size < 15,]$group1 = "b"
sggro1[sggro1$size >= 15 & sggro1$size < 55,]$group1 = "c"
sggro1[sggro1$size >= 55,]$group1 = "d"

sggro2$group1 = sggro1$group1

sloc$group1 = " "
sloc[sloc$size < 6,]$group1 = "a"
sloc[sloc$size >= 6 & sloc$size < 15,]$group1 = "b"
sloc[sloc$size >= 15 & sloc$size < 55,]$group1 = "c"
sloc[sloc$size >= 55,]$group1 = "d"

sgloc1$group1 = " "
sgloc1[sgloc1$size < 6,]$group1 = "a"
sgloc1[sgloc1$size >= 6 & sgloc1$size < 15,]$group1 = "b"
sgloc1[sgloc1$size >= 15 & sgloc1$size < 55,]$group1 = "c"
sgloc1[sgloc1$size >= 55,]$group1 = "d"

sgloc2$group1 = sgloc1$group1

sfrag$group1 = " "
sfrag[sfrag$size < 6,]$group1 = "a"
sfrag[sfrag$size >= 6 & sfrag$size < 15,]$group1 = "b"
sfrag[sfrag$size >= 15 & sfrag$size < 55,]$group1 = "c"
sfrag[sfrag$size >= 55,]$group1 = "d"

sgfrag1$group1 = " "
sgfrag1[sgfrag1$size < 6,]$group1 = "a"
sgfrag1[sgfrag1$size >= 6 & sgfrag1$size < 15,]$group1 = "b"
sgfrag1[sgfrag1$size >= 15 & sgfrag1$size < 55,]$group1 = "c"
sgfrag1[sgfrag1$size >= 55,]$group1 = "d"

sgfrag2$group1 = sgfrag1$group1

########## To add second category #########

sgro$group2 = " "
sgro[sgro$size < 15,]$group2 = "a"
sgro[sgro$size >= 15 & sgro$size < 55,]$group2 = "b"
sgro[sgro$size >= 55,]$group2 = "c"

sggro1$group2 = " "
sggro1[sggro1$size < 15,]$group2 = "a"
sggro1[sggro1$size >= 15 & sggro1$size < 55,]$group2 = "b"
sggro1[sggro1$size >= 55,]$group2 = "c"

sggro2$group2 = sggro1$group2

sloc$group2 = " "
sloc[sloc$size < 15,]$group2 = "a"
sloc[sloc$size >= 15 & sloc$size < 55,]$group2 = "b"
sloc[sloc$size >= 55,]$group2 = "c"

sgloc1$group2 = " "
sgloc1[sgloc1$size < 15,]$group2 = "a"
sgloc1[sgloc1$size >= 15 & sgloc1$size < 55,]$group2 = "b"
sgloc1[sgloc1$size >= 55,]$group2 = "c"

sgloc2$group2 = sgloc1$group2

sfrag$group2 = " "
sfrag[sfrag$size < 15,]$group2 = "a"
sfrag[sfrag$size >= 15 & sfrag$size < 55,]$group2 = "b"
sfrag[sfrag$size >= 55,]$group2 = "c"

sgfrag1$group2 = " "
sgfrag1[sgfrag1$size < 15,]$group2 = "a"
sgfrag1[sgfrag1$size >= 15 & sgfrag1$size < 55,]$group2 = "b"
sgfrag1[sgfrag1$size >= 55,]$group2 = "c"

sgfrag2$group2 = sgfrag1$group2

########## To add third category #########

sgro$group3 = " "
sgro[sgro$size < 15,]$group3 = "a"
sgro[sgro$size >= 15,]$group3 = "b"

sggro1$group3 = " "
sggro1[sggro1$size < 15,]$group3 = "a"
sggro1[sggro1$size >= 15,]$group3 = "b"

sggro2$group3 = sggro1$group3

sloc$group3 = " "
sloc[sloc$size < 15,]$group3 = "a"
sloc[sloc$size >= 15,]$group3 = "b"

sgloc1$group3 = " "
sgloc1[sgloc1$size < 15,]$group3 = "a"
sgloc1[sgloc1$size >= 15,]$group3 = "b"

sgloc2$group3 = sgloc1$group3

sfrag$group3 = " "
sfrag[sfrag$size < 15,]$group3 = "a"
sfrag[sfrag$size >= 15,]$group3 = "b"

sgfrag1$group3 = " "
sgfrag1[sgfrag1$size < 15,]$group3 = "a"
sgfrag1[sgfrag1$size >= 15,]$group3 = "b"

sgfrag2$group3 = sgfrag1$group3

######## Create new category data frames ###############

head(sgfrag1)
head(ling1cat1)

### ling1 #######

a = summarySE(data = sggro1, measurevar = "shannonsdsgtrans", groupvars = c("group1","plot"))

ling1cat1$sdsggro = a$shannonsdsgtrans
ling1cat1$sdsggroSE = a$se

a = summarySE(data = sggro1, measurevar = "shannonsdsgtrans", groupvars = c("group2","plot"))

ling1cat2$sdsggro = a$shannonsdsgtrans
ling1cat2$sdsggroSE = a$se

a = summarySE(data = sggro1, measurevar = "shannonsdsgtrans", groupvars = c("group3","plot"))

ling1cat3$sdsggro = a$shannonsdsgtrans
ling1cat3$sdsggroSE = a$se

a = summarySE(data = sgloc1, measurevar = "shannonsdsgtrans", groupvars = c("group1","plot"))

ling1cat1$sdsgloc = a$shannonsdsgtrans
ling1cat1$sdsglocSE = a$se

a = summarySE(data = sgloc1, measurevar = "shannonsdsgtrans", groupvars = c("group2","plot"))

ling1cat2$sdsgloc = a$shannonsdsgtrans
ling1cat2$sdsglocSE = a$se

a = summarySE(data = sgloc1, measurevar = "shannonsdsgtrans", groupvars = c("group3","plot"))

ling1cat3$sdsgloc = a$shannonsdsgtrans
ling1cat3$sdsglocSE = a$se

a = summarySE(data = sgfrag1, measurevar = "shannonsdsgtrans", groupvars = c("group1","plot"))

ling1cat1$sdsgfrag = a$shannonsdsgtrans
ling1cat1$sdsgfragSE = a$se

a = summarySE(data = sgfrag1, measurevar = "shannonsdsgtrans", groupvars = c("group2","plot"))

ling1cat2$sdsgfrag = a$shannonsdsgtrans
ling1cat2$sdsgfragSE = a$se

a = summarySE(data = sgfrag1, measurevar = "shannonsdsgtrans", groupvars = c("group3","plot"))

ling1cat3$sdsgfrag = a$shannonsdsgtrans
ling1cat3$sdsgfragSE = a$se

##### ling2 sdsg #######

a = summarySE(data = sggro2, measurevar = "shannonsdsgtrans", groupvars = c("group1","plot"))

ling2cat1$sdsggro = a$shannonsdsgtrans
ling2cat1$sdsggroSE = a$se

a = summarySE(data = sggro2, measurevar = "shannonsdsgtrans", groupvars = c("group2","plot"))

ling2cat2$sdsggro = a$shannonsdsgtrans
ling2cat2$sdsggroSE = a$se

a = summarySE(data = sggro2, measurevar = "shannonsdsgtrans", groupvars = c("group3","plot"))

ling2cat3$sdsggro = a$shannonsdsgtrans
ling2cat3$sdsggroSE = a$se

a = summarySE(data = sgloc2, measurevar = "shannonsdsgtrans", groupvars = c("group1","plot"))

ling2cat1$sdsgloc = a$shannonsdsgtrans
ling2cat1$sdsglocSE = a$se

a = summarySE(data = sgloc2, measurevar = "shannonsdsgtrans", groupvars = c("group2","plot"))

ling2cat2$sdsgloc = a$shannonsdsgtrans
ling2cat2$sdsglocSE = a$se

a = summarySE(data = sgloc2, measurevar = "shannonsdsgtrans", groupvars = c("group3","plot"))

ling2cat3$sdsgloc = a$shannonsdsgtrans
ling2cat3$sdsglocSE = a$se

a = summarySE(data = sgfrag2, measurevar = "shannonsdsgtrans", groupvars = c("group1","plot"))

ling2cat1$sdsgfrag = a$shannonsdsgtrans
ling2cat1$sdsgfragSE = a$se

a = summarySE(data = sgfrag2, measurevar = "shannonsdsgtrans", groupvars = c("group2","plot"))

ling2cat2$sdsgfrag = a$shannonsdsgtrans
ling2cat2$sdsgfragSE = a$se

a = summarySE(data = sgfrag2, measurevar = "shannonsdsgtrans", groupvars = c("group3","plot"))

ling2cat3$sdsgfrag = a$shannonsdsgtrans
ling2cat3$sdsgfragSE = a$se

###### ling2 sgsg #######


a = summarySE(data = sggro2, measurevar = "shannonsgsgtrans", groupvars = c("group1","plot"))

ling2cat1$sgsggro = a$shannonsgsgtrans
ling2cat1$sgsggroSE = a$se

a = summarySE(data = sggro2, measurevar = "shannonsgsgtrans", groupvars = c("group2","plot"))

ling2cat2$sgsggro = a$shannonsgsgtrans
ling2cat2$sgsggroSE = a$se

a = summarySE(data = sggro2, measurevar = "shannonsgsgtrans", groupvars = c("group3","plot"))

ling2cat3$sgsggro = a$shannonsgsgtrans
ling2cat3$sgsggroSE = a$se

a = summarySE(data = sgloc2, measurevar = "shannonsgsgtrans", groupvars = c("group1","plot"))

ling2cat1$sgsgloc = a$shannonsgsgtrans
ling2cat1$sgsglocSE = a$se

a = summarySE(data = sgloc2, measurevar = "shannonsgsgtrans", groupvars = c("group2","plot"))

ling2cat2$sgsgloc = a$shannonsgsgtrans
ling2cat2$sgsglocSE = a$se

a = summarySE(data = sgloc2, measurevar = "shannonsgsgtrans", groupvars = c("group3","plot"))

ling2cat3$sgsgloc = a$shannonsgsgtrans
ling2cat3$sgsglocSE = a$se

a = summarySE(data = sgfrag2, measurevar = "shannonsgsgtrans", groupvars = c("group1","plot"))

ling2cat1$sgsgfrag = a$shannonsgsgtrans
ling2cat1$sgsgfragSE = a$se

a = summarySE(data = sgfrag2, measurevar = "shannonsgsgtrans", groupvars = c("group2","plot"))

ling2cat2$sgsgfrag = a$shannonsgsgtrans
ling2cat2$sgsgfragSE = a$se

a = summarySE(data = sgfrag2, measurevar = "shannonsgsgtrans", groupvars = c("group3","plot"))

ling2cat3$sgsgfrag = a$shannonsgsgtrans
ling2cat3$sgsgfragSE = a$se

################# slope, rock and canopy in loc and frag #############

a = summarySE(sggro1, measurevar="canopy", groupvars=c("site","location","plot"))
a$site = as.character(a$site)
a$location = as.character(a$location)
sgloc1$canopy = 0
sgloc1$canopySE = 0

for (i in 1:185)
{
  sgloc1[sgloc1$site == a$site[i] & sgloc1$location == a$location[i] & sgloc1$plot == a$plot[i],]$canopy[1] = a$canopy[i]
  sgloc1[sgloc1$site == a$site[i] & sgloc1$location == a$location[i] & sgloc1$plot == a$plot[i],]$canopySE[1] = a$se[i]
}

b = summarySE(sggro1, measurevar="canopy", groupvars=c("site","plot"))
b$site = as.character(b$site)
sgfrag1$canopy = 0
sgfrag1$canopySE = 0

for (i in 1:105)
{
  sgfrag1[sgfrag1$site == b$site[i] & sgfrag1$plot == b$plot[i],]$canopy[1] = b$canopy[i]
  sgfrag1[sgfrag1$site == b$site[i] & sgfrag1$plot == b$plot[i],]$canopySE[1] = b$se[i]
}

sgloc2$canopy = sgloc1$canopy
sgloc2$canopySE = sgloc1$canopySE

sgfrag2$canopy = sgfrag1$canopy
sgfrag2$canopySE = sgfrag1$canopy

#########################

head(sggro1)
length(sgro[1,])
head(sgfrag1)

sgfrag1$group3plot = paste(as.character(sgfrag1$group3),as.character(sgfrag1$plot),sep = "-")
sgfrag2$group3plot = paste(as.character(sgfrag2$group3),as.character(sgfrag2$plot),sep = "-")
sfrag$group3plot = paste(as.character(sfrag$group3),as.character(sfrag$plot),sep = "-")

sgloc1$group3plot = paste(as.character(sgloc1$group3),as.character(sgloc1$plot),sep = "-")
sgloc2$group3plot = paste(as.character(sgloc2$group3),as.character(sgloc2$plot),sep = "-")
sloc$group3plot = paste(as.character(sloc$group3),as.character(sloc$plot),sep = "-")

sggro1$group3plot = paste(as.character(sggro1$group3),as.character(sggro1$plot),sep = "-")
sggro2$group3plot = paste(as.character(sggro2$group3),as.character(sggro2$plot),sep = "-")
sgro$group3plot = paste(as.character(sgro$group3),as.character(sgro$plot),sep = "-")


############# ordination scores #########



################ beta diversity for the groups: group 1 #############


bd1 = vegdist(sgrom)

mod = betadisper(bd1,sgro$group1)

anova(mod)

permutest(mod, pairwise = TRUE)

mod.HSD <- TukeyHSD(mod)
plot(mod.HSD)

plot(mod)

boxplot(mod)

scores(mod, choices=c(1,2))

########## remove useless things ####################

rm(cooks,temp,temp1,a,b,c,fit.nlmer1,fit.nlmer2,fit.nlmer3,fit.nlmer4,fit.nlmer5,fit.nlmer7,fit.nlmer8,fit2,fit3,fit4,fit5,fit6,fit7,fit8,fit9,influence,log.f)

rm(bb_se,cooks,parvals,parvals1,predvals,temp,a,b,c,fit.nlmer1,fit8,ggp,i,influence,maxvals,minvals,mpars,plot5,plot6,result,size,status1,x,y,FUN,log.f)

rm(parvals,parvals1,predvals,sdshannonsmall,seeds,seedslarge,seedsmedium,seedssmall,sl1,sl1large,sl1large1,sl1large12,sl1large125,sl1large126,sl1large2,sl1large5,sl1large57,sl1large6,sl1large67,sl1large7,sl1medium,sl1medium1,sl1medium12,sl1medium125,sl1medium126,sl1medium2,sl1medium5,sl1medium57,sl1medium6,sl1medium67,sl1medium7,sl1small,sl1small1,sl1small12,sl1small125,sl1small126,sl1small2,sl1small5,sl1small57,sl1small6,sl1small67,sl1small7,sl2,sl2large,sl2large1,sl2large12,sl2large125,sl2large126,sl2large2,sl2large5,sl2large57,sl2large6,sl2large67,sl2large7,sl2medium,sl2medium1,sl2medium12,sl2medium125,sl2medium126,sl2medium2,sl2medium5,sl2medium57,sl2medium6,sl2medium67,sl2medium7,sl2small,sl2small1,sl2small12,sl2small125,sl2small126,sl2small2,sl2small5,sl2small57,sl2small6,sl2small67,sl2small7,specbase,temp,temp1,temp2,tempspecbase,tempspecbase1,tempspecbase11,tempspecbase12,tempspecbase121,tempspecbase122,tempspecbase1251,tempspecbase1252,tempspecbase1261,tempspecbase1262,tempspecbase2,tempspecbase21,tempspecbase22,tempspecbase51,tempspecbase52,tempspecbase571,tempspecbase572,tempspecbase61,tempspecbase62,tempspecbase671,tempspecbase672,tempspecbase71,tempspecbase72,a,b,c,count,fit.nlmer1,fit3,fit4,fit5,fit6,fit7,fit8,ggp,i,j,maxvals,minvals,mpars,result,sam,size,FUN,log.f)

rm(x,y)

head(sgfrag1)

gro1m = read.csv("C:/Users/ashwinv/Desktop/gro1m.csv")
gro2m = read.csv("C:/Users/ashwinv/Desktop/gro2m.csv")
loc2m = read.csv("C:/Users/ashwinv/Desktop/loc2m.csv")

sgloc2$beta2

tail(scor$sites[,1])

sgloc2$beta1 = NA
sgloc2$beta2 = NA

count = 1
for (i in 1:185)
{
  if (loc2m$sum[i] != 0)
  {
    sgloc2$beta1[i] = scor$sites[,1][count]
    sgloc2$beta2[i] = scor$sites[,2][count]
    count = count + 1
  }
}

sgfrag2$beta1 = scor$sites[,1]
sgfrag2$beta2 = scor$sites[,2]
