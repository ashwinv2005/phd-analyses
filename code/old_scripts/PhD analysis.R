plotinfo = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Plot info for analyses.csv")
head(plotinfo)
with(plotinfo,boxplot(canopy1~fragsize, xlab = "Fragment size", ylab = "Canopy cover"))

seeds = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Seedsoverall.csv")
fruits = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Fruitsoverall.csv")
Julyseedlings = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Status by species June July.csv")
Octseedlings = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Status by species October.csv")


head(seedsinkadamanepersite)
head(seeds)

length(unique(Octseedlings$Species))

write.csv(seedsinkadamanepersite,"C:/Users/ashwinv/Desktop/seedsinkadamanepersite.csv")

##### SEEDS #######

overallseedsinkadamane = summarySE(data = seeds, measurevar = "count", groupvars = "name")
overallseedsinkadamane$count = overallseedsinkadamane$N * overallseedsinkadamane$count
overallseedsinkadamane = overallseedsinkadamane[,c(1,3)]
overallseedsinkadamane = overallseedsinkadamane[-1,]

seedsinkadamanepersite = summarySE(data = seeds, measurevar = "count", groupvars = c("site","name"))
seedsinkadamanepersite$count = seedsinkadamanepersite$N * seedsinkadamanepersite$count
seedsinkadamanepersite = seedsinkadamanepersite[,c(1,2,4)]
seedsinkadamanepersite = seedsinkadamanepersite[seedsinkadamanepersite$name != 0,]

seedsinkadamaneperloc = summarySE(data = seeds, measurevar = "count", groupvars = c("site","location","name"))
seedsinkadamaneperloc$count = seedsinkadamaneperloc$N * seedsinkadamaneperloc$count
seedsinkadamaneperloc = seedsinkadamaneperloc[,c(1,2,3,5)]
seedsinkadamaneperloc = seedsinkadamaneperloc[seedsinkadamaneperloc$name != 0,]

##### FRUITS #######

overallfruitsinkadamane = summarySE(data = fruits, measurevar = "count", groupvars = "Name")
overallfruitsinkadamane$count = overallfruitsinkadamane$N * overallfruitsinkadamane$count
overallfruitsinkadamane = overallfruitsinkadamane[,c(1,3)]
overallfruitsinkadamane = overallfruitsinkadamane[-1,]

fruitsinkadamanepersite = summarySE(data = fruits, measurevar = "count", groupvars = c("site","Name"))
fruitsinkadamanepersite$count = fruitsinkadamanepersite$N * fruitsinkadamanepersite$count
fruitsinkadamanepersite = fruitsinkadamanepersite[,c(1,2,4)]
fruitsinkadamanepersite = fruitsinkadamanepersite[fruitsinkadamanepersite$Name != 0,]

fruitsinkadamaneperloc = summarySE(data = fruits, measurevar = "count", groupvars = c("site","location","Name"))
fruitsinkadamaneperloc$count = fruitsinkadamaneperloc$N * fruitsinkadamaneperloc$count
fruitsinkadamaneperloc = fruitsinkadamaneperloc[,c(1,2,3,5)]
fruitsinkadamaneperloc = fruitsinkadamaneperloc[fruitsinkadamaneperloc$Name != 0,]

##### SEEDLINGS JULY CENSUS 1 #######

overallseedlingsjuly = summarySE(data = Julyseedlings, measurevar = "count", groupvars = "Species")
overallseedlingsjuly$count = overallseedlingsjuly$N * overallseedlingsjuly$count
overallseedlingsjuly = overallseedlingsjuly[,c(1,3)]
overallseedlingsjuly = overallseedlingsjuly[-1,]

julyseedlingspersite = summarySE(data = Julyseedlings, measurevar = "count", groupvars = c("site","Species"))
julyseedlingspersite$count = julyseedlingspersite$N * julyseedlingspersite$count
julyseedlingspersite = julyseedlingspersite[,c(1,2,4)]
julyseedlingspersite = julyseedlingspersite[julyseedlingspersite$count != 0,]

julyseedlingsperloc = summarySE(data = Julyseedlings, measurevar = "count", groupvars = c("site","location","Species"))
julyseedlingsperloc$count = julyseedlingsperloc$N * julyseedlingsperloc$count
julyseedlingsperloc = julyseedlingsperloc[,c(1,2,3,5)]
julyseedlingsperloc = julyseedlingsperloc[julyseedlingsperloc$count != 0,]

##### SEEDLINGS OCTOBER CENSUS 1 #######

overallseedlingsoct = summarySE(data = Octseedlings, measurevar = "count", groupvars = "Species")
overallseedlingsoct$count = overallseedlingsoct$N * overallseedlingsoct$count
overallseedlingsoct = overallseedlingsoct[,c(1,3)]
overallseedlingsoct = overallseedlingsoct[-1,]

octseedlingspersite = summarySE(data = Octseedlings, measurevar = "count", groupvars = c("site","Species"))
octseedlingspersite$count = octseedlingspersite$N * octseedlingspersite$count
octseedlingspersite = octseedlingspersite[,c(1,2,4)]
octseedlingspersite = octseedlingspersite[octseedlingspersite$count != 0,]

octseedlingsperloc = summarySE(data = Octseedlings, measurevar = "count", groupvars = c("site","location","Species"))
octseedlingsperloc$count = octseedlingsperloc$N * octseedlingsperloc$count
octseedlingsperloc = octseedlingsperloc[,c(1,2,3,5)]
octseedlingsperloc = octseedlingsperloc[octseedlingsperloc$count != 0,]

##### SEEDLINGS JULY CENSUS 2 #######

foverallseedlingsjuly = summarySE(data = Julyseedlings, measurevar = "Status.2", groupvars = "Species")
foverallseedlingsjuly$Status.2 = foverallseedlingsjuly$N * foverallseedlingsjuly$Status.2
foverallseedlingsjuly = foverallseedlingsjuly[,c(1,3)]
foverallseedlingsjuly = foverallseedlingsjuly[-1,]

fjulyseedlingspersite = summarySE(data = Julyseedlings, measurevar = "Status.2", groupvars = c("site","Species"))
fjulyseedlingspersite$Status.2 = fjulyseedlingspersite$N * fjulyseedlingspersite$Status.2
fjulyseedlingspersite = fjulyseedlingspersite[,c(1,2,4)]
fjulyseedlingspersite = fjulyseedlingspersite[fjulyseedlingspersite$Status.2 != 0,]

fjulyseedlingsperloc = summarySE(data = Julyseedlings, measurevar = "Status.2", groupvars = c("site","location","Species"))
fjulyseedlingsperloc$Status.2 = fjulyseedlingsperloc$N * fjulyseedlingsperloc$Status.2
fjulyseedlingsperloc = fjulyseedlingsperloc[,c(1,2,3,5)]
fjulyseedlingsperloc = fjulyseedlingsperloc[fjulyseedlingsperloc$Status.2 != 0,]

##### SEEDLINGS OCTOBER CENSUS 2 #######

foverallseedlingsoct = summarySE(data = Octseedlings, measurevar = "Status.2", groupvars = "Species")
foverallseedlingsoct$Status.2 = foverallseedlingsoct$N * foverallseedlingsoct$Status.2
foverallseedlingsoct = foverallseedlingsoct[,c(1,3)]
foverallseedlingsoct = foverallseedlingsoct[-1,]

foctseedlingspersite = summarySE(data = Octseedlings, measurevar = "Status.2", groupvars = c("site","Species"))
foctseedlingspersite$Status.2 = foctseedlingspersite$N * foctseedlingspersite$Status.2
foctseedlingspersite = foctseedlingspersite[,c(1,2,4)]
foctseedlingspersite = foctseedlingspersite[foctseedlingspersite$Status.2 != 0,]

foctseedlingsperloc = summarySE(data = Octseedlings, measurevar = "Status.2", groupvars = c("site","location","Species"))
foctseedlingsperloc$Status.2 = foctseedlingsperloc$N * foctseedlingsperloc$Status.2
foctseedlingsperloc = foctseedlingsperloc[,c(1,2,3,5)]
foctseedlingsperloc = foctseedlingsperloc[foctseedlingsperloc$Status.2 != 0,]

##### Species richness #######

richseedspersite = with(seedsinkadamanepersite,tapply(name,site,length))
richfruitspersite = with(fruitsinkadamanepersite,tapply(Name,site,length))

richseedlingspersitejuly = with(julyseedlingspersite,tapply(Species,site,length))
richseedlingspersiteoct = with(octseedlingspersite,tapply(Species,site,length))

frichseedlingspersitejuly = with(fjulyseedlingspersite,tapply(Species,site,length))
frichseedlingspersiteoct = with(foctseedlingspersite,tapply(Species,site,length))



fragsize = with(plotinfo,tapply(fragsize,site,mean))

plot(frichseedlingspersiteoct~fragsize, xlab = "Fragment size", ylab = "Seedling richness Oct 2")

plot(frichseedlingspersiteoct~richseedspersite, xlab = "Seed richness", ylab = "Seedling richness Oct 2")




perloc = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/perloc.csv")

head(perloc)

for (i in 1:37)
{
  perloc$seeds[i] = length(seedsinkadamaneperloc[seedsinkadamaneperloc$location == perloc$location[i] & seedsinkadamaneperloc$site == perloc$site[i],]$name)
  perloc$july1[i] = length(julyseedlingsperloc[julyseedlingsperloc$location == perloc$location[i] & julyseedlingsperloc$site == perloc$site[i],]$Species)
  perloc$july2[i] = length(fjulyseedlingsperloc[fjulyseedlingsperloc$location == perloc$location[i] & fjulyseedlingsperloc$site == perloc$site[i],]$Species)
  perloc$oct1[i] = length(octseedlingsperloc[octseedlingsperloc$location == perloc$location[i] & octseedlingsperloc$site == perloc$site[i],]$Species)

}

plot(perloc$seeds~perloc$size, xlab = "Fragment size", ylab = "Seed richness")
plot(perloc$july1~perloc$size, xlab = "Fragment size", ylab = "Seedling richness july 1")
plot(perloc$july2~perloc$size, xlab = "Fragment size", ylab = "Seedling richness july 2")
plot(perloc$oct1~perloc$size, xlab = "Fragment size", ylab = "Seedling richness oct 1")

plot(perloc$july2~perloc$seeds, xlab = "Seed richness", ylab = "Seedling richness july 2")

write.csv(unique(oct$Species),"C:/Users/ashwinv/Desktop/seedlingnameso.csv")

########### Convert to diversity (Vegan) format ###################

july = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Status by species June July.csv")
oct = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Status by species October.csv")

july$id = as.character(july$id)
july$Species = as.character(july$Species)
july$site= as.character(july$site)
july$location = as.character(july$location)
july$group = as.character(july$group)

oct$id = as.character(oct$id)
oct$Species = as.character(oct$Species)
oct$site= as.character(oct$site)
oct$location = as.character(oct$location)
oct$group = as.character(oct$group)

j1 = july[july$plot == 1,]
j2 = july[july$plot == 2,]
j5 = july[july$plot == 5,]
j6 = july[july$plot == 6,]
j7 = july[july$plot == 7,]

o1 = oct[oct$plot == 1,]
o2 = oct[oct$plot == 2,]
o5 = oct[oct$plot == 5,]
o6 = oct[oct$plot == 6,]
o7 = oct[oct$plot == 7,]

s11 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 1/1seedlinggrodiversity1.csv")
s12 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 1/1seedlinggrodiversity2.csv")
s15 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 1/1seedlinggrodiversity5.csv")
s16 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 1/1seedlinggrodiversity6.csv")
s17 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 1/1seedlinggrodiversity7.csv")

s21 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 2/2seedlinggrodiversity1.csv")
s22 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 2/2seedlinggrodiversity2.csv")
s25 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 2/2seedlinggrodiversity5.csv")
s26 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 2/2seedlinggrodiversity6.csv")
s27 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 2/2seedlinggrodiversity7.csv")

x = names(s11)

for (i in 1:length(j1$Species))
{
  temp = j1[i,]
  for (j in 1:length(x))
  {
    if (x[j] == temp$Species[1])
      f = j
  }
  s11[s11$site == temp$site[1] & s11$location == temp$location[1] & s11$group == temp$group[1],f] = temp$count[1]
  s21[s21$site == temp$site[1] & s21$location == temp$location[1] & s21$group == temp$group[1],f] = temp$Status.2[1]
}

for (i in 1:length(o1$Species))
{
  temp = o1[i,]
  for (j in 1:length(x))
  {
    if (x[j] == temp$Species)
      f = j
  }
  s11[s11$site == temp$site[1] & s11$location == temp$location[1] & s11$group == temp$group[1],f] = temp$count[1]
  s21[s21$site == temp$site[1] & s21$location == temp$location[1] & s21$group == temp$group[1],f] = temp$Status.2[1]
}

x = names(s12)

for (i in 1:length(j2$Species))
{
  temp = j2[i,]
  for (j in 1:length(x))
  {
    if (x[j] == temp$Species[1])
      f = j
  }
  s12[s12$site == temp$site[1] & s12$location == temp$location[1] & s12$group == temp$group[1],f] = temp$count[1]
  s22[s22$site == temp$site[1] & s22$location == temp$location[1] & s22$group == temp$group[1],f] = temp$Status.2[1]
}

for (i in 1:length(o2$Species))
{
  temp = o2[i,]
  for (j in 1:length(x))
  {
    if (x[j] == temp$Species)
      f = j
  }
  s12[s12$site == temp$site[1] & s12$location == temp$location[1] & s12$group == temp$group[1],f] = temp$count[1]
  s22[s22$site == temp$site[1] & s22$location == temp$location[1] & s22$group == temp$group[1],f] = temp$Status.2[1]
}

x = names(s15)

for (i in 1:length(j5$Species))
{
  temp = j5[i,]
  for (j in 1:length(x))
  {
    if (x[j] == temp$Species[1])
      f = j
  }
  s15[s15$site == temp$site[1] & s15$location == temp$location[1] & s15$group == temp$group[1],f] = temp$count[1]
  s25[s25$site == temp$site[1] & s25$location == temp$location[1] & s25$group == temp$group[1],f] = temp$Status.2[1]
}

for (i in 1:length(o5$Species))
{
  temp = o5[i,]
  for (j in 1:length(x))
  {
    if (x[j] == temp$Species)
      f = j
  }
  s15[s15$site == temp$site[1] & s15$location == temp$location[1] & s15$group == temp$group[1],f] = temp$count[1]
  s25[s25$site == temp$site[1] & s25$location == temp$location[1] & s25$group == temp$group[1],f] = temp$Status.2[1]
}

x = names(s16)

for (i in 1:length(j6$Species))
{
  temp = j6[i,]
  for (j in 1:length(x))
  {
    if (x[j] == temp$Species[1])
      f = j
  }
  s16[s16$site == temp$site[1] & s16$location == temp$location[1] & s16$group == temp$group[1],f] = temp$count[1]
  s26[s26$site == temp$site[1] & s26$location == temp$location[1] & s26$group == temp$group[1],f] = temp$Status.2[1]
}

for (i in 1:length(o6$Species))
{
  temp = o6[i,]
  for (j in 1:length(x))
  {
    if (x[j] == temp$Species)
      f = j
  }
  s16[s16$site == temp$site[1] & s16$location == temp$location[1] & s16$group == temp$group[1],f] = temp$count[1]
  s26[s26$site == temp$site[1] & s26$location == temp$location[1] & s26$group == temp$group[1],f] = temp$Status.2[1]
}

x = names(s17)

for (i in 1:length(j7$Species))
{
  temp = j7[i,]
  for (j in 1:length(x))
  {
    if (x[j] == temp$Species[1])
      f = j
  }
  s17[s17$site == temp$site[1] & s17$location == temp$location[1] & s17$group == temp$group[1],f] = temp$count[1]
  s27[s27$site == temp$site[1] & s27$location == temp$location[1] & s27$group == temp$group[1],f] = temp$Status.2[1]
}

for (i in 1:length(o7$Species))
{
  temp = o7[i,]
  for (j in 1:length(x))
  {
    if (x[j] == temp$Species)
      f = j
  }
  s17[s17$site == temp$site[1] & s17$location == temp$location[1] & s17$group == temp$group[1],f] = temp$count[1]
  s27[s27$site == temp$site[1] & s27$location == temp$location[1] & s27$group == temp$group[1],f] = temp$Status.2[1]
}

write.csv(s27,"C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 2/2seedlinggrodiversity7.csv")


######## For location and site data #######################

head(l11)
length(x)
sum(s11[,5:110])

l11 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 1/1seedlinglocdiversity1.csv")
l12 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 1/1seedlinglocdiversity2.csv")
l15 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 1/1seedlinglocdiversity5.csv")
l16 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 1/1seedlinglocdiversity6.csv")
l17 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 1/1seedlinglocdiversity7.csv")

l21 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 2/2seedlinglocdiversity1.csv")
l22 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 2/2seedlinglocdiversity2.csv")
l25 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 2/2seedlinglocdiversity5.csv")
l26 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 2/2seedlinglocdiversity6.csv")
l27 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 2/2seedlinglocdiversity7.csv")

f11 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 1/1seedlingfragdiversity1.csv")
f12 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 1/1seedlingfragdiversity2.csv")
f15 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 1/1seedlingfragdiversity5.csv")
f16 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 1/1seedlingfragdiversity6.csv")
f17 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 1/1seedlingfragdiversity7.csv")

f21 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 2/2seedlingfragdiversity1.csv")
f22 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 2/2seedlingfragdiversity2.csv")
f25 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 2/2seedlingfragdiversity5.csv")
f26 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 2/2seedlingfragdiversity6.csv")
f27 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 2/2seedlingfragdiversity7.csv")

f11$site = as.character(f11$site)
f21$site = as.character(f21$site)
f12$site = as.character(f12$site)
f22$site = as.character(f22$site)
f15$site = as.character(f15$site)
f25$site = as.character(f25$site)
f16$site = as.character(f16$site)
f26$site = as.character(f26$site)
f17$site = as.character(f17$site)
f27$site = as.character(f27$site)

l11$location = as.character(l11$location)
l21$location = as.character(l21$location)
l12$location = as.character(l12$location)
l22$location = as.character(l22$location)
l15$location = as.character(l15$location)
l25$location = as.character(l25$location)
l16$location = as.character(l16$location)
l26$location = as.character(l26$location)
l17$location = as.character(l17$location)
l27$location = as.character(l27$location)

names(f11)
x = length(f11$site)

for (i in 1:x)
{
  temp = l11[l11$site == f11$site[i],]
  for (j in 4:109)
  {
    sm = sum(temp[,j])
    f11[i,j-1] = sm
  }
  
  temp = l12[l12$site == f12$site[i],]
  for (j in 4:109)
  {
    sm = sum(temp[,j])
    f12[i,j-1] = sm
  }
  
  temp = l15[l15$site == f15$site[i],]
  for (j in 4:109)
  {
    sm = sum(temp[,j])
    f15[i,j-1] = sm
  }
  
  temp = l16[l16$site == f16$site[i],]
  for (j in 4:109)
  {
    sm = sum(temp[,j])
    f16[i,j-1] = sm
  }
  
  temp = l17[l17$site == f17$site[i],]
  for (j in 4:109)
  {
    sm = sum(temp[,j])
    f17[i,j-1] = sm
  }
  
  temp = l21[l21$site == f21$site[i],]
  for (j in 4:109)
  {
    sm = sum(temp[,j])
    f21[i,j-1] = sm
  }
  
  temp = l22[l22$site == f22$site[i],]
  for (j in 4:109)
  {
    sm = sum(temp[,j])
    f22[i,j-1] = sm
  }
  
  temp = l25[l25$site == f25$site[i],]
  for (j in 4:109)
  {
    sm = sum(temp[,j])
    f25[i,j-1] = sm
  }
  
  temp = l26[l26$site == f26$site[i],]
  for (j in 4:109)
  {
    sm = sum(temp[,j])
    f26[i,j-1] = sm
  }
  
  temp = l27[l27$site == f27$site[i],]
  for (j in 4:109)
  {
    sm = sum(temp[,j])
    f27[i,j-1] = sm
  }
}


head(f22)

write.csv(f11,"C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seedlings census 1/1seedlingfragdiversity1.csv")

############## For seed group location and site data ############

seednet = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seeds/seedplodiversity.csv")
seedgro = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seeds/seedgrodiversity.csv")
seedloc = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seeds/seedlocdiversity.csv")
seedfra = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seeds/seedfragdiversity.csv")

seedfra$site = as.character(seedfra$site)
length(names(seednet))

x = length(seedfra$site)

for (i in 1:x)
{
  temp = seedloc[seedloc$site == seedfra$site[i],]
  for (j in 6:95)
  {
    sm = sum(temp[,j])
    seedfra[i,j-1] = sm
  }
}

head(seedfra)

write.csv(seedfra,"C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Diversity analyses/Seeds/seedfragdiversity.csv")

