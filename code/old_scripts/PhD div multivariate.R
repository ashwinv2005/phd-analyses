temp1 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Status by species June July.csv")
temp2 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Status by species October.csv")
temp3 = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/Seeds by species.csv")

a = length(temp1$species)
b = length(temp2$species)
c = length(temp3$species)

sggroup = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/seedlingsforanalysis.csv")
sgroup = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/seedsforanalysis.csv")
sglocation = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/seedlingsforanalysisloc.csv")
slocation = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/seedsforanalysisloc.csv")
sgfragment = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/seedlingsforanalysisfrag.csv")
sfragment = read.csv("C:/Users/ashwinv/Desktop/Ashwin PhD Project/Data/Kadamane/Data for analysis/Project Data Compiled/Final/seedsforanalysisfrag.csv")

sgfragment$site = as.character(sgfragment$site)
sfragment$site = as.character(sfragment$site)
#sglocation$location = as.character(sglocation$location)
#slocation$location = as.character(slocation$location)
#sggroup$group = as.character(sggroup$group)
#sgroup$group = as.character(sgroup$group)
sgfragment$species = as.character(sgfragment$species)
sfragment$species = as.character(sfragment$species)


head(temp2)

sggroup = sggroup[,1:8]
sgroup = sgroup[,1:5]
sgroup$status1 = 0

for (i in 1:b)
{
  sgfragment[sgfragment$site == temp2$site[i] & sgfragment$plot == temp2$plot[i] & sgfragment$species == temp2$species[i],]$status1[1] = sgfragment[sgfragment$site == temp2$site[i] & sgfragment$plot == temp2$plot[i] & sgfragment$species == temp2$species[i],]$status1[1] + temp2$count[i]
  sgfragment[sgfragment$site == temp2$site[i] & sgfragment$plot == temp2$plot[i] & sgfragment$species == temp2$species[i],]$status2[1] = sgfragment[sgfragment$site == temp2$site[i] & sgfragment$plot == temp2$plot[i] & sgfragment$species == temp2$species[i],]$status2[1] + temp2$Status.2[i]
  #sgfragment[sgfragment$site == temp1$site[i] & sgfragment$plot == temp1$plot[i] & sgfragment$species == temp1$species[i],]$statusmid[1] = sgfragment[sgfragment$site == temp1$site[i] & sgfragment$plot == temp1$plot[i] & sgfragment$species == temp1$species[i],]$statusmid[1] + temp1$Status.1[i]
}

for (i in 1:c)
{
  sfragment[sfragment$site == temp3$site[i] & sfragment$species == temp3$species[i],]$status1[1] = sfragment[sfragment$site == temp3$site[i] & sfragment$species == temp3$species[i],]$status1[1] + temp3$count[i]
  #sggroup[sggroup$site == temp2$site[i] & sggroup$location == temp2$location[i] & sggroup$group == temp2$group[i] & sggroup$plot == temp2$plot[i] & sggroup$species == temp2$species[i],]$status2[1] = temp2$Status.2[i]
  #sggroup[sggroup$site == temp2$site[i] & sggroup$location == temp2$location[i] & sggroup$group == temp2$group[i] & sggroup$plot == temp2$plot[i] & sggroup$species == temp2$species[i],]$statusmid[1] = temp2$Status.1[i]
}

head(sgloc1)

sggroup$size = sggroup$connect = sggroup$slope = sggroup$rock = sggroup$canopy = sggroup$shannon = sggroup$rich = sggroup$sdsgtrans = sggroup$sgsgtrans = 0
sglocation$size = sglocation$connect = sglocation$slope = sglocation$rock = sglocation$canopy = sglocation$shannon = sglocation$rich = sglocation$sdsgtrans = sglocation$sgsgtrans = 0
sgfragment$size = sgfragment$connect = sgfragment$slope = sgfragment$rock = sgfragment$canopy = sgfragment$shannon = sgfragment$rich = sgfragment$sdsgtrans = sgfragment$sgsgtrans = 0

sgroup$size = sgroup$connect = sgroup$shannon = sgroup$rich = 0
slocation$size = slocation$connect = slocation$shannon = slocation$rich = 0
sfragment$size = sfragment$connect = sfragment$shannon = sfragment$rich = 0

sggroup$beta11 = sggroup$beta12 = sggroup$beta21 = sggroup$beta22 = 0
sglocation$beta11 = sglocation$beta12 = sglocation$beta21 = sglocation$beta22 = 0
sgfragment$beta11 = sgfragment$beta12 = sgfragment$beta21 = sgfragment$beta22 = 0

sgroup$beta1 = sgroup$beta2 = 0
slocation$beta1 = slocation$beta2 = 0
sfragment$beta1 = sfragment$beta2 = 0


for (i in 1:length(sggroup$site))
{
  #sggroup$size[i] = sggro1[sggro1$site == sggroup$site[i] & sggro1$location == sggroup$location[i] & sggro1$group == sggroup$group[i] & sggro1$plot == sggroup$plot[i],]$size[1]
  #sggroup$connect[i] = sggro1[sggro1$site == sggroup$site[i] & sggro1$location == sggroup$location[i] & sggro1$group == sggroup$group[i] & sggro1$plot == sggroup$plot[i],]$connect[1]
  #sggroup$slope[i] = sggro1[sggro1$site == sggroup$site[i] & sggro1$location == sggroup$location[i] & sggro1$group == sggroup$group[i] & sggro1$plot == sggroup$plot[i],]$slope[1]
  #sggroup$rock[i] = sggro1[sggro1$site == sggroup$site[i] & sggro1$location == sggroup$location[i] & sggro1$group == sggroup$group[i] & sggro1$plot == sggroup$plot[i],]$rock[1]
  #sggroup$canopy[i] = sggro1[sggro1$site == sggroup$site[i] & sggro1$location == sggroup$location[i] & sggro1$group == sggroup$group[i] & sggro1$plot == sggroup$plot[i],]$canopy[1]
  #sggroup$shannon[i] = sggro1[sggro1$site == sggroup$site[i] & sggro1$location == sggroup$location[i] & sggro1$group == sggroup$group[i] & sggro1$plot == sggroup$plot[i],]$shannon[1]
  #sggroup$rich[i] = sggro1[sggro1$site == sggroup$site[i] & sggro1$location == sggroup$location[i] & sggro1$group == sggroup$group[i] & sggro1$plot == sggroup$plot[i],]$rich[1]
  #sggroup$sdsgtrans[i] = sggro1[sggro1$site == sggroup$site[i] & sggro1$location == sggroup$location[i] & sggro1$group == sggroup$group[i] & sggro1$plot == sggroup$plot[i],]$shannonsdsgtrans[1]
  #sggroup$sgsgtrans[i] = sggro2[sggro2$site == sggroup$site[i] & sggro2$location == sggroup$location[i] & sggro2$group == sggroup$group[i] & sggro2$plot == sggroup$plot[i],]$shannonsgsgtrans[1]
  sggroup$beta11[i] = sggro1[sggro1$site == sggroup$site[i] & sggro1$location == sggroup$location[i] & sggro1$group == sggroup$group[i] & sggro1$plot == sggroup$plot[i],]$beta1[1]
  sggroup$beta12[i] = sggro1[sggro1$site == sggroup$site[i] & sggro1$location == sggroup$location[i] & sggro1$group == sggroup$group[i] & sggro1$plot == sggroup$plot[i],]$beta2[1]
  sggroup$beta21[i] = sggro2[sggro2$site == sggroup$site[i] & sggro2$location == sggroup$location[i] & sggro2$group == sggroup$group[i] & sggro2$plot == sggroup$plot[i],]$beta1[1]
  sggroup$beta22[i] = sggro2[sggro2$site == sggroup$site[i] & sggro2$location == sggroup$location[i] & sggro2$group == sggroup$group[i] & sggro2$plot == sggroup$plot[i],]$beta2[1]
}

for (i in 1:length(sglocation$site))
{
  #sglocation$size[i] = sgloc1[sgloc1$site == sglocation$site[i] & sgloc1$location == sglocation$location[i] & sgloc1$plot == sglocation$plot[i],]$size[1]
  #sglocation$connect[i] = sgloc1[sgloc1$site == sglocation$site[i] & sgloc1$location == sglocation$location[i] & sgloc1$plot == sglocation$plot[i],]$connect[1]
  #sglocation$slope[i] = sgloc1[sgloc1$site == sglocation$site[i] & sgloc1$location == sglocation$location[i] & sgloc1$plot == sglocation$plot[i],]$slope[1]
  #sglocation$rock[i] = sgloc1[sgloc1$site == sglocation$site[i] & sgloc1$location == sglocation$location[i] & sgloc1$plot == sglocation$plot[i],]$rock[1]
  #sglocation$canopy[i] = sgloc1[sgloc1$site == sglocation$site[i] & sgloc1$location == sglocation$location[i] & sgloc1$plot == sglocation$plot[i],]$canopy[1]
  #sglocation$shannon[i] = sgloc1[sgloc1$site == sglocation$site[i] & sgloc1$location == sglocation$location[i] & sgloc1$plot == sglocation$plot[i],]$shannon[1]
  #sglocation$rich[i] = sgloc1[sgloc1$site == sglocation$site[i] & sgloc1$location == sglocation$location[i] & sgloc1$plot == sglocation$plot[i],]$rich[1]
  #sglocation$sdsgtrans[i] = sgloc1[sgloc1$site == sglocation$site[i] & sgloc1$location == sglocation$location[i] & sgloc1$plot == sglocation$plot[i],]$shannonsdsgtrans[1]
  #sglocation$sgsgtrans[i] = sgloc2[sgloc2$site == sglocation$site[i] & sgloc2$location == sglocation$location[i] & sgloc2$plot == sglocation$plot[i],]$shannonsgsgtrans[1]
  sglocation$beta11[i] = sgloc1[sgloc1$site == sglocation$site[i] & sgloc1$location == sglocation$location[i] & sgloc1$plot == sglocation$plot[i],]$beta1[1]
  sglocation$beta12[i] = sgloc1[sgloc1$site == sglocation$site[i] & sgloc1$location == sglocation$location[i] & sgloc1$plot == sglocation$plot[i],]$beta2[1]
  sglocation$beta21[i] = sgloc2[sgloc2$site == sglocation$site[i] & sgloc2$location == sglocation$location[i] & sgloc2$plot == sglocation$plot[i],]$beta1[1]
  sglocation$beta22[i] = sgloc2[sgloc2$site == sglocation$site[i] & sgloc2$location == sglocation$location[i] & sgloc2$plot == sglocation$plot[i],]$beta2[1]
}

for (i in 1:length(sgfragment$site))
{
  #sgfragment$size[i] = sgfrag1[sgfrag1$site == sgfragment$site[i] & sgfrag1$plot == sgfragment$plot[i],]$size[1]
  #sgfragment$connect[i] = sgfrag1[sgfrag1$site == sgfragment$site[i] & sgfrag1$plot == sgfragment$plot[i],]$connect[1]
  #sgfragment$slope[i] = sgfrag1[sgfrag1$site == sgfragment$site[i] & sgfrag1$plot == sgfragment$plot[i],]$slope[1]
  #sgfragment$rock[i] = sgfrag1[sgfrag1$site == sgfragment$site[i] & sgfrag1$plot == sgfragment$plot[i],]$rock[1]
  #sgfragment$canopy[i] = sgfrag1[sgfrag1$site == sgfragment$site[i] & sgfrag1$plot == sgfragment$plot[i],]$canopy[1]
  #sgfragment$shannon[i] = sgfrag1[sgfrag1$site == sgfragment$site[i] & sgfrag1$plot == sgfragment$plot[i],]$shannon[1]
  #sgfragment$rich[i] = sgfrag1[sgfrag1$site == sgfragment$site[i] & sgfrag1$plot == sgfragment$plot[i],]$rich[1]
  #sgfragment$sdsgtrans[i] = sgfrag1[sgfrag1$site == sgfragment$site[i] & sgfrag1$plot == sgfragment$plot[i],]$shannonsdsgtrans[1]
  #sgfragment$sgsgtrans[i] = sgfrag2[sgfrag2$site == sgfragment$site[i] & sgfrag2$plot == sgfragment$plot[i],]$shannonsgsgtrans[1]
  sgfragment$beta11[i] = sgfrag1[sgfrag1$site == sgfragment$site[i] & sgfrag1$plot == sgfragment$plot[i],]$beta1[1]
  sgfragment$beta12[i] = sgfrag1[sgfrag1$site == sgfragment$site[i] & sgfrag1$plot == sgfragment$plot[i],]$beta2[1]
  sgfragment$beta21[i] = sgfrag2[sgfrag2$site == sgfragment$site[i] & sgfrag2$plot == sgfragment$plot[i],]$beta1[1]
  sgfragment$beta22[i] = sgfrag2[sgfrag2$site == sgfragment$site[i] & sgfrag2$plot == sgfragment$plot[i],]$beta2[1]
}

for (i in 1:length(sgroup$site))
{
  #sgroup$size[i] = sgro[sgro$site == sgroup$site[i] & sgro$location == sgroup$location[i] & sgro$group == sgroup$group[i],]$size[1]
  #sgroup$connect[i] = sgro[sgro$site == sgroup$site[i] & sgro$location == sgroup$location[i] & sgro$group == sgroup$group[i],]$connect[1]
  #sgroup$shannon[i] = sgro[sgro$site == sgroup$site[i] & sgro$location == sgroup$location[i] & sgro$group == sgroup$group[i],]$shannon[1]
  #sgroup$rich[i] = sgro[sgro$site == sgroup$site[i] & sgro$location == sgroup$location[i] & sgro$group == sgroup$group[i],]$rich[1]
  sgroup$beta1[i] = sgro[sgro$site == sgroup$site[i] & sgro$location == sgroup$location[i] & sgro$group == sgroup$group[i],]$beta1[1]
  sgroup$beta2[i] = sgro[sgro$site == sgroup$site[i] & sgro$location == sgroup$location[i] & sgro$group == sgroup$group[i],]$beta2[1]
}

for (i in 1:length(slocation$site))
{
  #slocation$size[i] = sloc[sloc$site == slocation$site[i] & sloc$location == slocation$location[i],]$size[1]
  #slocation$connect[i] = sloc[sloc$site == slocation$site[i] & sloc$location == slocation$location[i],]$connect[1]
  #slocation$shannon[i] = sloc[sloc$site == slocation$site[i] & sloc$location == slocation$location[i],]$shannon[1]
  #slocation$rich[i] = sloc[sloc$site == slocation$site[i] & sloc$location == slocation$location[i],]$rich[1]
  slocation$beta1[i] = sloc[sloc$site == slocation$site[i] & sloc$location == slocation$location[i],]$beta1[1]
  slocation$beta2[i] = sloc[sloc$site == slocation$site[i] & sloc$location == slocation$location[i],]$beta2[1]
}

for (i in 1:length(sfragment$site))
{
  #sfragment$size[i] = sfrag[sfrag$site == sfragment$site[i],]$size[1]
  #sfragment$connect[i] = sfrag[sfrag$site == sfragment$site[i],]$connect[1]
  #sfragment$shannon[i] = sfrag[sfrag$site == sfragment$site[i],]$shannon[1]
  #sfragment$rich[i] = sfrag[sfrag$site == sfragment$site[i],]$rich[1]
  sfragment$beta1[i] = sfrag[sfrag$site == sfragment$site[i],]$beta1[1]
  sfragment$beta2[i] = sfrag[sfrag$site == sfragment$site[i],]$beta2[1]
}

sgfragment$species = as.factor(sgfragment$species)
sgfragment$plot = as.factor(sgfragment$plot)
sgfragment$site = as.factor(sgfragment$site)

sggroup$rock = sggroup$rock/100
sglocation$rock = sglocation$rock/100
sgfragment$rock = sgfragment$rock/100
sggroupsel$rock = sggroupsel$rock/100
sglocationsel$rock = sglocationsel$rock/100
sgfragmentsel$rock = sgfragmentsel$rock/100

a = summarySE(data = sggroup, measurevar = "status1", groupvars = c("groupfac","plot"))
a$total = a$N*a$status1
head(sggroupsel)

a$total = round(a$total)
sggroupsel$total = 0
head(sgloc1)
sgloc1[38:43,]

for (i in 1:length(a$total))
{
  sggroupsel[sggroupsel$groupfac == a$groupfac[i] & sggroupsel$plot == a$plot[i],]$total = a$total[i]
}

sggroup$plot5 = sggroup$plot6 = sggroup$plot57 = sggroup$plot67 = sggroup$plot567 = 0
sglocation$plot5 = sglocation$plot6 = sglocation$plot57 = sglocation$plot67 = sglocation$plot567 = 0
sggroup$plot7 = 0
sglocation$plot7 = 0

sggroupsel$plot5 = sggroupsel$plot6 = sggroupsel$plot57 = sggroupsel$plot67 = sggroupsel$plot567 = 0
sglocationsel$plot5 = sglocationsel$plot6 = sglocationsel$plot57 = sglocationsel$plot67 = sglocationsel$plot567 = 0
sggroupsel$plot7 = 0
sglocationsel$plot7 = 0

sggrouprare$plot5 = sggrouprare$plot6 = sggrouprare$plot57 = sggrouprare$plot67 = sggrouprare$plot567 = 0
sggrouprare$plot7 = 0

sgfragment$plot5 = sgfragment$plot6 = sgfragment$plot57 = sgfragment$plot67 = sgfragment$plot567 = 0
sgfragmentsel$plot5 = sgfragmentsel$plot6 = sgfragmentsel$plot57 = sgfragmentsel$plot67 = sgfragmentsel$plot567 = 0
sgfragment$plot7 = 0
sgfragmentsel$plot7 = 0

for (i in 1:length(sgfragment$site))
{
  if (sgfragment$plot[i] == "5")
  {
    sgfragment$plot5[i] = 1
    sgfragment$plot57[i] = 1
    sgfragment$plot567[i] = 1
  }
  if (sgfragment$plot[i] == "6")
  {
    sgfragment$plot6[i] = 1
    sgfragment$plot67[i] = 1
    sgfragment$plot567[i] = 1
  }
  if (sgfragment$plot[i] == "7")
  {
    sgfragment$plot7[i] = 1
    sgfragment$plot57[i] = 1
    sgfragment$plot67[i] = 1
    sgfragment$plot567[i] = 1
  }
}

for (i in 1:length(sgloc2$site))
{
  if (sgloc2$plot[i] == "7")
  {
    sgloc1$plot7[i] = 1
    sgloc2$plot7[i] = 1
  }
}

sgloc1$richp = 0
for (i in 1:length(sloc$site))
{
  sgloc1[sgloc1$site == sloc$site[i] & sgloc1$location == sloc$location[i],]$richp = sloc$rich[i]
}

sgloc256 = sgloc2[sgloc2$plot == "1" | sgloc2$plot == "2" | sgloc2$plot == "5" | sgloc2$plot == "6",]
sgfrag26 = sgfrag2[sgfrag2$plot == "1" | sgfrag2$plot == "2" | sgfrag2$plot == "6",]
sgfrag257 = sgfrag2[sgfrag2$plot == "1" | sgfrag2$plot == "2" | sgfrag2$plot == "5" | sgfrag2$plot == "7",]
sgfrag267 = sgfrag2[sgfrag2$plot == "1" | sgfrag2$plot == "2" | sgfrag2$plot == "6" | sgfrag2$plot == "7",]

sgfragment56 = sgfragment[sgfragment$plot != "7",]

sfrag$richdiv = sfrag$rich - 2
sgfrag1$richdiv = sgfrag1$rich - 2
sgfrag15$richdiv = sgfrag15$rich - 2
sgfrag156$richdiv = sgfrag156$rich - 2
sgfrag157$richdiv = sgfrag157$rich - 2
sgfrag16$richdiv = sgfrag16$rich - 2
sgfrag167$richdiv = sgfrag167$rich - 2
sgfrag2$totalp = sgfrag1$total
sgfrag25$totalp = sgfrag15$total
sgfrag256$totalp = sgfrag156$total
sgfrag257$totalp = sgfrag157$total
sgfrag26$totalp = sgfrag16$total
sgfrag267$totalp = sgfrag167$total
sggro1$richdiv = sggro1$rich - 2
sggro2$richdiv = sggro2$rich - 2
sgloc1$richdiv = sgloc1$rich - 2
sgloc15$richdiv = sgloc15$rich - 2
sgloc156$richdiv = sgloc156$rich - 2
sgloc157$richdiv = sgloc157$rich - 2
sgloc16$richdiv = sgloc16$rich - 2
sgloc167$richdiv = sgloc167$rich - 2
sgloc2$totalp = sgloc1$total
sgloc25$totalp = sgloc15$total
sgloc256$totalp = sgloc156$total
sgloc257$totalp = sgloc157$total
sgloc26$totalp = sgloc16$total
sgloc267$totalp = sgloc167$total
sgro$richdiv = sgro$rich - 2
sloc$richdiv = sloc$rich - 2

names(sloc)
head(sloc)

sloc$total = 0
for (i in 1:length(sloc$site))
{
  a = sum(sloc[i,5:96])
  sloc$total[i] = a
}

sgloc156$plot125 = 1 - sgloc156$plot6
sgloc256$plot125 = 1 - sgloc256$plot6
sgfrag156$plot125 = 1 - sgfrag156$plot6
sgfrag256$plot125 = 1 - sgfrag256$plot6